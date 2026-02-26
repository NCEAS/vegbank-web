#' Download Table Data Module
#'
#' Provides functions for downloading filtered table data as a ZIP bundle
#' via the VegBank API's ?bundle=csv endpoint.
#' @noRd

# Maximum number of records allowed for download
DOWNLOAD_MAX_RECORDS <- 20000L

#' Table Download Configuration
#'
#' Defines download specifications for each table type including API resource
#' paths and file naming conventions.
#'
#' @noRd
TABLE_DOWNLOAD_CONFIG <- list(
  plot_table = list(
    resource = "plot-observations",
    filename_prefix = "vegbank_plots"
  )
)

#' Get current table filter state
#'
#' Extracts search term and filter parameters from table inputs.
#' Note: This function uses isolate() for reactive values, making it safe to call
#' from download handlers but NOT suitable for creating reactive dependencies.
#' For observers that need to react to filter changes, read the reactives directly.
#'
#' @param table_id The DataTable ID (e.g., "plot_table")
#' @param input Shiny input object
#' @param state App state reactive values
#' @return List with search term and filter code
#' @noRd
get_table_filter_state <- function(table_id, input, state) {
  search_input_id <- paste0(table_id, "_search")
  search_term <- input[[search_input_id]]

  # Handle cross-resource filter (e.g., plot_table filtered by project)
  filter_info <- NULL
  if (table_id == "plot_table" && !is.null(state$plot_filter)) {
    filter_info <- tryCatch(
      shiny::isolate(state$plot_filter()),
      error = function(e) {
        # If state$plot_filter is a plain function (tests), call it directly
        if (is.function(state$plot_filter)) state$plot_filter() else NULL
      }
    )
    # Validate that we have both type and code
    if (is.null(filter_info) || is.null(filter_info$type) || is.null(filter_info$code)) {
      filter_info <- NULL
    }
  }

  list(
    search = search_term,
    filter = filter_info # Now returns full list(type, code, label) or NULL
  )
}

#' Build API bundle download URL
#'
#' Constructs the URL for the VegBank API's ?bundle=csv endpoint, incorporating
#' any active search term or cross-resource filter from the current table state.
#'
#' When a filter is active, the resource path is derived from the VegBank code
#' prefix of `filter$code` (via RESOURCE_REGISTRY), not from `filter$type`.
#' This ensures that every filter variant — project, party, community concept,
#' plant concept, dataset citation, single-entity citation, etc. — maps to the
#' correct API sub-resource path:
#'
#'   pj.  → /projects/{code}/plot-observations?bundle=csv
#'   py.  → /parties/{code}/plot-observations?bundle=csv
#'   cc.  → /community-concepts/{code}/plot-observations?bundle=csv
#'   pc.  → /plant-concepts/{code}/plot-observations?bundle=csv
#'   ds.  → /user-datasets/{code}/plot-observations?bundle=csv
#'   ob.  → /plot-observations/{code}?bundle=csv  (single obs citation)
#'
#' Filters whose prefix is not in RESOURCE_REGISTRY fall back to the general
#' /plot-observations endpoint with a vb_code query parameter.
#'
#' @param config Table download configuration (from TABLE_DOWNLOAD_CONFIG)
#' @param filter_state List with search and filter parameters (from get_table_filter_state)
#' @return Character URL string
#'
#' @importFrom httr modify_url
#' @noRd
build_bundle_url <- function(config, filter_state) {
  base_url <- vegbankr::vb_get_base_url()

  has_filter <- !is.null(filter_state$filter) &&
    !is.null(filter_state$filter$type) &&
    !is.null(filter_state$filter$code)

  has_search <- !is.null(filter_state$search) && nzchar(trimws(filter_state$search))

  if (has_filter) {
    filter_code <- filter_state$filter$code

    # Derive the parent resource API type from the code prefix
    prefix <- tolower(strsplit(as.character(filter_code), "\\.")[[1]][1])
    resource_info <- get_resource_by_prefix(prefix)
    parent_api_type <- if (!is.null(resource_info)) resource_info$api_type else NULL

    if (!is.null(parent_api_type) && parent_api_type != config$resource) {
      # Sub-resource path: /{parent_api_type}/{code}/plot-observations
      path <- paste0("/", parent_api_type, "/", filter_code, "/", config$resource)
      query_params <- list(bundle = "csv", limit = DOWNLOAD_MAX_RECORDS)
    } else if (!is.null(parent_api_type) && parent_api_type == config$resource) {
      # The filter code IS a plot observation (single-entity citation): /plot-observations/{code}
      path <- paste0("/", config$resource, "/", filter_code)
      query_params <- list(bundle = "csv", limit = DOWNLOAD_MAX_RECORDS)
    } else {
      # Unknown prefix: fall back to general endpoint with vb_code param
      path <- paste0("/", config$resource)
      query_params <- list(bundle = "csv", limit = DOWNLOAD_MAX_RECORDS, vb_code = filter_code)
    }
  } else {
    # No filter: general endpoint
    path <- paste0("/", config$resource)
    query_params <- list(bundle = "csv", limit = DOWNLOAD_MAX_RECORDS)
  }

  if (has_search) query_params$search <- trimws(filter_state$search)
  url <- httr::modify_url(paste0(base_url, path), query = query_params)

  url
}

#' Create download handler for table data
#'
#' Factory function that creates a Shiny downloadHandler configured for a
#' specific table type. Delegates ZIP generation to the VegBank API's
#' ?bundle=csv endpoint, which returns a comprehensive set of related CSVs.
#'
#' @param table_id The DataTable ID (e.g., "plot_table")
#' @param input Shiny input object
#' @param state App state reactive values
#' @param session Shiny session object (for sending custom messages)
#' @return A Shiny downloadHandler
#'
#' @importFrom httr GET write_disk stop_for_status
#' @noRd
create_table_download_handler <- function(table_id, input, state, session) {
  config <- TABLE_DOWNLOAD_CONFIG[[table_id]]

  if (is.null(config)) {
    stop("No download configuration found for table: ", table_id)
  }

  shiny::downloadHandler(
    filename = function() {
      now <- Sys.time()
      paste0(
        config$filename_prefix,
        "_", format(now, "%Y%m%d"),
        "_", format(now, "%H%M%S"), ".zip"
      )
    },
    content = function(file) {
      # Get current filter state
      filter_state <- get_table_filter_state(table_id, input, state)

      # Use client-provided filtered count (from DataTable AJAX response)
      # This is more efficient than fetching again and ensures consistency
      # with the count used to enable/disable the download button
      count_input_id <- paste0(table_id, "_filtered_count")
      count <- input[[count_input_id]]

      # Validate count
      if (is.null(count) || !is.numeric(count) || is.na(count)) {
        session$sendCustomMessage("hideLoadingOverlay", list(type = "download"))
        shiny::showNotification(
          "Unable to determine record count. Please try again.",
          type = "error",
          duration = NULL
        )
        return(NULL)
      }

      # Server-side validation: ensure count is within limits
      # This defends against client manipulation
      if (count > DOWNLOAD_MAX_RECORDS) {
        session$sendCustomMessage("hideLoadingOverlay", list(type = "download"))
        shiny::showNotification(
          paste0(
            "Download limit exceeded. Your filters match ", format(count, big.mark = ","), " records, ",
            "but the maximum allowed is ", format(DOWNLOAD_MAX_RECORDS, big.mark = ","), ". ",
            "Please refine your search or filters."
          ),
          type = "warning",
          duration = 10
        )
        return(NULL)
      }

      if (count == 0) {
        session$sendCustomMessage("hideLoadingOverlay", list(type = "download"))
        shiny::showNotification(
          "No records match your current filters.",
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      # Show loading overlay with count
      session$sendCustomMessage("showLoadingOverlay", list(
        type = "download",
        detail = paste0("Fetching ", format(count, big.mark = ","), " records...")
      ))

      # Build the bundle URL and stream the API response directly to the output file
      url <- build_bundle_url(config, filter_state)

      download_result <- tryCatch(
        {
          response <- httr::GET(url, httr::write_disk(file, overwrite = TRUE))
          httr::stop_for_status(response)
          list(success = TRUE)
        },
        error = function(e) {
          list(success = FALSE, error = e)
        }
      )

      session$sendCustomMessage("hideLoadingOverlay", list(type = "download"))

      if (!isTRUE(download_result$success)) {
        shiny::showNotification(
          paste("Failed to prepare download:", conditionMessage(download_result$error)),
          type = "error",
          duration = NULL
        )
        invisible(NULL)
      }
    }
  )
}
