#' Download Table Data Module
#'
#' Provides functions for downloading filtered table data as CSV or ZIP files.
#' @noRd

# Maximum number of records allowed for download
DOWNLOAD_MAX_RECORDS <- 20000L

#' Table Download Configuration
#'
#' Defines download specifications for each table type including API endpoints,
#' nested data structures, and file naming conventions.
#'
#' @noRd
TABLE_DOWNLOAD_CONFIG <- list(
  plot_table = list(
    resource = "plot-observations",
    filename_prefix = "vegbank_plots",
    has_nested = TRUE,
    nested_fields = c("top_taxon_observations", "top_classifications", "disturbances", "soils"),
    nested_labels = c("taxa", "communities", "disturbances", "soils"),
    api_params = list(
      with_nested = "TRUE",
      detail = "full",
      max_taxa = 100000L # Get all taxa, not just top 5
    ),
    primary_key = "ob_code"
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
    filter = filter_info  # Now returns full list(type, code, label) or NULL
  )
}

#' Check if table has active filters
#'
#' @param table_id The DataTable ID
#' @param input Shiny input object
#' @param state App state reactive values
#' @return TRUE if table has search or filter applied
#' @noRd
has_table_filters <- function(table_id, input, state) {
  filter_state <- get_table_filter_state(table_id, input, state)

  has_search <- !is.null(filter_state$search) && nzchar(trimws(filter_state$search))
  has_filter <- !is.null(filter_state$filter) && 
                !is.null(filter_state$filter$type) && 
                !is.null(filter_state$filter$code)

  has_search || has_filter
}

#' Fetch filtered record count from API
#'
#' @param config Table download configuration
#' @param filter_state List with search and filter parameters
#' @return Integer count or NA on error
#' @noRd
fetch_filtered_count <- function(config, filter_state) {
  query_params <- list()

  if (!is.null(filter_state$search) && nzchar(trimws(filter_state$search))) {
    query_params$search <- trimws(filter_state$search)
  }

  # Use vb_code parameter for cross-resource filtering (vegbankr infers type from code prefix)
  if (!is.null(filter_state$filter) && 
      !is.null(filter_state$filter$type) && 
      !is.null(filter_state$filter$code)) {
    query_params$vb_code <- filter_state$filter$code
  }

  # Debug output
  message("DEBUG fetch_filtered_count:")
  message("  filter_state$filter: ", paste(capture.output(str(filter_state$filter)), collapse = "\n"))
  message("  query_params: ", paste(names(query_params), "=", query_params, collapse = ", "))

  tryCatch(
    {
      args <- c(
        list(resource = config$resource),
        query_params
      )
      message("  Calling vb_count with: resource='", config$resource, "', ", 
              paste(names(query_params), "='", query_params, "'", sep = "", collapse = ", "))
      count <- do.call(vegbankr::vb_count, args)
      message("  Count result: ", count)
      as.integer(count)
    },
    error = function(e) {
      warning("Failed to fetch count: ", e$message)
      NA_integer_
    }
  )
}

#' Fetch filtered data from API
#'
#' @param config Table download configuration
#' @param filter_state List with search and filter parameters
#' @return Data frame with results or NULL on error
#' @noRd
fetch_filtered_data <- function(config, filter_state) {
  query_params <- config$api_params

  if (!is.null(filter_state$search) && nzchar(trimws(filter_state$search))) {
    query_params$search <- trimws(filter_state$search)
  }

  # Use vb_code parameter for cross-resource filtering (vegbankr infers type from code prefix)
  if (!is.null(filter_state$filter) && 
      !is.null(filter_state$filter$type) && 
      !is.null(filter_state$filter$code)) {
    query_params$vb_code <- filter_state$filter$code
  }

  tryCatch(
    {
      args <- c(
        list(
          resource = config$resource,
          limit = NULL # Fetch all matching records
        ),
        query_params
      )
      do.call(vegbankr::vb_get, args)
    },
    error = function(e) {
      warning("Failed to fetch data: ", e$message)
      NULL
    }
  )
}

#' Extract nested dataframe into separate table with foreign key
#'
#' @param data Main data frame containing nested columns
#' @param nested_col Name of the nested column
#' @param primary_key Name of the primary key column (e.g., "ob_code")
#' @return Data frame with foreign key added, or empty data frame
#' @noRd
extract_nested_table <- function(data, nested_col, primary_key) {
  if (!nested_col %in% names(data)) {
    return(data.frame())
  }

  nested_list <- data[[nested_col]]
  pk_values <- data[[primary_key]]

  if (!is.list(nested_list) || length(nested_list) == 0) {
    return(data.frame())
  }

  # Extract each nested dataframe and add foreign key
  extracted_rows <- list()

  for (i in seq_along(nested_list)) {
    nested_df <- nested_list[[i]]
    pk_value <- pk_values[[i]]

    if (is.null(nested_df) || !is.data.frame(nested_df) || nrow(nested_df) == 0) {
      next
    }

    # Add foreign key column
    nested_df[[primary_key]] <- pk_value
    extracted_rows[[length(extracted_rows) + 1]] <- nested_df
  }

  if (length(extracted_rows) == 0) {
    return(data.frame())
  }

  # Combine all rows
  tryCatch(
    {
      do.call(rbind, extracted_rows)
    },
    error = function(e) {
      warning("Error combining nested rows: ", e$message)
      data.frame()
    }
  )
}

#' Prepare data for CSV export
#'
#' Splits nested dataframes into separate tables and removes nested columns
#' from main table.
#'
#' @param data Raw data from API with nested columns
#' @param config Table download configuration
#' @return Named list of data frames ready for CSV export
#' @noRd
prepare_csv_tables <- function(data, config) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  # Debug: Check for list columns
  list_cols <- sapply(data, is.list)
  message("DEBUG prepare_csv_tables:")
  message("  Total columns: ", ncol(data))
  message("  List columns found: ", paste(names(data)[list_cols], collapse = ", "))
  message("  Configured nested_fields: ", paste(config$nested_fields, collapse = ", "))

  result <- list()

  # Extract nested tables if configured
  if (config$has_nested && !is.null(config$nested_fields)) {
    for (i in seq_along(config$nested_fields)) {
      field_name <- config$nested_fields[[i]]
      label <- config$nested_labels[[i]]

      nested_df <- extract_nested_table(data, field_name, config$primary_key)

      if (nrow(nested_df) > 0) {
        # Move foreign key to first column
        if (config$primary_key %in% names(nested_df)) {
          col_order <- c(config$primary_key, setdiff(names(nested_df), config$primary_key))
          nested_df <- nested_df[, col_order, drop = FALSE]
        }
        result[[label]] <- nested_df
      }
    }
  }

  # Create main table with ALL list columns removed (not just configured nested fields)
  main_df <- data
  
  # Remove all list columns to ensure CSV compatibility
  list_cols <- sapply(main_df, is.list)
  if (any(list_cols)) {
    cols_to_remove <- names(main_df)[list_cols]
    message("  Removing all list columns from main table: ", paste(cols_to_remove, collapse = ", "))
    main_df <- main_df[, !list_cols, drop = FALSE]
  }

  # Add main table first
  result <- c(list(main = main_df), result)

  result
}

#' Create README file explaining the download structure
#'
#' @param config Table download configuration
#' @param filter_state Filter state used for download
#' @param record_count Number of records downloaded
#' @return Character string with README content
#' @noRd
create_download_readme <- function(config, filter_state, record_count) {
  has_search <- !is.null(filter_state$search) && nzchar(trimws(filter_state$search))
  has_filter <- !is.null(filter_state$filter) && 
                !is.null(filter_state$filter$type) && 
                !is.null(filter_state$filter$code)

  paste0(
    "VegBank Data Download\n",
    "=====================\n\n",
    "Downloaded: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
    "Source: https://vegbank.org\n",
    "Records: ", record_count, "\n\n",
    if (has_search) paste0("Search Filter: ", filter_state$search, "\n") else "",
    if (has_filter) paste0("Resource Filter: ", filter_state$filter$type, " = ", filter_state$filter$code, 
                           if (!is.null(filter_state$filter$label)) paste0(" (", filter_state$filter$label, ")") else "", 
                           "\n") else "",
    "\n",
    "File Structure\n",
    "--------------\n\n",
    "This download contains multiple related CSV files:\n\n",
    "- main.csv: Primary ", config$resource, " data\n",
    if (config$has_nested) {
      paste0(
        paste0("- ", config$nested_labels, ".csv: ",
          tools::toTitleCase(gsub("_", " ", config$nested_labels)),
          " (linked by ", config$primary_key, ")\n",
          collapse = ""
        ),
        "\n"
      )
    } else {
      ""
    },
    "The '", config$primary_key, "' column serves as the primary key in main.csv\n",
    "and as a foreign key in the related tables, allowing you to join the data.\n\n",
    "Example (R):\n",
    "  main <- read.csv('main.csv')\n",
    if (config$has_nested && length(config$nested_labels) > 0) {
      paste0(
        "  ", config$nested_labels[[1]], " <- read.csv('", config$nested_labels[[1]], ".csv')\n",
        "  merged <- merge(main, ", config$nested_labels[[1]], ", by = '", config$primary_key, "')\n\n"
      )
    } else {
      ""
    },
    "Citation\n",
    "--------\n",
    "Peet, R.K., M.T. Lee, M.D. Jennings, D. Faber-Langendoen (eds). 2013.\n",
    "VegBank: The vegetation plot archive of the Ecological Society of America.\n",
    "http://vegbank.org\n"
  )
}

#' Create ZIP file with CSV tables
#'
#' @param csv_tables Named list of data frames
#' @param config Table download configuration
#' @param filter_state Filter state for README
#' @return Path to temporary ZIP file
#' @importFrom utils write.csv
#' @noRd
create_download_zip <- function(csv_tables, config, filter_state) {
  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Write each CSV
  csv_files <- character()
  for (name in names(csv_tables)) {
    df <- csv_tables[[name]]
    if (nrow(df) > 0) {
      filepath <- file.path(temp_dir, paste0(name, ".csv"))
      write.csv(df, filepath, row.names = FALSE)
      csv_files <- c(csv_files, filepath)
    }
  }

  # Write README
  readme_path <- file.path(temp_dir, "README.txt")
  record_count <- if (!is.null(csv_tables$main)) nrow(csv_tables$main) else 0
  readme_content <- create_download_readme(config, filter_state, record_count)
  writeLines(readme_content, readme_path)
  csv_files <- c(csv_files, readme_path)

  # Create ZIP
  zip_path <- tempfile(fileext = ".zip")

  # Use zip::zip if available, otherwise fall back to utils::zip
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(
      zipfile = zip_path,
      files = basename(csv_files),
      root = temp_dir,
      mode = "cherry-pick"
    )
  } else {
    # Fall back to utils::zip
    current_wd <- getwd()
    setwd(temp_dir)
    on.exit(setwd(current_wd), add = TRUE)

    utils::zip(
      zipfile = zip_path,
      files = basename(csv_files),
      flags = "-q"
    )
  }

  zip_path
}

#' Create download handler for table data
#'
#' Factory function that creates a Shiny downloadHandler configured for
#' a specific table type.
#'
#' @param table_id The DataTable ID (e.g., "plot_table")
#' @param input Shiny input object
#' @param state App state reactive values
#' @return A Shiny downloadHandler
#' @noRd
create_table_download_handler <- function(table_id, input, state) {
  config <- TABLE_DOWNLOAD_CONFIG[[table_id]]

  if (is.null(config)) {
    stop("No download configuration found for table: ", table_id)
  }

  shiny::downloadHandler(
    filename = function() {
      paste0(config$filename_prefix, "_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      # Get current filter state
      filter_state <- get_table_filter_state(table_id, input, state)

      # Check count first (DOES VB SUPPORT COUNT WITH CROSS-RESOURCE FILTERS?)
      # count <- fetch_filtered_count(config, filter_state)

      # if (is.na(count)) {
      #   shiny::showNotification(
      #     "Unable to determine record count. Please try again.",
      #     type = "error",
      #     duration = 5
      #   )
      #   return(NULL)
      # }

      # if (count > DOWNLOAD_MAX_RECORDS) {
      #   shiny::showNotification(
      #     paste0(
      #       "Download limit exceeded. Your filters match ", count, " records, ",
      #       "but the maximum allowed is ", DOWNLOAD_MAX_RECORDS, ". ",
      #       "Please refine your search or filters."
      #     ),
      #     type = "warning",
      #     duration = 10
      #   )
      #   return(NULL)
      # }

      # if (count == 0) {
      #   shiny::showNotification(
      #     "No records match your current filters.",
      #     type = "warning",
      #     duration = 5
      #   )
      #   return(NULL)
      # }

      # Show progress
      progress <- shiny::Progress$new()
      progress$set(message = "Preparing download", value = 0)
      on.exit(progress$close(), add = TRUE)

      progress$set(detail = paste0("Fetching records..."), value = 0.2)

      # Fetch data
      data <- fetch_filtered_data(config, filter_state)

      if (is.null(data) || nrow(data) == 0) {
        shiny::showNotification(
          "Failed to fetch data. Please try again.",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      progress$set(detail = "Processing nested data...", value = 0.5)

      # Prepare CSV tables
      csv_tables <- prepare_csv_tables(data, config)

      if (length(csv_tables) == 0) {
        shiny::showNotification(
          "No data to export.",
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      progress$set(detail = "Creating ZIP file...", value = 0.8)

      # Create ZIP
      zip_path <- create_download_zip(csv_tables, config, filter_state)

      progress$set(detail = "Download ready!", value = 1.0)

      # Copy to output file
      file.copy(zip_path, file, overwrite = TRUE)

      shiny::showNotification(
        paste0("Downloaded records with related data."),
        type = "message",
        duration = 5
      )
    }
  )
}
