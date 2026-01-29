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
    api_params = list(
      with_nested = "TRUE",
      detail = "full",
      num_taxa = 1000000L, # Get all taxa, not just top 5
      num_comms = 1000000L # Get all communities, not just top 5
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
    filter = filter_info # Now returns full list(type, code, label) or NULL
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
          limit = 1000000 # Fetch all matching records
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

#' Auto-detect nested data frame columns
#'
#' Identifies columns that contain lists of data frames.
#'
#' @param data Data frame to check
#' @return Character vector of nested column names
#' @noRd
detect_nested_columns <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }

  nested_cols <- names(data)[vapply(data, function(col) {
    # Check if it's a list and contains at least one data frame
    if (!is.list(col) || length(col) == 0) {
      return(FALSE)
    }
    # Check first non-NULL element
    first_non_null <- Find(Negate(is.null), col)
    !is.null(first_non_null) && is.data.frame(first_non_null)
  }, logical(1))]

  nested_cols
}

#' Extract nested dataframe into separate table with foreign key
#'
#' Uses tidyr::unnest() for robust extraction of nested data frames.
#'
#' @param data Main data frame containing nested columns
#' @param nested_col Name of the nested column
#' @param primary_key Name of the primary key column (e.g., "ob_code")
#' @return Data frame with foreign key added, or empty data frame
#'
#' @importFrom tidyr unnest all_of
#' @noRd
extract_nested_table_with_fk <- function(data, nested_col, primary_key) {
  if (!nested_col %in% names(data)) {
    return(data.frame())
  }

  tryCatch(
    {
      # Select only the primary key and nested column
      subset_data <- data[, c(primary_key, nested_col), drop = FALSE]

      # Unnest using tidyr - handles empty/NULL values gracefully
      unnested <- tidyr::unnest(
        subset_data,
        cols = tidyr::all_of(nested_col),
        keep_empty = FALSE # Drop rows where nested column is empty
      )

      # Return empty data frame if no rows resulted
      if (nrow(unnested) == 0) {
        return(data.frame())
      }

      # Reorder to put foreign key first
      if (primary_key %in% names(unnested)) {
        col_order <- c(primary_key, setdiff(names(unnested), primary_key))
        unnested <- unnested[, col_order, drop = FALSE]
      }

      return(as.data.frame(unnested))
    },
    error = function(e) {
      warning("Error extracting nested column '", nested_col, "': ", e$message)
      data.frame()
    }
  )
}

#' Prepare data for CSV export
#'
#' Auto-detects and splits nested dataframes into separate tables,
#' removing all list columns from main table.
#'
#' @param data Raw data from API with nested columns
#' @param config Table download configuration
#' @return Named list of data frames ready for CSV export
#' @noRd
prepare_csv_tables <- function(data, config) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  result <- list()

  # Auto-detect nested columns if table has nested data
  if (config$has_nested) {
    nested_cols <- detect_nested_columns(data)

    message("DEBUG prepare_csv_tables:")
    message("  Total columns: ", ncol(data))
    message("  Auto-detected nested columns: ", paste(nested_cols, collapse = ", "))

    # Extract each nested column into its own table
    for (nested_col in nested_cols) {
      nested_df <- extract_nested_table_with_fk(data, nested_col, config$primary_key)

      if (nrow(nested_df) > 0) {
        # Map API column names to user-friendly CSV file names
        table_name <- switch(nested_col,
          "top_taxon_observations" = "taxon_observations",
          "top_classifications" = "community_classifications",
          nested_col # fallback to original name
        )
        result[[table_name]] <- nested_df
      }
    }
  }

  # Create main table with ALL list columns removed
  main_df <- data
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
#' @param csv_tables Named list of data frames representing the CSV tables
#' @return Character string with README content
#' @noRd
create_download_readme <- function(config, filter_state, record_count, csv_tables = NULL) {
  has_search <- !is.null(filter_state$search) && nzchar(trimws(filter_state$search))
  has_filter <- !is.null(filter_state$filter) &&
    !is.null(filter_state$filter$type) &&
    !is.null(filter_state$filter$code)

  # Get list of nested tables (exclude 'main')
  nested_tables <- if (!is.null(csv_tables)) {
    setdiff(names(csv_tables), "main")
  } else {
    character(0)
  }

  paste0(
    "VegBank Data Download\n",
    "=====================\n\n",
    "Downloaded: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
    "Source: https://vegbank.org\n",
    "Records: ", record_count, "\n\n",
    if (has_search) paste0("Search Filter: ", filter_state$search, "\n") else "",
    if (has_filter) {
      paste0(
        "Resource Filter: ", filter_state$filter$type, " = ", filter_state$filter$code,
        if (!is.null(filter_state$filter$label)) {
          paste0(" (", filter_state$filter$label, ")")
        } else {
          ""
        },
        "\n"
      )
    } else {
      ""
    },
    "\n",
    "File Structure\n",
    "--------------\n\n",
    "This download contains multiple related CSV files:\n\n",
    "- main.csv: Primary ", config$resource, " data\n",
    if (length(nested_tables) > 0) {
      paste0(
        paste0("- ", nested_tables, ".csv: ",
          tools::toTitleCase(gsub("_", " ", nested_tables)),
          " (linked by ", config$primary_key, ")\n",
          collapse = ""
        ),
        "\n"
      )
    } else {
      ""
    },
    "The '", config$primary_key, "' column serves as the primary key in main.csv\n",
    if (length(nested_tables) > 0) {
      paste0(
        "and as a foreign key in the related tables, allowing you to join the data.\n\n",
        "Example (R):\n",
        "  main <- read.csv('main.csv')\n",
        paste0(
          "  ", nested_tables[[1]], " <- read.csv('", nested_tables[[1]], ".csv')\n",
          "  merged <- merge(main, ", nested_tables[[1]], ", by = '", config$primary_key, "')\n\n"
        )
      )
    } else {
      "\n"
    },
    "Citation\n",
    "--------\n",
    "Peet, R.K., M.T. Lee, M.D. Jennings, D. Faber-Langendoen (eds). 2013.\n",
    "VegBank: The vegetation plot archive of the Ecological Society of America.\n",
    "http://vegbank.org, searched on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n"
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
  readme_content <- create_download_readme(config, filter_state, record_count, csv_tables)
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
#' @param session Shiny session object (for sending custom messages)
#' @return A Shiny downloadHandler
#' @noRd
create_table_download_handler <- function(table_id, input, state, session) {
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
          duration = 5
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

      # Fetch data
      data <- fetch_filtered_data(config, filter_state)

      if (is.null(data) || nrow(data) == 0) {
        session$sendCustomMessage("hideLoadingOverlay", list(type = "download"))
        shiny::showNotification(
          "Failed to fetch data. Please try again.",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      session$sendCustomMessage("updateLoadingOverlay", list(
        type = "download",
        detail = "Untangling nested data..."
      ))

      # Prepare CSV tables
      csv_tables <- prepare_csv_tables(data, config)

      if (length(csv_tables) == 0) {
        session$sendCustomMessage("hideLoadingOverlay", list(type = "download"))
        shiny::showNotification(
          "No data to export.",
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      session$sendCustomMessage("updateLoadingOverlay", list(
        type = "download",
        detail = "Zipping up your backpack..."
      ))

      # Create ZIP
      zip_path <- create_download_zip(csv_tables, config, filter_state)

      # Copy to output file
      file.copy(zip_path, file, overwrite = TRUE)

      # Hide overlay with success message
      session$sendCustomMessage("hideLoadingOverlay", list(
        type = "download",
        message = paste0(
          "Downloaded ", format(count, big.mark = ","), " records with related data."
        )
      ))
    }
  )
}
