#' Generalized Table Functions Module
#'
#' Provides common table functions that can be used across different data types.
#'
#' @importFrom DT datatable
#' @importFrom shiny withProgress incProgress showNotification
#' @importFrom htmltools HTML

#' Create a table with the given configuration
#'
#' @param data_sources List of data frames required for the table
#' @param required_sources Character vector of keys in data_sources that must be present
#' @param process_function Function to process the data into display format
#' @param table_config List with table configuration options
#' @returns A DT datatable object ready for display in a Shiny app
#' @export
create_table <- function(data_sources, required_sources, process_function, table_config = list()) {
  shiny::withProgress(
    expr = {
      if (is_any_data_missing(data_sources, required_sources)) {
        return(create_empty_table(table_config$empty_message))
      }

      shiny::incProgress(0.2, detail = "Processing data sources")

      # Process the data with the provided function
      display_data <- process_function(data_sources)

      # Default column definitions
      column_defs <- table_config$column_defs %||% list()

      DT::datatable(
        display_data,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = "single", target = "row", selectable = FALSE),
        options = list(
          dom = table_config$dom %||% "frtip",
          pageLength = table_config$page_length %||% 100,
          scrollY = table_config$scroll_y %||% "calc(100vh - 300px)",
          scrollX = table_config$scroll_x %||% TRUE,
          scrollCollapse = TRUE,
          deferRender = TRUE,
          processing = TRUE,
          columnDefs = column_defs
        )
      )
    },
    message = table_config$progress_message %||% "Processing table data",
    value = 0
  )
}

#' Check if any required data sources are missing
#'
#' @param data_sources List of data frames
#' @param required_sources Character vector of keys in data_sources that must be present
#' @param error_message Optional custom error message
#' @returns TRUE if any required data is missing, FALSE otherwise
#' @export
is_any_data_missing <- function(data_sources, required_sources, error_message = NULL) {
  is_missing <- FALSE

  for (source in required_sources) {
    if (!source %in% names(data_sources) ||
      is.null(data_sources[[source]]) ||
      nrow(data_sources[[source]]) == 0) {
      is_missing <- TRUE
      break
    }
  }

  if (is_missing) {
    shiny::showNotification(
      error_message %||% "Missing required data. Please try again or check your connection.",
      type = "error"
    )
    return(TRUE)
  }
  FALSE
}

#' Create an empty DT table for missing data
#'
#' @param message Optional custom message to display
#' @returns A DT datatable with a message indicating no data is available
#' @export
create_empty_table <- function(message = NULL) {
  DT::datatable(
    data.frame(
      "No.Data.Available" = message %||% "Please try again or check your connection",
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    options = list(dom = "t"),
    rownames = FALSE
  )
}

#' Clean a column in data, replacing NA/empty with a default value
#'
#' @param data Data frame
#' @param column_name Name of the column to clean
#' @param default_value Value to use for NA/empty values
#' @returns A character vector with cleaned data
#' @export
clean_column_data <- function(data, column_name, default_value = "Not Provided") {
  if (column_name %in% colnames(data)) {
    ifelse(is.na(data[[column_name]]) | data[[column_name]] == "",
      default_value,
      data[[column_name]]
    )
  } else {
    rep(default_value, nrow(data))
  }
}

#' Create generic action buttons for each row
#'
#' @param data Data frame
#' @param actions List of action definitions, each with id, label, and class
#' @returns A character vector of HTML strings for action buttons
#' @export
create_action_buttons <- function(data, actions) {
  vapply(seq_len(nrow(data)), function(i) {
    buttons <- vapply(actions, function(action) {
      sprintf(
        '<button class="btn btn-sm %s" onclick="Shiny.setInputValue(\'%s\', %d, {priority: \'event\'})">%s</button>',
        action$class %||% "btn-outline-primary",
        action$id,
        i,
        action$label
      )
    }, character(1))

    sprintf(
      '<div class="btn-group btn-group-sm">%s</div>',
      paste(buttons, collapse = "")
    )
  }, character(1))
}

# Utility function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
