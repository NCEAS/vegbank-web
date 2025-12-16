# Helper functions extracted from detail_view.R

#' Coalesce Function for empty string, NULL, or NA
#' Returns the first argument if it is not an empty string, NULL, or NA otherwise returns the second argument.
#' @param a First value to check
#' @param b Second value to return if the first is NULL or NA
#' @return The first value if valid, otherwise the second value
#' @noRd
`%|||%` <- function(a, b) {
  if (is.null(a)) {
    return(b)
  }
  if (length(a) > 1) {
    a <- a[1]
  }
  if (is.na(a) || trimws(as.character(a)) == "") {
    return(b)
  }
  a
}

# ---- XSS Prevention Helpers ----

#' Escape a string for safe inclusion in HTML content
#'
#' Converts HTML special characters to their entity equivalents to prevent XSS.
#'
#' @param text Character string to escape
#' @return HTML-escaped string safe for inclusion in HTML content
#' @noRd
escape_html <- function(text) {
  if (is.null(text) || is.na(text)) {
    return("")
  }
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  text
}

#' Escape a string for safe inclusion in a JavaScript string literal
#'
#' Escapes characters that could break out of a JS string context.
#'
#' @param text Character string to escape
#' @return String safe for inclusion inside JS single-quoted string literals
#' @noRd
escape_js_string <- function(text) {
  if (is.null(text) || is.na(text)) {
    return("")
  }
  # Escape backslashes first, then other special chars
  text <- gsub("\\", "\\\\", text, fixed = TRUE)
  text <- gsub("'", "\\'", text, fixed = TRUE)
  text <- gsub('"', '\\"', text, fixed = TRUE)
  text <- gsub("\n", "\\n", text, fixed = TRUE)
  text <- gsub("\r", "\\r", text, fixed = TRUE)
  # Prevent </script> injection
  text <- gsub("/", "\\/", text, fixed = TRUE)
  text
}

#' Safe Date Parser for GMT Format Strings
#' Parses date strings that may be in GMT format or other standard formats.
#' @param date_string The date string to parse
#' @return A Date object or NA if parsing fails
#' @noRd
safe_parse_date <- function(date_string) {
  if (is.null(date_string) || length(date_string) == 0 || is.na(date_string) || date_string == "") {
    return(NA)
  }

  tryCatch(
    {
      parsed_date <- as.POSIXct(date_string, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
      if (!is.na(parsed_date)) {
        return(as.Date(parsed_date))
      }
      as.Date(date_string)
    },
    error = function(e) {
      NA
    }
  )
}

#' Format a date string for detail displays
#'
#' Wraps `safe_parse_date()` and returns a formatted string or a default placeholder
#' when parsing fails.
#'
#' @param date_string Raw date value pulled from VegBank results
#' @param default_value Placeholder to use when the date cannot be parsed
#' @param format_string Format passed to `format()` for parsed dates
#' @return A character string suitable for UI display
#' @noRd
format_date <- function(date_string, default_value = "Not provided", format_string = "%Y-%m-%d") {
  parsed <- safe_parse_date(date_string)
  if (is.na(parsed)) {
    return(default_value)
  }

  format(parsed, format_string)
}

#' Read Display Names from Lookup Table
#'
#' Reads display names from the display_name_lookup.txt file.
#'
#' @return A named vector where names are snake_case field names and values are display names
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @noRd
get_field_display_names <- function() {
  file_path <- system.file("shiny/www/display_name_lookup.txt", package = "vegbankweb")
  if (file.exists(file_path)) {
    lookup <- utils::read.csv(file_path, stringsAsFactors = FALSE, comment.char = "/")
    stats::setNames(lookup$display, lookup$snake)
  } else {
    warning("Display name lookup file not found: ", file_path)
    character()
  }
}

#' Format Dataframe Fields as Detail Table HTML
#'
#' Extracts and formats field values from a dataframe for display in a detail table.
#' This is a lower-level helper that returns the HTML table directly without renderUI wrapping.
#'
#' @param fields A character vector of field names to display from the dataframe
#' @param dataframe A dataframe containing the data to display
#' @return An HTML table element or a paragraph tag if no data is available
#' @noRd
format_fields_for_detail_table <- function(fields, dataframe) {
  display_names <- get_field_display_names()
  valid_fields <- fields[fields %in% colnames(dataframe)]

  if (length(valid_fields) == 0) {
    return(htmltools::tags$p("No data available for this section"))
  }

  values <- lapply(dataframe[valid_fields], function(x) {
    if (is.null(x) || all(is.na(x))) "Not recorded" else x
  })

  values <- lapply(values, function(x) {
    if (is.logical(x)) ifelse(x, "Yes", "No") else x
  })

  create_detail_table(values, col_names = display_names)
}

#' Render Detail Fields Table in Shiny UI
#'
#' Creates a Shiny renderUI output that displays dataframe fields in a formatted table.
#' Wraps format_fields_for_detail_table with renderUI for use in Shiny server logic.
#'
#' @param fields A character vector of field names to display from the dataframe
#' @param dataframe A dataframe containing the data to display
#' @return A shiny.render.function that renders the detail table
#' @noRd
render_detail_table <- function(fields, dataframe) {
  shiny::renderUI({
    format_fields_for_detail_table(fields, dataframe)
  })
}

#' Create Detail Table HTML Element
#'
#' Constructs an HTML table element from a named list of values with display names.
#'
#' @param details A named list where names are field names and values are formatted display values
#' @param col_names A named vector mapping field names to human-readable display names
#' @return An htmltools table tag
#' @noRd
create_detail_table <- function(details, col_names) {
  htmltools::tags$table(
    class = "table table-sm table-striped table-hover",
    style = "width: 100%; table-layout: fixed; word-break: break-word; white-space: normal;",
    htmltools::tags$tbody(
      lapply(names(details), function(name) {
        display_name <- if (name %in% names(col_names)) col_names[[name]] else name
        htmltools::tags$tr(
          htmltools::tags$td(display_name),
          htmltools::tags$td(class = "text-end", details[[name]])
        )
      })
    )
  )
}

#' Create Detail Table with Custom Headers
#'
#' Creates an HTML table with custom column headers and data rows.
#' Useful for tables that don't follow the standard two-column label-value format.
#'
#' @param headers Character vector of header labels for table columns
#' @param rows List of htmltools tr tags representing the table rows
#' @param table_class CSS class(es) to apply to the table
#' @param table_style CSS style string to apply to the table
#' @param header_styles Character vector of CSS style strings to apply to each header,
#'   or NULL for no custom styles. Length should match headers if provided.
#' @return An htmltools table tag with thead and tbody
#' @noRd
create_detail_table_with_headers <- function(headers, rows,
                                             table_class = "table table-sm table-striped table-hover",
                                             table_style = NULL,
                                             header_styles = NULL) {
  # Validate header_styles length if provided
  if (!is.null(header_styles) && length(header_styles) != length(headers)) {
    warning(
      "header_styles length (", length(header_styles),
      ") does not match headers length (", length(headers), ")"
    )
  }

  # Build header cells with optional styles
  header_cells <- if (is.null(header_styles)) {
    lapply(headers, htmltools::tags$th)
  } else {
    mapply(function(header, style) {
      if (is.null(style) || style == "") {
        htmltools::tags$th(header)
      } else {
        htmltools::tags$th(style = style, header)
      }
    }, headers, header_styles, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }

  htmltools::tags$table(
    class = table_class,
    style = table_style,
    htmltools::tags$thead(
      htmltools::tags$tr(header_cells)
    ),
    htmltools::tags$tbody(rows)
  )
}

#' Create Empty Detail View Outputs
#'
#' Generates a named list of renderUI outputs with a standard "not available" message.
#' Used when data is NULL or empty to provide consistent empty state handling across
#' all detail views.
#'
#' @param output_names Character vector of output names to create
#' @param entity_type String describing the entity type (e.g., "Reference", "Party", "Plot")
#' @return Named list of shiny.render.function objects showing "not available" message
#' @noRd
create_empty_detail_view <- function(output_names, entity_type = "Details") {
  empty_ui <- shiny::renderUI({
    htmltools::tags$p(paste0(entity_type, " not available"))
  })

  stats::setNames(
    rep(list(empty_ui), length(output_names)),
    output_names
  )
}

#' Create Section Header with Border
#'
#' Creates a styled section header div with consistent formatting across all detail views.
#' The header has bold text and a bottom border in the app's color scheme.
#'
#' @param title The text to display in the header
#' @param margin_bottom Optional bottom margin CSS value (default: "8px")
#' @return An htmltools div tag with styled header
#' @noRd
create_section_header <- function(title, margin_bottom = "8px") {
  htmltools::tags$div(
    title,
    style = sprintf(
      "font-weight: bold; width: 100%%; border-bottom: 1px solid #2c5443; margin-bottom: %s;",
      margin_bottom
    )
  )
}

#' Create Clickable Detail Link
#'
#' Creates a clickable link that triggers a Shiny input event for navigation between
#' detail views. The link prevents default anchor behavior and sets a Shiny input value
#' with priority 'event' to ensure proper reactivity.
#'
#' Values are escaped to prevent XSS attacks from malicious data in VegBank records.
#'
#' @param input_id The Shiny input ID to trigger (e.g., "ref_link_click")
#' @param code_value The value to send to the input (e.g., rf_code)
#' @param display_text The text to display in the link
#' @return An htmltools anchor tag with onclick handler
#' @noRd
create_detail_link <- function(input_id, code_value, display_text) {
  # Escape for XSS prevention
  safe_code <- escape_js_string(as.character(code_value))
  safe_display <- escape_html(as.character(display_text))

  htmltools::tags$a(
    href = "#",
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
      input_id,
      safe_code
    ),
    htmltools::HTML(safe_display)
  )
}

#' Check if Field Has Valid Value
#'
#' Determines whether a field in a dataframe or list contains a valid, non-empty value.
#' Checks for NULL, NA, zero-length, and empty string values.
#'
#' @param data A dataframe or named list containing the field
#' @param field The field name to check
#' @return Logical indicating whether the field has a valid value
#' @noRd
has_valid_field_value <- function(data, field) {
  if (!field %in% names(data)) {
    return(FALSE)
  }

  value <- data[[field]]
  if (is.null(value) || length(value) == 0) {
    return(FALSE)
  }
  if (all(is.na(value))) {
    return(FALSE)
  }
  !all(trimws(as.character(value)) == "")
}

#' Format Date Range for Display
#'
#' Formats start and stop dates into a human-readable range string.
#' Handles cases where only one date is present or both are missing.
#'
#' @param start_date Start date (as string or Date object)
#' @param stop_date Stop date (as string or Date object)
#' @param format_string Date format string for output (default: "%Y-%m-%d")
#' @return Formatted date range string (e.g., "From 2020-01-01 to 2023-12-31")
#' @noRd
format_date_range <- function(start_date, stop_date, format_string = "%Y-%m-%d") {
  start_parsed <- safe_parse_date(start_date)
  stop_parsed <- safe_parse_date(stop_date)

  if (!is.na(start_parsed) && !is.na(stop_parsed)) {
    paste0("From ", format(start_parsed, format_string), " to ", format(stop_parsed, format_string))
  } else if (!is.na(start_parsed)) {
    paste0("From ", format(start_parsed, format_string))
  } else if (!is.na(stop_parsed)) {
    paste0("Until ", format(stop_parsed, format_string))
  } else {
    "Date not recorded"
  }
}

#' Format Boolean Value for Display
#'
#' Formats a boolean or logical value into a human-readable string.
#' Handles NULL, NA, and non-logical values gracefully.
#'
#' @param val A boolean, logical, or other value to format
#' @return A human-readable string representation of the boolean value
#' @noRd
format_boolean <- function(val) {
  if (is.null(val) || is.na(val)) {
    return("Not recorded")
  }
  if (is.logical(val)) {
    return(if (val) "Yes" else "No")
  }
  as.character(val)
}

#' Bind Nested Rows into a Single Data Frame
#'
#' Attempts to combine nested list or data frame elements into a single data frame.
#' Handles various input formats gracefully and returns an empty data frame on failure.
#'
#' @param nested A nested structure that may be NULL, a data frame, or a list of
#'   data frames to combine.
#' @return A data frame containing the combined rows, or an empty data frame if
#'   the input cannot be processed.
#' @noRd
bind_nested_rows <- function(nested) {
  if (is.null(nested)) {
    return(data.frame())
  }

  if (is.data.frame(nested)) {
    return(nested)
  }

  if (is.list(nested) && length(nested) > 0) {
    return(tryCatch(dplyr::bind_rows(nested), error = function(e) data.frame()))
  }

  data.frame()
}

#' Extract Nested Table from Data Frame Column
#'
#' Extracts a nested table (stored as a list-column) from a single-row data frame
#' and returns it as a standard data frame. Handles various nesting formats and
#' returns an empty data frame if the column is missing or invalid.
#'
#' @param row_df A single-row data frame containing the nested column.
#' @param column_name Character string naming the column to extract.
#' @return A data frame containing the extracted nested table, or an empty data
#'   frame if extraction fails.
#' @noRd
extract_nested_table <- function(row_df, column_name) {
  if (is.null(row_df) || nrow(row_df) == 0 || !column_name %in% names(row_df)) {
    return(data.frame())
  }

  column <- row_df[[column_name]]

  if (is.null(column)) {
    return(data.frame())
  }

  if (is.data.frame(column)) {
    return(column)
  }

  if (!is.list(column) || length(column) == 0) {
    return(data.frame())
  }

  bind_nested_rows(column[[1]])
}
