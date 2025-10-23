# Helper functions extracted from detail_view.R

#' Coalesce Function for NULL or NA
#' Returns the first argument if it is not NULL or NA; otherwise returns the second argument.
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
  if (is.na(a) || a == "" || identical(a, "NA")) {
    return(b)
  }
  a
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

  tryCatch({
    parsed_date <- as.POSIXct(date_string, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
    if (!is.na(parsed_date)) {
      return(as.Date(parsed_date))
    }
    as.Date(date_string)
  }, error = function(e) {
    NA
  })
}

#' Read Display Names from Lookup Table
#'
#' Reads display names from the display_name_lookup.txt file.
#'
#' @return A named vector where names are snake_case field names and values are display names
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

#' Safely Render Detail Fields in a Shiny UI
#'
#' This function creates a Shiny renderUI output that displays data fields in a formatted table.
#'
#' @param fields A character vector of field names to display from the dataframe
#' @param dataframe A dataframe containing the data to display
#' @noRd
safe_render_details <- function(fields, dataframe) {
  display_names <- get_field_display_names()
  shiny::renderUI({
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
  })
}

#' Create an HTML Table for Detail View
#' @noRd
create_detail_table <- function(details, col_names) {
  htmltools::tags$table(
    class = "table table-sm table-striped table-hover",
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
