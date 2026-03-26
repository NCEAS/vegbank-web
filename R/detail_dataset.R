#' Build Dataset Details View
#'
#' Constructs the complete detail view for a user dataset, including a header card,
#' a details card (accession code, author, date range, plot count, description),
#' and a formatted citation card.
#'
#' @param result A dataframe containing dataset data from vegbankr::vb_get_user_datasets()
#' @return A named list with three shiny.render.function elements:
#'   dataset_header, dataset_details, dataset_citation
#' @noRd
build_dataset_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("dataset_header", "dataset_details", "dataset_citation"),
      "Dataset details"
    ))
  }

  ds <- result[1, , drop = FALSE]

  start_year <- format_date(ds$start, format_string = "%Y")

  date_range_display <- format_date_range(ds$start, ds$stop)
  author_name        <- parse_dataset_author_label(ds$owner_label %|||% "")

  list(
    dataset_header = shiny::renderUI({
      header_rows <- list(
        htmltools::tags$h5(
          ds$name %|||% "Unnamed Dataset",
          style = "font-weight: 600; margin-bottom: 0px;"
        ),
        htmltools::tags$h5(
          ds$ds_code,
          style = "color: var(--vb-green); font-weight: 600; margin-bottom: 0;"
        )
      )

      header_rows <- add_permalink_button_to_last_row(
        header_rows,
        ds$ds_code
      )

      htmltools::div(htmltools::tagList(header_rows))
    }),

    dataset_details = shiny::renderUI({
      htmltools::tags$div(
        htmltools::tags$table(
          class = "table table-sm table-striped table-hover",
          htmltools::tags$tbody(
            htmltools::tags$tr(
              htmltools::tags$td("Accession Code"),
              htmltools::tags$td(
                class = "text-end",
                ds$accession_code %|||% "Unspecified"
              )
            ),
            htmltools::tags$tr(
              htmltools::tags$td("Author"),
              htmltools::tags$td(
                class = "text-end",
                if (has_valid_field_value(ds, "py_code")) {
                  create_detail_link("party_link_click", ds$py_code, ds$owner_label %|||% "Unknown")
                } else {
                  ds$owner_label %|||% "Unknown"
                }
              )
            ),
            htmltools::tags$tr(
              htmltools::tags$td("Date Range"),
              htmltools::tags$td(
                class = "text-end",
                date_range_display
              )
            ),
            htmltools::tags$tr(
              htmltools::tags$td("Plots"),
              htmltools::tags$td(
                class = "text-end",
                create_obs_count_link(
                  ds$obs_count,
                  ds$ds_code,
                  ds$name %|||% ds$ds_code
                )
              )
            )
          )
        ),
        if (has_valid_field_value(ds, "description")) {
          htmltools::tags$div(
            style = "margin-top: 15px;",
            create_section_header("Description", "10px"),
            htmltools::tags$div(
              id = "dataset-description",
              htmltools::HTML(sanitize_description_html(ds$description))
            )
          )
        }
      )
    }),

    dataset_citation = shiny::renderUI({
      build_dataset_citation_text(ds, author_name, start_year)
    })
  )
}

#' Parse Dataset Owner Label to "First Last" Format
#'
#' Converts a VegBank owner_label in "Last, First" format to "First Last".
#'
#' @param owner_label Character string in "Last, First" format (e.g., "Palmquist, Kyle")
#' @return Character string in "First Last" format, or the original value if not parseable
#' @noRd
parse_dataset_author_label <- function(owner_label) {
  if (is.null(owner_label) || is.na(owner_label) || !nzchar(trimws(as.character(owner_label)))) {
    return("Unknown Author")
  }
  parts <- strsplit(trimws(as.character(owner_label)), ",\\s*")[[1]]
  if (length(parts) == 2) {
    paste(trimws(parts[2]), trimws(parts[1]))
  } else {
    as.character(owner_label)
  }
}

#' Build Dataset Citation Text
#'
#' Constructs a formatted citation string for a VegBank dataset in the format:
#' Author Name (Start Year): VegBank Plot Observations: "Dataset Name".
#' VegBank. Dataset. accession_code.
#'
#' @param ds Single-row dataframe of dataset fields
#' @param author_name Author name already formatted as "First Last"
#' @param start_year Formatted start year string (or "Unspecified")
#' @return A character string containing the full citation
#' @noRd
build_dataset_citation_text <- function(ds, author_name, start_year) {
  # Escape all interpolated fields
  safe_author    <- htmltools::htmlEscape(as.character(author_name %|||% "Unknown Author"))
  safe_name      <- htmltools::htmlEscape(as.character(ds$name %|||% "Unnamed Dataset"))
  safe_accession <- htmltools::htmlEscape(as.character(ds$accession_code %|||% "Unspecified"))

  paste0(
    safe_author, " (", start_year, "): VegBank Plot Observations: \u201C",
    safe_name, "\u201D. VegBank. Dataset. ", safe_accession, "."
  )
}
