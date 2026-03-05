#' Build Reference Details View
#'
#' Constructs the complete detail view for a reference, including summary, identifiers, and
#' publication sections. Handles NULL or empty results gracefully by returning empty UI elements.
#'
#' @param result A dataframe containing reference data from vegbankr::vb_get_references()
#' @return A named list with three shiny.render.function elements: reference_header, reference_identifiers, reference_publication
#' @noRd
build_reference_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("reference_header", "reference_identifiers", "reference_publication"),
      "Reference details"
    ))
  }

  ref <- result[1, , drop = FALSE]
  ref$publication_date <- format_date(ref$publication_date)

  summary_ui <- shiny::renderUI({
    reference_type <- ref$reference_type
    vb_code <- ref$rf_code
    label <- ref$rf_label %|||% "Unnamed Reference"
    htmltools::tags$div(
      if (has_valid_field_value(result, "reference_type")) {
        htmltools::tags$i(tools::toTitleCase(reference_type))
      },
      htmltools::tags$h5(label, style = "font-weight: 600; margin-bottom: 0px;"),
      htmltools::tags$h5(vb_code, style = "color: var(--vb-green); font-weight: 600;"),
      if (has_valid_field_value(result, "publication_date")) {
        htmltools::tags$p(paste0("Published: ", (ref$publication_date)))
      }
    )
  })

  identifiers_ui <- build_reference_identifiers_ui(ref)

  publication_ui <- build_reference_publication_ui(ref)

  list(
    reference_header = summary_ui,
    reference_identifiers = identifiers_ui,
    reference_publication = publication_ui
  )
}

#' Build Reference Identifiers UI
#'
#' Creates a Shiny UI element displaying reference identifiers (DOI, URL, ISBN).
#' URLs are rendered as clickable links. Shows a fallback message when no identifiers are available.
#'
#' @param ref A single-row dataframe containing reference data
#' @return A shiny.render.function that renders the identifiers table or fallback message
#' @noRd
build_reference_identifiers_ui <- function(ref) {
  identifier_fields <- c("doi", "url", "isbn")
  available_identifier_fields <- identifier_fields[identifier_fields %in% names(ref)]

  fields_with_values <- available_identifier_fields[
    vapply(available_identifier_fields, function(field) has_valid_field_value(ref, field), logical(1))
  ]

  if (length(fields_with_values) == 0) {
    return(shiny::renderUI({
      htmltools::tags$p("No DOI, ISBN, or URL recorded")
    }))
  }

  display_names <- get_field_display_names()
  shiny::renderUI({
    # Format values with special handling for URLs
    formatted_values <- lapply(fields_with_values, function(field_name) {
      raw_value <- ref[[field_name]]

      # Return "Unspecified" for missing/empty values
      if (is.null(raw_value) || length(raw_value) == 0 || all(is.na(raw_value))) {
        return("Unspecified")
      }

      field_value <- raw_value[1]
      if (is.na(field_value) || trimws(as.character(field_value)) == "") {
        return("Unspecified")
      }

      # Render URLs as clickable links only for safe schemes (http/https).
      # Reject other schemes (e.g. javascript:, data:) to prevent stored XSS.
      if (field_name == "url") {
        url_string <- as.character(field_value)
        url_lower <- tolower(trimws(url_string))
        is_safe_url <- startsWith(url_lower, "http://") || startsWith(url_lower, "https://")
        if (is_safe_url) {
          return(htmltools::tags$a(
            href = url_string,
            target = "_blank",
            rel = "noopener noreferrer",
            url_string
          ))
        } else {
          return(url_string)
        }
      }

      field_value
    })
    names(formatted_values) <- fields_with_values
    create_detail_table(formatted_values, col_names = display_names)
  })
}

#' Build Reference Publication UI
#'
#' Creates a Shiny UI element displaying reference publication details.
#' Shows the full citation prominently, followed by additional publication metadata in a table.
#'
#' @param ref A single-row dataframe containing reference data
#' @return A shiny.render.function that renders the citation and publication information table
#' @noRd
build_reference_publication_ui <- function(ref) {
  shiny::renderUI({
    citation_text <- ref$full_citation %|||% "No citation recorded"

    publication_fields <- c(
      "title", "publisher", "publication_place", "publication_date",
      "total_pages", "degree", "journal"
    )

    table_content <- format_fields_for_detail_table(publication_fields, ref)

    htmltools::tagList(
      create_section_header("Citation"),
      htmltools::tags$p(citation_text, style = "margin-top: 8px; margin-bottom: 16px;"),
      create_section_header("Other information"),
      table_content
    )
  })
}
