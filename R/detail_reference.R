#' Build Reference Details View
#'
#' Constructs the complete detail view for a reference, including summary, identifiers, and
#' publication sections. Handles NULL or empty results gracefully by returning empty UI elements.
#'
#' @param result A dataframe containing reference data from vegbankr::get_reference()
#' @return A named list with three shiny.render.function elements: reference_summary, reference_identifiers, reference_publication
#' @noRd
build_reference_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    empty_ui <- shiny::renderUI({
      htmltools::tags$p("Reference details not available")
    })
    return(list(
      reference_summary = empty_ui,
      reference_identifiers = empty_ui,
      reference_publication = empty_ui
    ))
  }

  ref <- result[1, , drop = FALSE]
  ref$publication_date <- {
    parsed <- safe_parse_date(ref$publication_date)
    if (is.na(parsed)) {
      ref$publication_date
    } else {
      format(parsed, "%Y-%m-%d")
    }
  }

  summary_ui <- shiny::renderUI({
    short_name <- ref$short_name %|||% "Reference not recorded"
    reference_type <- ref$reference_type
    type_available <- !is.null(reference_type) && !is.na(reference_type) && reference_type != ""
    htmltools::tags$div(
      if (type_available) htmltools::tags$div(htmltools::tags$i(tools::toTitleCase(reference_type))),
      htmltools::tags$div(short_name, style = "font-weight: 600;")
    )
  })

  identifiers_ui <- build_reference_identifiers_ui(ref)

  publication_ui <- build_reference_publication_ui(ref)

  list(
    reference_summary = summary_ui,
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

  has_identifier_value <- function(field) {
    value <- ref[[field]]
    if (is.null(value) || length(value) == 0) {
      return(FALSE)
    }
    if (all(is.na(value))) {
      return(FALSE)
    }
    !all(trimws(as.character(value)) == "")
  }

  fields_with_values <- available_identifier_fields[
    vapply(available_identifier_fields, has_identifier_value, logical(1))
  ]

  if (length(fields_with_values) == 0) {
    return(shiny::renderUI({
      htmltools::tags$p("No DOI, ISBN, or URL provided")
    }))
  }

  display_names <- get_field_display_names()
  shiny::renderUI({
    formatted_values <- lapply(fields_with_values, function(field_name) {
      raw_value <- ref[[field_name]]

      # Return "Not recorded" for missing/empty values
      if (is.null(raw_value) || length(raw_value) == 0 || all(is.na(raw_value))) {
        return("Not recorded")
      }

      field_value <- raw_value[1]
      if (is.na(field_value) || trimws(as.character(field_value)) == "") {
        return("Not recorded")
      }

      # Render URLs as clickable links
      if (field_name == "url") {
        url_string <- as.character(field_value)
        return(htmltools::tags$a(
          href = url_string,
          target = "_blank",
          rel = "noopener noreferrer",
          url_string
        ))
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
    citation_text <- ref$full_citation %|||% "Not recorded"

    publication_fields <- c(
      "title", "publisher", "publication_place", "publication_date",
      "total_pages", "degree", "journal"
    )
    table_content <- safe_render_details(publication_fields, ref)

    htmltools::tagList(
      htmltools::tags$div(
        "Citation",
        style = "font-weight: bold; width: 100%; border-bottom: 1px solid #2c5443;"
      ),
      htmltools::tags$p(citation_text, style = "margin-top: 8px; margin-bottom: 16px;"),
      htmltools::tags$div(
        "Other information",
        style = "font-weight: bold; width: 100%; border-bottom: 1px solid #2c5443; margin-bottom: 8px;"
      ),
      table_content
    )
  })
}
