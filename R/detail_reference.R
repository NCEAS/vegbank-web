#' Build Reference Details View
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

  # TODO: Make url actual link in UI
  identifiers_ui <-
    if (length(fields_with_values) == 0) {
      shiny::renderUI({
        htmltools::tags$p("No DOI, ISBN, or URL provided")
      })
    } else {
      safe_render_details(fields_with_values, ref)
    }

  # TODO: Format publication details more cleanly?
  publication_fields <- c(
    "title", "publisher", "publication_place", "publication_date",
    "total_pages", "full_citation", "degree", "journal"
  )
  publication_ui <- safe_render_details(publication_fields, ref)

  list(
    reference_summary = summary_ui,
    reference_identifiers = identifiers_ui,
    reference_publication = publication_ui
  )
}
