#' Build Stratum Method Details View
#'
#' Constructs the complete detail view for a stratum method, including overview and
#' detail sections. Handles NULL or empty results gracefully by returning empty UI elements.
#'
#' @param result A dataframe containing stratum method data from vegbankr::vb_get_stratum_methods()
#' @return A named list with three shiny.render.function elements:
#'         stratum_method_header, stratum_method_details, and stratum_types
#' @noRd
build_stratum_method_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("stratum_method_header", "stratum_method_details", "stratum_types"),
      "Stratum method details"
    ))
  }

  sm <- result[1, , drop = FALSE]

  # Overview Card - sm_code and stratum_method_name
  header_ui <- shiny::renderUI({
    sm_code <- sm$sm_code %|||% "Not recorded"
    stratum_method_name <- sm$stratum_method_name %|||% "Not recorded"

    htmltools::tags$div(
      htmltools::tags$h5(stratum_method_name, style = "font-weight: 600; margin-bottom: 0px;"),
      htmltools::tags$h5(sm_code, style = "color: var(--vegbank-green); font-weight: 600;")
    )
  })

  # Detail Card - stratum_method_description, stratum_assignment, and reference link
  details_ui <- shiny::renderUI({
    stratum_method_description <- sm$stratum_method_description %|||% "Not recorded"
    stratum_assignment <- sm$stratum_assignment %|||% "Not recorded"

    # Create reference link if rf_code and rf_label exist
    reference_display <- if (has_valid_field_value(sm, "rf_code") &&
      has_valid_field_value(sm, "rf_label")) {
      create_detail_link("ref_link_click", sm$rf_code, sm$rf_label)
    } else {
      "Not provided"
    }

    details <- list(
      stratum_method_description = stratum_method_description,
      stratum_assignment = stratum_assignment,
      reference = reference_display
    )

    col_names <- c(
      stratum_method_description = "Stratum Method Description",
      stratum_assignment = "Stratum Assignment",
      reference = "Reference"
    )

    create_detail_table(details, col_names = col_names)
  })

  # Stratum Types Card - render table of stratum types
  types_ui <- shiny::renderUI({
    # Extract nested stratum_types
    stratum_types <- extract_nested_table(sm, "stratum_types")

    if (is.null(stratum_types) || nrow(stratum_types) == 0) {
      return(htmltools::tags$p("No stratum types recorded"))
    }

    # Build table rows
    rows <- lapply(seq_len(nrow(stratum_types)), function(i) {
      st <- stratum_types[i, ]
      htmltools::tags$tr(
        htmltools::tags$td(st$stratum_index %|||% ""),
        htmltools::tags$td(st$stratum_name %|||% ""),
        htmltools::tags$td(st$stratum_description %|||% "")
      )
    })

    htmltools::tagList(


      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        style = "width: 100%; table-layout: fixed; word-break: break-word; white-space: normal;",
        htmltools::tags$thead(
          htmltools::tags$tr(
            htmltools::tags$th(style = "width: 25%;", "Stratum Index"),
            htmltools::tags$th(style = "width: 35%;", "Stratum Name"),
            htmltools::tags$th(style = "width: 40%;", "Stratum Description")
          )
        ),
        htmltools::tags$tbody(rows)
      )
    )
  })

  list(
    stratum_method_header = header_ui,
    stratum_method_details = details_ui,
    stratum_types = types_ui
  )
}
