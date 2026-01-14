#' Build Cover Method Details View
#'
#' Constructs the complete detail view for a cover method, including overview and
#' detail sections. Handles NULL or empty results gracefully by returning empty UI elements.
#'
#' @param result A dataframe containing cover method data from vegbankr::vb_get_cover_methods()
#' @return A named list with two shiny.render.function elements: cover_method_header, cover_method_details
#' @noRd
build_cover_method_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("cover_method_header", "cover_method_details"),
      "Cover method details"
    ))
  }

  cm <- result[1, , drop = FALSE]

  # Overview Card - cm_code and cover_type
  header_ui <- shiny::renderUI({
    cm_code <- cm$cm_code %|||% "Not recorded"
    cover_type <- cm$cover_type %|||% "Not recorded"

    htmltools::tags$div(
      htmltools::tags$h5(cover_type, style = "font-weight: 600; margin-bottom: 0px;"),
      htmltools::tags$h5(cm_code, style = "color: var(--vegbank-green); font-weight: 600;")
    )
  })

  # Detail Card - cover_estimation_method and reference link
  details_ui <- shiny::renderUI({
    cover_estimation_method <- cm$cover_estimation_method %|||% "Not recorded"

    # Create reference link if rf_code and rf_label exist
    reference_display <- if (has_valid_field_value(cm, "rf_code") &&
      has_valid_field_value(cm, "rf_label")) {
      create_detail_link("ref_link_click", cm$rf_code, cm$rf_label)
    } else {
      "Not provided"
    }

    details <- list(
      cover_estimation_method = cover_estimation_method,
      reference = reference_display
    )

    col_names <- c(
      cover_estimation_method = "Cover Estimation Method",
      reference = "Reference"
    )

    create_detail_table(details, col_names = col_names)
  })

  list(
    cover_method_header = header_ui,
    cover_method_details = details_ui
  )
}
