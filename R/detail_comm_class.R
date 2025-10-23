#' Build Community Classification Details View
#'
#' @param result A data frame returned by vegbankr::get_community_classification()
#' @noRd
build_comm_class_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(list(
      observation_details = shiny::renderUI({
        htmltools::tags$p("Community details not available")
      }),
      community_interpretation = shiny::renderUI({
        htmltools::tags$p("No description available")
      })
    ))
  }

  list(
    observation_details = shiny::renderUI({
      safe_render_details(c("cl_code", "inspection", "table_analysis", "multivariate_analysis"), result)
    }),
    community_interpretation = shiny::renderUI({
      safe_render_details(c("cc_code", "class_fit", "class_confidence", "comm_authority_id", "type"), result)
    })
  )
}
