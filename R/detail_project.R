#' Build Project Details View
#' @noRd
build_project_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("project_name", "project_description", "project_dates", "project_observations", "project_contributors"),
      "Project details"
    ))
  }

  # Format date fields with helper function for consistency
  date_fields <- intersect(c("start_date", "stop_date", "last_plot_added_date"), names(result))
  for (field in date_fields) {
    result[[field]] <- vapply(result[[field]], format_date, character(1), USE.NAMES = FALSE)
  }

  list(
    project_name = shiny::renderUI({
      htmltools::tags$p(result$project_name)
    }),
    project_observations = shiny::renderUI({
      htmltools::tags$p("Number of observations: ", htmltools::tags$strong(result$obs_count))
    }),
    project_description = shiny::renderUI({
      htmltools::tags$div(id = "project-description", htmltools::HTML(result$project_description))
    }),
    # TODO: Implement contributor listing when API supports cross-resource queries
    project_contributors = shiny::renderUI({
      htmltools::tags$p("No contributors available")
    }),
    project_dates = render_detail_table(c("start_date", "stop_date", "last_plot_added_date"), result)
  )
}
