#' Build Project Details View
#' @noRd
build_project_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(list(
      project_name = shiny::renderUI({
        htmltools::tags$p("project details not available")
      }),
      project_description = shiny::renderUI({
        htmltools::tags$p("No description available")
      }),
      project_dates = shiny::renderUI({
        htmltools::tags$p("No dates available")
      }),
      project_observations = shiny::renderUI({
        htmltools::tags$p("No observations available")
      }),
      project_contributors = shiny::renderUI({
        htmltools::tags$p("No contributors available")
      })
    ))
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
    project_contributors = shiny::renderUI({
      htmltools::tags$p("No contributors available")
    }),
    project_dates = safe_render_details(c("start_date", "stop_date", "last_plot_added_date"), result)
  )
}
