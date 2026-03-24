#' Build Project Details View
#' @noRd
build_project_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("project_header", "project_description", "project_dates", "project_observations", "project_contributors"),
      "Project details"
    ))
  }

  # Format date fields with helper function for consistency
  date_range <- format_date_range(result$start_date, result$stop_date)
  date_fields <- intersect(c("start_date", "stop_date", "last_plot_added_date"), names(result))
  for (field in date_fields) {
    result[[field]] <- vapply(result[[field]], format_date, character(1), USE.NAMES = FALSE)
  }

  list(
    project_header = shiny::renderUI({
      htmltools::div(
        htmltools::tags$h5(result$project_name, style = "font-weight: 600; margin-bottom: 0px;"),
        htmltools::tags$h5(result$pj_code, style = "color: var(--vb-green); font-weight: 600;"),
        if (date_range != "Unspecified date") {
          htmltools::tags$p(
            date_range
          )
        }
      )
    }),
    project_observations = shiny::renderUI({
      htmltools::tags$p(
        "Number of observations: ",
        create_obs_count_link(
          result$obs_count,
          result$pj_code,
          result$project_name
        )
      )
    }),

    project_description = shiny::renderUI({
      if (has_valid_field_value(result, "project_description")) {
        htmltools::tags$div(id = "project-description", htmltools::HTML(sanitize_description_html(result$project_description)))
      } else {
        htmltools::p("No description recorded")
      }
    }),
    project_dates = render_detail_table(c("start_date", "stop_date", "last_plot_added_date"), result),
    # TODO: Implement contributor listing when API supports cross-resource queries
    project_contributors = shiny::renderUI({
      htmltools::tags$p("No contributors recorded")
    })
  )
}
