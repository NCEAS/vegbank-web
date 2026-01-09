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
        if (date_range != "Date not recorded") {
          htmltools::tags$p(
            date_range
          )
        }
      )
    }),
    project_observations = shiny::renderUI({
      obs_count <- suppressWarnings(as.integer(result$obs_count))
      if (is.na(obs_count)) obs_count <- 0L

      if (obs_count > 0 && !is.null(result$pj_code) && !is.null(result$project_name)) {
        htmltools::tags$p(
          "Number of observations: ",
          htmltools::tags$a(
            href = "#",
            class = "obs-count-link dt-shiny-action",
            `data-input-id` = "obs_count_click",
            `data-value` = htmltools::htmlEscape(result$pj_code, attribute = TRUE),
            `data-label` = htmltools::htmlEscape(result$project_name, attribute = TRUE),
            as.character(obs_count)
          )
        )
      } else {
        htmltools::tags$p("Number of observations: ", htmltools::tags$strong(obs_count))
      }
    }),

    project_description = shiny::renderUI({
      htmltools::tags$div(id = "project-description", htmltools::htmlEscape(result$project_description))
    }),
    project_dates = render_detail_table(c("start_date", "stop_date", "last_plot_added_date"), result),
    # TODO: Implement contributor listing when API supports cross-resource queries
    project_contributors = shiny::renderUI({
      htmltools::tags$p("No contributors available")
    })
  )
}
