#' Build Plot Observation Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed plot observation information.
#'
#' @param result A list of 3 dataframes (plot_observation, taxa, and communities)
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags
#' @importFrom shiny renderUI
#' @noRd
build_plot_obs_details_view <- function(result) {
  taxa_details_ui <- shiny::renderUI({
    tryCatch(
      {
        taxa <- result$taxa
        if (is.null(taxa) || nrow(taxa) == 0) {
          return(htmltools::tags$p("No taxa recorded"))
        }

        taxa$cover <- as.numeric(taxa$cover)
        sorted_taxa <- taxa[order(-taxa$cover), ]
        rows <- lapply(seq_len(nrow(sorted_taxa)), function(i) {
          row <- sorted_taxa[i, ]
          htmltools::tags$tr(
            htmltools::tags$td(row$int_curr_plant_sci_name_no_auth),
            htmltools::tags$td(style = "text-align: right;", sprintf("%.2f%%", row$cover))
          )
        })
        htmltools::tags$table(
          class = "table table-sm table-striped table-hover",
          htmltools::tags$thead(
            htmltools::tags$tr(htmltools::tags$th("Scientific Name"), htmltools::tags$th("Cover"))
          ), htmltools::tags$tbody(rows)
        )
      },
      error = function(e) {
        paste("Error processing taxa:", e$message)
      }
    )
  })

  communities_details_ui <- shiny::renderUI({
    tryCatch(
      {
        communities <- result$communities
        if (is.null(communities) || nrow(communities) == 0) {
          return(htmltools::tags$p("No communities recorded"))
        }

        rows <- lapply(seq_len(nrow(communities)), function(i) {
          row <- communities[i, ]
          htmltools::tags$tr(
            htmltools::tags$td(
              htmltools::tags$a(
                href = "#",
                onclick = sprintf(
                  "Shiny.setInputValue('comm_link_click', '%s', {priority: 'event'}); return false;",
                  row$cc_code
                ),
                row$comm_name
              )
            )
          )
        })
        htmltools::tags$table(class = "table table-sm table-striped table-hover", htmltools::tags$tbody(rows))
      },
      error = function(e) {
        paste("Error processing communities:", e$message)
      }
    )
  })

  list(
    plot_id_details = render_detail_table(
      c("author_obs_code", "author_plot_code"),
      result$plot_observation
    ),
    location_details = render_detail_table(
      c("confidentiality_text", "latitude", "longitude", "location_narrative", "state_province", "country"),
      result$plot_observation
    ),
    layout_details = render_detail_table(
      c("area", "permanence"),
      result$plot_observation
    ),
    environmental_details = render_detail_table(
      c("elevation", "slope_aspect", "slope_gradient"),
      result$plot_observation
    ),
    methods_details = render_detail_table(
      c(
        "obs_start_date", "project_name", "cover_type", "stratum_method_name", "stratum_method_description",
        "taxon_observation_area", "auto_taxon_cover"
      ),
      result$plot_observation
    ),
    plot_quality_details = render_detail_table(
      "plot_validation_level_descr",
      result$plot_observation
    ),
    taxa_details = taxa_details_ui,
    communities_details = communities_details_ui
  )
}
