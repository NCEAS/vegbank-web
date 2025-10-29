#' Build Taxon Details View
#' @noRd
build_taxon_details_view <- function(result) {
  if (is.null(result)) {
    return(list(
      taxon_name = shiny::renderUI({
        htmltools::tags$p("Taxon details not available")
      }),
      taxon_coverage = shiny::renderUI({
        htmltools::tags$p("No coverage data available")
      }),
      taxon_aliases = shiny::renderUI({
        htmltools::tags$p("No aliases available")
      }),
      taxon_identifiers = shiny::renderUI({
        htmltools::tags$p("No identifier information available")
      })
    ))
  }

  list(
    taxon_name = shiny::renderUI({
      htmltools::tags$p(result$author_plant_name)
    }),
    taxon_coverage = shiny::renderUI({
      htmltools::tags$div(
        htmltools::tags$p(
          htmltools::tags$span("Cover percentage: "),
          paste0(result$max_cover, "%")
        ),
        htmltools::tags$p(
          htmltools::tags$span("Taxon inference area: "),
          paste0(result$taxon_inference_area, " m\u00B2")
        )
      )
    }),
    taxon_aliases = safe_render_details(
      c(
        "int_curr_plant_common", "int_curr_plant_sci_name_no_auth", "int_curr_plant_sci_full",
        "int_orig_plant_common", "int_orig_plant_sci_name_no_auth", "int_orig_plant_sci_full"
      ),
      result
    ),
    taxon_identifiers = safe_render_details(c("ob_code", "int_curr_pc_code", "int_orig_pc_code"), result)
  )
}
