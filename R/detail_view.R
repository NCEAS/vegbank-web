#' @noRd
MAX_TAXA_RETURNED <- 100000

#' Detail View Router
#'
#' Coordinates fetching detail payloads and dispatching to entity-specific
#' builders that populate the overlay outputs.
#'
#' @param resource_type Type of detail to show (e.g., "plot-observation",
#'   "community-concept", "community-classification", "project",
#'   "plant-concept", "party")
#' @param vb_code VegBank identifier used to fetch the detail payload
#' @param output Shiny output object
#' @param session Shiny session object
#' @return TRUE when the view is rendered successfully, FALSE otherwise
#' @noRd
show_detail_view <- function(resource_type, vb_code, output, session) {
  shiny::withProgress(
    message = paste0("Loading ", resource_type, " details..."),
    value = 0.2,
    expr = {
      shiny::incProgress(0.3, "Fetching details")

      result <- switch(resource_type,
        "community-classification" =
          vegbankr::vb_get_community_classifications(vb_code, detail = "full", with_nested = TRUE),
        "community-concept" =
          vegbankr::vb_get_community_concepts(vb_code, detail = "full", with_nested = TRUE),
        "plot-observation" =
          vegbankr::vb_get_plot_observations(vb_code,
            detail = "full", with_nested = TRUE,
            max_taxa = MAX_TAXA_RETURNED
          ),
        "project" =
          vegbankr::vb_get_projects(vb_code, detail = "full"),
        "party" =
          vegbankr::vb_get_parties(vb_code, detail = "full"),
        "plant-concept" =
          vegbankr::vb_get_plant_concepts(vb_code, detail = "full", with_nested = TRUE),
        "reference" =
          vegbankr::vb_get_references(vb_code, detail = "full"),
        "cover-method" =
          vegbankr::vb_get_cover_methods(vb_code, detail = "full", with_nested = TRUE)
      )

      if (length(result) == 0) {
        shiny::incProgress(0.4, "Error loading details")
        shiny::showNotification(
          paste0("Failed to load ", resource_type, " details. Please try again."),
          type = "error"
        )
        return(FALSE)
      }

      # Clear all output slots - names must stay aligned with ui.R definitions
      output$plot_header <- shiny::renderUI(NULL)
      output$author_code_details <- shiny::renderUI(NULL)
      output$date_details <- shiny::renderUI(NULL)
      output$location_details <- shiny::renderUI(NULL)
      output$layout_details <- shiny::renderUI(NULL)
      output$environmental_details <- shiny::renderUI(NULL)
      output$methods_details <- shiny::renderUI(NULL)
      output$plot_quality_details <- shiny::renderUI(NULL)
      output$plot_vegetation_details <- shiny::renderUI(NULL)
      output$plot_misc_details <- shiny::renderUI(NULL)
      output$taxa_details <- shiny::renderUI(NULL)
      output$communities_details <- shiny::renderUI(NULL)
      output$community_concept_header <- shiny::renderUI(NULL)
      output$community_concept_details <- shiny::renderUI(NULL)
      output$community_party_perspective <- shiny::renderUI(NULL)
      output$taxon_name <- shiny::renderUI(NULL)
      output$taxon_scientific <- shiny::renderUI(NULL)
      output$taxon_common <- shiny::renderUI(NULL)
      output$taxon_coverage <- shiny::renderUI(NULL)
      output$taxon_identifiers <- shiny::renderUI(NULL)
      output$taxon_aliases <- shiny::renderUI(NULL)
      output$comm_class_header <- shiny::renderUI(NULL)
      output$comm_class_details <- shiny::renderUI(NULL)
      output$comm_class_interpretations <- shiny::renderUI(NULL)
      output$comm_class_contributors <- shiny::renderUI(NULL)
      output$project_header <- shiny::renderUI(NULL)
      output$project_description <- shiny::renderUI(NULL)
      output$project_dates <- shiny::renderUI(NULL)
      output$project_contributors <- shiny::renderUI(NULL)
      output$project_observations <- shiny::renderUI(NULL)
      output$party_header <- shiny::renderUI(NULL)
      output$party_organization <- shiny::renderUI(NULL)
      output$party_contact <- shiny::renderUI(NULL)
      output$party_contributions <- shiny::renderUI(NULL)
      output$plant_concept_header <- shiny::renderUI(NULL)
      output$plant_concept_details <- shiny::renderUI(NULL)
      output$plant_party_perspective <- shiny::renderUI(NULL)
      output$reference_header <- shiny::renderUI(NULL)
      output$reference_identifiers <- shiny::renderUI(NULL)
      output$reference_publication <- shiny::renderUI(NULL)
      output$cover_method_header <- shiny::renderUI(NULL)
      output$cover_method_details <- shiny::renderUI(NULL)
      output$cover_method_indexes <- shiny::renderUI(NULL)

      shiny::incProgress(0.5, "Processing details")

      switch(resource_type,
        "project" = {
          details <- build_project_details_view(result)
          output$project_header <- details$project_header
          output$project_description <- details$project_description
          output$project_dates <- details$project_dates
          output$project_observations <- details$project_observations
          output$project_contributors <- details$project_contributors
        },
        "party" = {
          details <- build_party_details_view(result)
          output$party_header <- details$party_header
          output$party_organization <- details$party_organization
          output$party_contact <- details$party_contact
          output$party_contributions <- details$party_contributions
        },
        "community-classification" = {
          details <- build_comm_class_details_view(result)
          output$comm_class_header <- details$comm_class_header
          output$comm_class_details <- details$comm_class_details
          output$comm_class_interpretations <- details$comm_class_interpretations
          output$comm_class_contributors <- details$comm_class_contributors
        },
        "community-concept" = {
          details <- build_comm_concept_details_view(result)
          output$community_concept_header <- details$community_concept_header
          output$community_concept_details <- details$community_concept_details
          output$community_party_perspective <- details$community_party_perspective
        },
        "plot-observation" = {
          details <- build_plot_obs_details_view(result)
          output$plot_header <- details$plot_header
          output$author_code_details <- details$author_code_details
          output$date_details <- details$date_details
          output$location_details <- details$location_details
          output$layout_details <- details$layout_details
          output$environmental_details <- details$environmental_details
          output$methods_details <- details$methods_details
          output$plot_quality_details <- details$plot_quality_details
          output$plot_vegetation_details <- details$plot_vegetation_details
          output$plot_misc_details <- details$plot_misc_details
          output$taxa_details <- details$taxa_details
          output$communities_details <- details$communities_details
        },
        "plant-concept" = {
          details <- build_plant_concept_details_view(result)
          output$plant_concept_header <- details$plant_concept_header
          output$plant_concept_details <- details$plant_concept_details
          output$plant_party_perspective <- details$plant_party_perspective
        },
        "reference" = {
          details <- build_reference_details_view(result)
          output$reference_header <- details$reference_header
          output$reference_identifiers <- details$reference_identifiers
          output$reference_publication <- details$reference_publication
        },
        "cover-method" = {
          details <- build_cover_method_details_view(result)
          output$cover_method_header <- details$cover_method_header
          output$cover_method_details <- details$cover_method_details
          output$cover_method_indexes <- details$cover_method_indexes
        }
      )

      shiny::incProgress(0.6, "Details ready")
      session$sendCustomMessage("openOverlay", list())
      session$sendCustomMessage("updateDetailType", list(type = resource_type))

      TRUE
    }
  )
}
