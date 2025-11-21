# Shim to support temporary resource fetching until vegbankr is updated
get_resource_by_code_tmp <- function(resource, vb_code, ...) {
  vegbankr:::get_all_resources(resource = paste(resource, vb_code, sep = "/"), ...)
}

#' Detail View Router
#'
#' Coordinates fetching detail payloads and dispatching to entity-specific
#' builders that populate the overlay outputs.
#'
#' @param detail_type Type of detail to show (e.g., "plot-observation",
#'   "community-concept", "community-classification", "project",
#'   "taxon-observation", "plant-concept", "party")
#' @param vb_code VegBank identifier used to fetch the detail payload
#' @param output Shiny output object
#' @param session Shiny session object
#' @return TRUE when the view is rendered successfully, FALSE otherwise
#' @noRd
show_detail_view <- function(detail_type, vb_code, output, session) {
  shiny::withProgress(
    message = paste0("Loading ", detail_type, " details..."),
    value = 0.2,
    expr = {
      shiny::incProgress(0.3, "Fetching details")

      result <- switch(detail_type,
        "community-classification" = vegbankr::get_community_classification(vb_code),
        "community-concept" = vegbankr::get_community_concept(vb_code),
        "taxon-observation" = vegbankr::get_taxon_observation(vb_code),
        "plot-observation" = get_resource_by_code_tmp("plot-observations",
                                                      vb_code, detail = "full",
                                                      with_nested = TRUE,
                                                      num_taxa = 100000),
        "project" = vegbankr::get_project(vb_code),
        "party" = vegbankr::get_party(vb_code),
        "plant-concept" = vegbankr::get_plant_concept(vb_code),
        "reference" = vegbankr::get_reference(vb_code)
      )

      if (length(result) == 0) {
        shiny::incProgress(0.4, "Error loading details")
        shiny::showNotification(
          paste0("Failed to load ", detail_type, " details. Please try again."),
          type = "error"
        )
        return(FALSE)
      }

      # Clear all output slots - names must stay aligned with ui.R definitions
      output$plot_id_details <- shiny::renderUI(NULL)
      output$location_details <- shiny::renderUI(NULL)
      output$layout_details <- shiny::renderUI(NULL)
      output$environmental_details <- shiny::renderUI(NULL)
      output$methods_details <- shiny::renderUI(NULL)
      output$plot_quality_details <- shiny::renderUI(NULL)
      output$taxa_details <- shiny::renderUI(NULL)
      output$communities_details <- shiny::renderUI(NULL)
      output$community_concept_name <- shiny::renderUI(NULL)
      output$community_concept_details <- shiny::renderUI(NULL)
      output$community_party_perspective <- shiny::renderUI(NULL)
      output$taxon_name <- shiny::renderUI(NULL)
      output$taxon_scientific <- shiny::renderUI(NULL)
      output$taxon_common <- shiny::renderUI(NULL)
      output$taxon_coverage <- shiny::renderUI(NULL)
      output$taxon_identifiers <- shiny::renderUI(NULL)
      output$taxon_aliases <- shiny::renderUI(NULL)
      output$observation_details <- shiny::renderUI(NULL)
      output$community_interpretation <- shiny::renderUI(NULL)
      output$project_name <- shiny::renderUI(NULL)
      output$project_description <- shiny::renderUI(NULL)
      output$project_dates <- shiny::renderUI(NULL)
      output$project_contributors <- shiny::renderUI(NULL)
      output$project_observations <- shiny::renderUI(NULL)
      output$party_name <- shiny::renderUI(NULL)
      output$party_organization <- shiny::renderUI(NULL)
      output$party_contact <- shiny::renderUI(NULL)
      output$party_projects <- shiny::renderUI(NULL)
      output$plant_concept_name <- shiny::renderUI(NULL)
      output$plant_concept_details <- shiny::renderUI(NULL)
      output$plant_party_perspective <- shiny::renderUI(NULL)
      output$reference_summary <- shiny::renderUI(NULL)
      output$reference_identifiers <- shiny::renderUI(NULL)
      output$reference_publication <- shiny::renderUI(NULL)

      shiny::incProgress(0.5, "Processing details")

      switch(detail_type,
        "project" = {
          details <- build_project_details_view(result)
          output$project_name <- details$project_name
          output$project_description <- details$project_description
          output$project_dates <- details$project_dates
          output$project_contributors <- details$project_contributors
          output$project_observations <- details$project_observations
        },
        "party" = {
          details <- build_party_details_view(result)
          output$party_name <- details$party_name
          output$party_organization <- details$party_organization
          output$party_contact <- details$party_contact
          output$party_projects <- details$party_projects
        },
        "community-classification" = {
          details <- build_comm_class_details_view(result)
          output$observation_details <- details$observation_details
          output$community_interpretation <- details$community_interpretation
        },
        "community-concept" = {
          details <- build_comm_concept_details_view(result)
          output$community_concept_name <- details$community_concept_name
          output$community_concept_details <- details$community_concept_details
          output$community_party_perspective <- details$community_party_perspective
        },
        "taxon-observation" = {
          details <- build_taxon_details_view(result)
          output$taxon_name <- details$taxon_name
          output$taxon_aliases <- details$taxon_aliases
          output$taxon_coverage <- details$taxon_coverage
          output$taxon_identifiers <- details$taxon_identifiers
        },
        "plot-observation" = {
          details <- build_plot_obs_details_view(result)
          output$plot_id_details <- details$plot_id_details
          output$location_details <- details$location_details
          output$layout_details <- details$layout_details
          output$environmental_details <- details$environmental_details
          output$methods_details <- details$methods_details
          output$plot_quality_details <- details$plot_quality_details
          output$taxa_details <- details$taxa_details
          output$communities_details <- details$communities_details
        },
        "plant-concept" = {
          details <- build_plant_concept_details_view(result)
          output$plant_concept_name <- details$plant_concept_name
          output$plant_concept_details <- details$plant_concept_details
          output$plant_party_perspective <- details$plant_party_perspective
        },
        "reference" = {
          details <- build_reference_details_view(result)
          output$reference_summary <- details$reference_summary
          output$reference_identifiers <- details$reference_identifiers
          output$reference_publication <- details$reference_publication
        }
      )

      shiny::incProgress(0.6, "Details ready")
      session$sendCustomMessage("openOverlay", list())
      session$sendCustomMessage("updateDetailType", list(type = detail_type))

      TRUE
    }
  )
}
