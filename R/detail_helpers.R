#' Detail Helper Functions
#'
#' Helper functions for creating links and handling detail views.
#' @keywords internal

#' Show Detail View
#'
#' Generic function to fetch and display details in the overlay.
#'
#' @param detail_type Type of detail to show ("plot" or "community")
#' @param accession_code The accession code to fetch details for
#' @param state The application state list
#' @param output The Shiny output object
#' @param session The Shiny session object
#' @param api_client API client to use for fetching details
#' @return Boolean indicating success or failure
#' @keywords internal
show_detail_view <- function(detail_type, accession_code, state, output, session, api_client) {
  show_progress(paste0("Loading ", detail_type, " details..."))(function(step, complete) {
    step(0.2, "Fetching details")

    # Determine which API function to call based on detail type
    result <- if (detail_type == "community") {
      api_client$get_community_details(accession_code)
    } else {
      api_client$get_observation_details(accession_code)
    }

    if (!result$success) {
      step(0.3, "Error loading details")
      shiny::showNotification(
        paste0("Failed to load ", detail_type, " details. Please try again."),
        type = "error"
      )
      return(FALSE)
    }

    step(0.5, "Processing details")

    # Clear all output slots
    output$plot_id_details <- shiny::renderUI(NULL)
    output$locationDetails <- shiny::renderUI(NULL)
    output$layout_details <- shiny::renderUI(NULL)
    output$environmental_details <- shiny::renderUI(NULL)
    output$methods_details <- shiny::renderUI(NULL)
    output$plot_quality_details <- shiny::renderUI(NULL)
    output$taxaDetails <- shiny::renderUI(NULL)
    output$community_name <- shiny::renderUI(NULL)
    output$community_description <- shiny::renderUI(NULL)

    # Generate the appropriate view based on detail type
    if (detail_type == "community") {
      details <- build_community_details_view(result$data)
      output$community_name <- details$community_name
      output$community_description <- details$community_description
      state$selected_community_accession(accession_code)
    } else {
      details <- build_details_view(result$data)
      output$plot_id_details <- details$plot_id_details
      output$locationDetails <- details$location_details
      output$layout_details <- details$layout_details
      output$environmental_details <- details$environmental_details
      output$methods_details <- details$methods_details
      output$plot_quality_details <- details$plot_quality_details
      output$taxaDetails <- details$taxa_details
      state$selected_accession(accession_code)
    }

    # Update state
    state$details_open(TRUE)
    state$detail_type(detail_type)

    complete(paste0(detail_type, " details ready"))
    session$sendCustomMessage("openOverlay", list())
    session$sendCustomMessage("updateDetailType", list(type = detail_type))

    return(TRUE)
  })
}
