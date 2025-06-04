#' Detail View Functions
#'
#' Functions for building the detail view overlay for various entities


#' Show Detail View
#'
#' Generic function to fetch and display details in the overlay.
#'
#' @param detail_type Type of detail to show ("plot", "community", or "taxon-observation")
#' @param accession_code The accession code to fetch details for
#' @param output The Shiny output object
#' @param session The Shiny session object
#' @return Boolean indicating success or failure
#'
#' @noRd
show_detail_view <- function(detail_type, accession_code, output, session) {
  # Use the shared progress handler for handling progress updates
  progress_handler$with_safe_progress(
    expr = {
      progress_handler$inc_progress(0.2, "Fetching details")

      # Determine which API function to call based on detail type
      result <- if (detail_type == "community") {
        veg_bank_api$get_community_details(accession_code)
      } else if (detail_type == "taxon-observation") {
        veg_bank_api$get_taxon_observation_details(accession_code)
      } else {
        veg_bank_api$get_observation_details(accession_code)
      }

      if (!result$success) {
        progress_handler$inc_progress(0.3, "Error loading details")
        progress_handler$show_notification(
          paste0("Failed to load ", detail_type, " details. Please try again."),
          type = "error"
        )
        return(FALSE)
      }

      progress_handler$inc_progress(0.3, "Processing details")

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
      output$occurence_count <- shiny::renderUI(NULL)
      output$taxon_name <- shiny::renderUI(NULL)
      output$taxon_scientific <- shiny::renderUI(NULL)
      output$taxon_common <- shiny::renderUI(NULL)
      output$taxon_coverage <- shiny::renderUI(NULL)
      output$taxon_identifiers <- shiny::renderUI(NULL)

      # TODO: This should be a switch case for clarity
      # Generate the appropriate view based on detail type
      if (detail_type == "community") {
        # Get first row of community data bc multiple rows for multiple classsystems
        details <- build_community_details_view(result$data)
        output$community_name <- details$community_name
        output$community_description <- details$community_description
        output$occurence_count <- details$occurence_count
      } else if (detail_type == "taxon-observation") {
        details <- build_taxon_details_view(result$data)
        output$taxon_name <- details$taxon_name
        output$taxon_scientific <- details$taxon_scientific
        output$taxon_common <- details$taxon_common
        output$taxon_coverage <- details$taxon_coverage
        output$taxon_identifiers <- details$taxon_identifiers
      } else {
        details <- build_plot_obs_details_view(result$data)
        output$plot_id_details <- details$plot_id_details
        output$locationDetails <- details$location_details
        output$layout_details <- details$layout_details
        output$environmental_details <- details$environmental_details
        output$methods_details <- details$methods_details
        output$plot_quality_details <- details$plot_quality_details
        output$taxaDetails <- details$taxa_details
      }

      progress_handler$inc_progress(0.3, paste0(detail_type, " details ready"))
      session$sendCustomMessage("openOverlay", list())
      session$sendCustomMessage("updateDetailType", list(type = detail_type))

      TRUE
    },
    message = paste0("Loading ", detail_type, " details..."),
    value = 0.2
  )
}


#' Build Plot Observation Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed plot observation information.
#'
#' @param observation_data A data row representing the selected plot.
#' @return A list of Shiny UI outputs.
#' @importFrom htmltools tags
#' @importFrom shiny renderUI
#'
#' @noRd
build_plot_obs_details_view <- function(observation_data) {
  # Mapping internal field names to their display names.
  display_names <- list(
    authorplotcode = "Author Plot Code",
    authorobscode = "Author Observation Code",
    area = "Area",
    permanence = "Permanent",
    elevation = "Elevation",
    slopeaspect = "Slope Aspect",
    slopegradient = "Slope Gradient",
    confidentialitystatus = "Confidentiality Status",
    confidentialitytext = "Confidentiality Status",
    latitude = "Latitude",
    longitude = "Longitude",
    locationnarrative = "Location Description",
    stateprovince = "State/Province",
    country = "Country",
    obsstartdate = "Observation Start Date",
    project_id = "Project ID",
    projectname = "Project Name",
    covermethod_id = "Cover Method ID",
    covertype = "Cover Type",
    stratummethod_id = "Stratum Method ID",
    stratummethodname = "Stratum Method",
    stratummethoddescription = "Stratum Method Description",
    taxonobservationarea = "Taxon Observation Area",
    autotaxoncover = "Taxon Cover Automatically Calculated",
    plotvalidationlevel = "Plot Validation Level",
    plotvalidationleveldescr = "Validation Level"
  )

  create_table <- function(details, col_names) {
    htmltools::tags$table(
      class = "table table-sm table-striped table-hover",
      htmltools::tags$tbody(
        lapply(names(details), function(name) {
          display_name <- if (!is.null(col_names[[name]])) col_names[[name]] else name
          htmltools::tags$tr(
            htmltools::tags$td(htmltools::tags$strong(display_name)),
            htmltools::tags$td(class = "text-end", details[[name]])
          )
        })
      )
    )
  }

  safe_render_details <- function(fields) {
    shiny::renderUI({
      values <- lapply(observation_data[fields], function(x) {
        if (is.null(x) || all(is.na(x))) "Not recorded" else x
      })
      create_table(values, col_names = display_names)
    })
  }

  taxa_details_ui <- shiny::renderUI({
    tryCatch(
      {
        taxa <- observation_data[["taxa"]]
        if (is.null(taxa)) {
          "No taxa recorded"
        }
        if (!is.data.frame(taxa)) taxa <- as.data.frame(taxa)
        if (nrow(taxa) == 0) {
          "No taxa recorded"
        }

        taxa$cover <- as.numeric(taxa$cover)
        sorted_taxa <- taxa[order(-taxa$cover), ]
        rows <- lapply(seq_len(nrow(sorted_taxa)), function(i) {
          row <- sorted_taxa[i, ]
          htmltools::tags$tr(
            htmltools::tags$td(row$authorplantname),
            htmltools::tags$td(style = "text-align: right;", sprintf("%.2f%%", row$cover))
          )
        })
        htmltools::tags$table(
          class = "table table-sm table-striped table-hover",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th("Author Plant Name"),
              htmltools::tags$th("Cover")
            )
          ),
          htmltools::tags$tbody(rows)
        )
      },
      error = function(e) {
        paste("Error processing taxa:", e$message)
      }
    )
  })

  list(
    plot_id_details = safe_render_details(c("authorobscode", "authorplotcode")),
    location_details = safe_render_details(c(
      "confidentialitytext", "latitude", "longitude",
      "locationnarrative", "stateprovince", "country"
    )),
    layout_details = safe_render_details(c("area", "permanence")),
    environmental_details = safe_render_details(c("elevation", "slopeaspect", "slopegradient")),
    methods_details = safe_render_details(c(
      "obsstartdate", "projectname", "covertype",
      "stratummethodname", "stratummethoddescription",
      "taxonobservationarea", "autotaxoncover"
    )),
    plot_quality_details = safe_render_details("plotvalidationleveldescr"),
    taxa_details = taxa_details_ui
  )
}

#' Build Community Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed community information.
#'
#' @param community_data A data row representing the selected community.
#' @return A list of Shiny UI outputs.
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#'
#' @noRd
build_community_details_view <- function(community_data) {
  if (is.null(community_data)) {
    return(list(
      community_name = shiny::renderUI({
        htmltools::tags$p("Community details not available")
      }),
      community_description = shiny::renderUI({
        htmltools::tags$p("No description available")
      }),
      occurence_count = shiny::renderUI({
        htmltools::tags$p("No occurrences available")
      })
    ))
  }

  # TODO: when api is replaced by VegBankR this should not be necessary
  community_data <- as.data.frame(community_data) # Converting to data frame prefixes with "data."
  community_data <- community_data[1, ]  # Get the first row

  list(
    community_name = shiny::renderUI({
      htmltools::tags$b(community_data$data.commname)
    }),
    community_description = shiny::renderUI({
      # The description contains HTML entities that need to be properly rendered
      htmltools::tags$div(
        id = "community-description",
        htmltools::HTML(community_data$data.commdescription)
      )
    }),
    occurence_count = shiny::renderUI({
      htmltools::tags$p(
        "Number of occurrences: ",
        htmltools::tags$strong(community_data$data.obscount)
      )
    })
  )
}

#' Build Taxon Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed taxon information.
#'
#' @param taxon_data A data row representing the selected taxon.
#' @return A list of Shiny UI outputs.
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#'
#' @noRd
build_taxon_details_view <- function(taxon_data) {
  if (is.null(taxon_data)) {
    return(list(
      taxon_name = shiny::renderUI({
        htmltools::tags$p("Taxon details not available")
      }),
      taxon_scientific = shiny::renderUI({
        htmltools::tags$p("No scientific name available")
      }),
      taxon_common = shiny::renderUI({
        htmltools::tags$p("No common name available")
      }),
      taxon_coverage = shiny::renderUI({
        htmltools::tags$p("No coverage data available")
      }),
      taxon_identifiers = shiny::renderUI({
        htmltools::tags$p("No identifier information available")
      })
    ))
  }

  # TODO: when api is replaced by VegBankR this should not be necessary
  taxon_data <- as.data.frame(taxon_data) # Converting to data frame

  list(
    taxon_name = shiny::renderUI({
      htmltools::tags$b(taxon_data$data.authorplantname)
    }),
    taxon_scientific = shiny::renderUI({
      htmltools::tags$div(
        id = "taxon-scientific",
        htmltools::tags$p(
          htmltools::tags$strong("Current scientific name: "),
          htmltools::tags$em(taxon_data$data.int_currplantscinamenoauth)
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Full scientific name: "),
          htmltools::tags$em(taxon_data$data.int_currplantscifull)
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Original scientific name: "),
          htmltools::tags$em(taxon_data$data.int_origplantscinamenoauth)
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Full original name: "),
          htmltools::tags$em(taxon_data$data.int_origplantscifull)
        )
      )
    }),
    taxon_common = shiny::renderUI({
      htmltools::tags$div(
        htmltools::tags$p(
          htmltools::tags$strong("Current common name: "),
          taxon_data$data.int_currplantcommon
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Original common name: "),
          taxon_data$data.int_origplantcommon
        )
      )
    }),
    taxon_coverage = shiny::renderUI({
      htmltools::tags$div(
        htmltools::tags$p(
          htmltools::tags$strong("Cover percentage: "),
          paste0(taxon_data$data.maxcover, "%")
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Taxon inference area: "),
          paste0(taxon_data$data.taxoninferencearea, " m\u00B2") # Replace m² with Unicode escape
        )
      )
    }),
    taxon_identifiers = shiny::renderUI({
      htmltools::tags$div(
        htmltools::tags$p(
          htmltools::tags$strong("Taxon Observation ID: "),
          taxon_data$data.taxonobservation_id
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Current plant code: "),
          taxon_data$data.int_currplantcode
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Original plant code: "),
          taxon_data$data.int_origplantcode
        )
      )
    })
  )
}
