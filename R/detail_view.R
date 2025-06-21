#' Detail View Functions
#'
#' Functions for building the detail view overlay for various entities


#' Show Detail View
#'
#' Generic function to fetch and display details in the overlay.
#'
#' @param detail_type Type of detail to show ("plot-observation", "community-concept", or "taxon-observation")
#' @param accession_code The accession code to fetch details for
#' @param output The Shiny output object
#' @param session The Shiny session object
#' @return Boolean indicating success or failure
#'
#' @noRd
show_detail_view <- function(detail_type, accession_code, output, session) {
  # Use native Shiny progress functions
  shiny::withProgress(
    expr = {
      shiny::incProgress(0.3, "Fetching details")

      result <- switch(detail_type,
        "community-concept" = vegbankr::get_community_concept(accession_code),
        "taxon-observation" = vegbankr::get_taxon_observation(accession_code),
        "plot-observation" = vegbankr::get_plot_observation_details(accession_code)
      )

      if (length(result) == 0) {
        shiny::incProgress(0.4, "Error loading details")
        shiny::showNotification(
          paste0("Failed to load ", detail_type, " details. Please try again."),
          type = "error"
        )
        return(FALSE)
      }

      # Clear all output slots - ENSURE NAMES MATCH WHAT'S IN THE UI
      output$plot_id_details <- shiny::renderUI(NULL)
      output$location_details <- shiny::renderUI(NULL) # Changed from locationDetails
      output$layout_details <- shiny::renderUI(NULL)
      output$environmental_details <- shiny::renderUI(NULL)
      output$methods_details <- shiny::renderUI(NULL)
      output$plot_quality_details <- shiny::renderUI(NULL)
      output$taxa_details <- shiny::renderUI(NULL) # Changed from taxaDetails
      output$community_name <- shiny::renderUI(NULL)
      output$community_description <- shiny::renderUI(NULL)
      output$occurence_count <- shiny::renderUI(NULL)
      output$taxon_name <- shiny::renderUI(NULL)
      output$taxon_scientific <- shiny::renderUI(NULL)
      output$taxon_common <- shiny::renderUI(NULL)
      output$taxon_coverage <- shiny::renderUI(NULL)
      output$taxon_identifiers <- shiny::renderUI(NULL)

      # Generate the appropriate view based on detail type
      switch(detail_type,
        "community-concept" = {
          shiny::incProgress(0.5, "Processing community details")
          details <- build_community_details_view(result)
          output$community_name <- details$community_name
          output$community_description <- details$community_description
          output$observation_count <- details$observation_count
          output$community_aliases <- details$community_aliases
        },
        "taxon-observation" = {
          shiny::incProgress(0.5, "Processing taxon details")
          details <- build_taxon_details_view(result)
          output$taxon_name <- details$taxon_name
          output$taxon_aliases <- details$taxon_aliases
          output$taxon_coverage <- details$taxon_coverage
          output$taxon_identifiers <- details$taxon_identifiers
        },
        "plot-observation" = {
          shiny::incProgress(0.5, "Processing plot observation details")
          details <- build_plot_obs_details_view(result)
          output$plot_id_details <- details$plot_id_details
          output$location_details <- details$location_details
          output$layout_details <- details$layout_details
          output$environmental_details <- details$environmental_details
          output$methods_details <- details$methods_details
          output$plot_quality_details <- details$plot_quality_details
          output$taxa_details <- details$taxa_details
          output$communities_details <- details$communities_details
        }
      )

      shiny::incProgress(0.6, paste0("Details ready"))
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
            htmltools::tags$tr(
              htmltools::tags$th("Scientific Name"),
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
                  row$accession_code
                ),
                row$comm_name
              )
            )
          )
        })
        htmltools::tags$table(
          class = "table table-sm table-striped table-hover",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th("Community Classification")
            )
          ),
          htmltools::tags$tbody(rows)
        )
      },
      error = function(e) {
        paste("Error processing communities:", e$message)
      }
    )
  })

  list(
    plot_id_details = safe_render_details(
      c(
        "author_obs_code",
        "author_plot_code"
      ),
      result$plot_observation
    ),
    location_details = safe_render_details(
      c(
        "confidentiality_text",
        "latitude",
        "longitude",
        "location_narrative",
        "state_province",
        "country"
      ),
      result$plot_observation
    ),
    layout_details = safe_render_details(
      c(
        "area",
        "permanence"
      ),
      result$plot_observation
    ),
    environmental_details = safe_render_details(
      c(
        "elevation",
        "slope_aspect",
        "slope_gradient"
      ),
      result$plot_observation
    ),
    methods_details = safe_render_details(c(
      "obs_start_date",
      "project_name",
      "cover_type",
      "stratum_method_name",
      "stratum_method_description",
      "taxon_observation_area",
      "auto_taxon_cover"
    ), result$plot_observation),
    plot_quality_details = safe_render_details(
      "plot_validation_level_descr",
      result$plot_observation
    ),
    taxa_details = taxa_details_ui,
    communities_details = communities_details_ui
  )
}

#' Build Community Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed community information.
#'
#' @param result A data frame returned by vebankr::get_community_details()
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#' @importFrom tidyr pivot_wider
#' @noRd
build_community_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(list(
      community_name = shiny::renderUI({
        htmltools::tags$p("Community details not available")
      }),
      community_description = shiny::renderUI({
        htmltools::tags$p("No description available")
      }),
      observation_count = shiny::renderUI({
        htmltools::tags$p("No observations available")
      }),
      community_aliases = shiny::renderUI({
        htmltools::tags$p("No aliases available")
      })
    ))
  }

  scientific_class <- subset(result, result$class_system == "Scientific")

  # Create aliases dataframe with each class_system as a column
  aliases <- tidyr::pivot_wider(
    data = result[, c("class_system", "comm_name")],
    names_from = "class_system",
    values_from = "comm_name"
  )

  list(
    community_name = shiny::renderUI({
      htmltools::tags$b(scientific_class$comm_name)
    }),
    observation_count = shiny::renderUI({
      htmltools::tags$p(
        "Number of observations: ",
        htmltools::tags$strong(scientific_class$obs_count)
      )
    }),
    community_description = shiny::renderUI({
      # The description contains HTML entities <i></i> that need to be properly rendered
      htmltools::tags$div(
        id = "community-description",
        htmltools::HTML(scientific_class$comm_description)
      )
    }),
    community_aliases = safe_render_details(
      colnames(aliases),
      aliases
    )
  )
}

#' Build Taxon Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed taxon information.
#'
#' @param result A data frame returned by vebankr::get_taxon_observation_details()
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
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
      htmltools::tags$b(result$author_plant_name)
    }),
    taxon_coverage = shiny::renderUI({
      htmltools::tags$div(
        htmltools::tags$p(
          htmltools::tags$strong("Cover percentage: "),
          paste0(result$max_cover, "%")
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Taxon inference area: "),
          paste0(result$taxon_inference_area, " m\u00B2") # Replace m² with Unicode escape
        )
      )
    }),
    taxon_aliases = safe_render_details(
      c(
        "int_curr_plant_common",
        "int_curr_plant_sci_name_no_auth",
        "int_curr_plant_sci_full",
        "int_orig_plant_common",
        "int_orig_plant_sci_name_no_auth",
        "int_orig_plant_sci_full"
      ),
      result
    ),
    taxon_identifiers = safe_render_details(
      c(
        "taxon_observation_id",
        "int_curr_plant_code",
        "int_orig_plant_code"
      ),
      result
    )
  )
}

#' Read Display Names from Lookup Table
#'
#' Reads display names from the display_name_lookup.txt file
#'
#' @return A named vector where names are snake_case field names and values are display names
#'
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @noRd
get_field_display_names <- function() {
  file_path <- system.file("shiny/www/display_name_lookup.txt", package = "vegbankWeb")

  # If running in development mode and file not found in package
  if (file_path == "") {
    pkg_root <- tryCatch(
      {
        rprojroot::find_package_root_file()
      },
      error = function(e) {
        getwd()
      }
    )
    file_path <- file.path(pkg_root, "inst/shiny/www/display_name_lookup.txt")
  }

  # Read lookup table
  if (file.exists(file_path)) {
    lookup <- utils::read.csv(file_path, stringsAsFactors = FALSE, comment.char = "/")
    # Create a named vector: snake_case -> display
    display_names <- setNames(lookup$display, lookup$snake)
    display_names
  } else {
    warning("Display name lookup file not found: ", file_path)
    c()
  }
}

safe_render_details <- function(fields, dataframe) {
  # Read display names from the lookup file
  display_names <- get_field_display_names()

  shiny::renderUI({
    # First check if all fields exist
    valid_fields <- fields[fields %in% colnames(dataframe)]
    if (length(valid_fields) == 0) {
      return(htmltools::tags$p("No data available for this section"))
    }

    values <- lapply(dataframe[valid_fields], function(x) {
      if (is.null(x) || all(is.na(x))) "Not recorded" else x
    })

    # Convert logical values to human-readable text
    values <- lapply(values, function(x) {
      if (is.logical(x)) ifelse(x, "Yes", "No") else x
    })

    create_detail_table(values, col_names = display_names)
  })
}

create_detail_table <- function(details, col_names) {
  htmltools::tags$table(
    class = "table table-sm table-striped table-hover",
    htmltools::tags$tbody(
      lapply(names(details), function(name) {
        display_name <- if (name %in% names(col_names)) col_names[[name]] else name
        htmltools::tags$tr(
          htmltools::tags$td(htmltools::tags$strong(display_name)),
          htmltools::tags$td(class = "text-end", details[[name]])
        )
      })
    )
  )
}
