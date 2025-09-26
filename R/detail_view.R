#' Detail View Functions
#'
#' Functions for building the detail view overlay for various entities


#' Show Detail View
#'
#' Generic function to fetch and display details in the overlay.
#'
#' @param detail_type Type of detail to show ("plot-observation", "community-concept",
#' "community-classification", "project", "taxon-observation", or "plant-concept")
#' @param accession_code The accession code to fetch details for
#' @param output The Shiny output object
#' @param session The Shiny session object
#' TODO Remove this when vegbankr is updated
#' @param get_plant_concept_func Function to get plant concept data (optional)
#' @return Boolean indicating success or failure
#'
#' @noRd
show_detail_view <- function(detail_type, accession_code, output, session, get_plant_concept_func = NULL) {
  # Use native Shiny progress functions
  shiny::withProgress(
    expr = {
      shiny::incProgress(0.3, "Fetching details")

      result <- switch(detail_type,
        "community-classification" = vegbankr::get_community_classification(accession_code),
        "community-concept" = vegbankr::get_community_concept(accession_code),
        "taxon-observation" = vegbankr::get_taxon_observation(accession_code),
        "plot-observation" = vegbankr::get_plot_observation_details(accession_code),
        "project" = vegbankr::get_project(accession_code),
        "party" = vegbankr::get_party(accession_code),
        # TODO replace this when vegbankr is updated
        "plant-concept" = if (!is.null(get_plant_concept_func)) get_plant_concept_func(accession_code) else NULL
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
      output$location_details <- shiny::renderUI(NULL)
      output$layout_details <- shiny::renderUI(NULL)
      output$environmental_details <- shiny::renderUI(NULL)
      output$methods_details <- shiny::renderUI(NULL)
      output$plot_quality_details <- shiny::renderUI(NULL)
      output$taxa_details <- shiny::renderUI(NULL)
      output$community_name <- shiny::renderUI(NULL)
      output$community_description <- shiny::renderUI(NULL)
      output$observation_count <- shiny::renderUI(NULL)
      output$taxon_name <- shiny::renderUI(NULL)
      output$taxon_scientific <- shiny::renderUI(NULL)
      output$taxon_common <- shiny::renderUI(NULL)
      output$taxon_coverage <- shiny::renderUI(NULL)
      output$taxon_identifiers <- shiny::renderUI(NULL)
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
      output$plant_aliases <- shiny::renderUI(NULL)

      # Generate the appropriate view based on detail type
      switch(detail_type,
        "project" = {
          shiny::incProgress(0.5, "Processing project details")
          details <- build_project_details_view(result)
          output$project_name <- details$project_name
          output$project_description <- details$project_description
          output$project_dates <- details$project_dates
          output$project_contributors <- details$project_contributors
          output$project_observations <- details$project_observations
        },
        "party" = {
          shiny::incProgress(0.5, "Processing party details")
          details <- build_party_details_view(result)
          output$party_name <- details$party_name
          output$party_organization <- details$party_organization
          output$party_contact <- details$party_contact
          output$party_projects <- details$party_projects
        },
        "community-classification" = {
          shiny::incProgress(0.5, "Processing community classification details")
          details <- build_comm_class_details_view(result)
          output$observation_details <- details$observation_details
          output$community_interpretation <- details$community_interpretation
        },
        "community-concept" = {
          shiny::incProgress(0.5, "Processing community details")
          details <- build_comm_concept_details_view(result)
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
        },
        "plant-concept" = {
          shiny::incProgress(0.5, "Processing plant concept details")
          details <- build_plant_concept_details_view(result)
          output$plant_concept_name <- details$plant_concept_name
          output$plant_concept_details <- details$plant_concept_details
          output$plant_party_perspective <- details$plant_party_perspective
          output$plant_aliases <- details$plant_aliases
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

#' Build Community Classifiaction Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed community classification information.
#'
#' @param result A data frame returned by vegbankr::get_community_classification()
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#' @importFrom tidyr pivot_wider
#' @noRd
build_comm_class_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(list(
      observation_details = shiny::renderUI({
        htmltools::tags$p("Community details not available")
      }),
      community_interpretation = shiny::renderUI({
        htmltools::tags$p("No description available")
      })
    ))
  }

  list(
    observation_details = shiny::renderUI({
      safe_render_details(
        c("comm_class_accession_code", "inspection", "table_analysis", "multivariate_analysis"),
        result
      )
    }),
    community_interpretation = shiny::renderUI({
      safe_render_details(
        c("comm_concept_id", "class_fit", "class_confidence", "comm_authority_id", "type"),
        result
      )
    })
  )
}

#' Build Community Concept Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed community concept information.
#'
#' @param result A data frame returned by vegbankr::get_community_concept()
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#' @importFrom tidyr pivot_wider
#' @noRd
build_comm_concept_details_view <- function(result) {
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
      htmltools::tags$p(scientific_class$comm_name)
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

#' Build Project Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed project information.
#'
#' @param result A data frame returned by vegbankr::get_project()
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#' @importFrom tidyr pivot_wider
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
      # TODO: Add classified plots
      htmltools::tags$p(
        "Number of observations: ",
        htmltools::tags$strong(result$obs_count)
      )
    }),
    project_description = shiny::renderUI({
      # The description contains HTML entities <i></i> that need to be properly rendered
      htmltools::tags$div(
        id = "project-description",
        htmltools::HTML(result$project_description)
      )
    }),
    project_contributors = shiny::renderUI({
      # TODO: Handle contributors
      htmltools::tags$p("No contributors available")
    }),
    project_dates = safe_render_details(
      c("start_date", "stop_date", "last_plot_added_date"),
      result
    )
  )
}

#' Build Taxon Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed taxon information.
#'
#' @param result A data frame returned by vegbankr::get_taxon_observation_details()
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

#' Build Party Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed party information.
#'
#' @param result A data frame returned by vegbankr::get_party()
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#' @noRd
build_party_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(list(
      party_name = shiny::renderUI({
        htmltools::tags$p("Party details not available")
      }),
      party_organization = shiny::renderUI({
        htmltools::tags$p("No organization available")
      }),
      party_contact = shiny::renderUI({
        htmltools::tags$p("No contact information available")
      }),
      party_projects = shiny::renderUI({
        htmltools::tags$p("No projects available")
      })
    ))
  }

  # Format full name based on available parts
  full_name <- paste(
    ifelse(is.na(result$salutation), "", paste0(result$salutation, " ")),
    ifelse(is.na(result$given_name), "", result$given_name),
    ifelse(is.na(result$middle_name), "", paste0(" ", result$middle_name)),
    ifelse(is.na(result$surname), "", paste0(" ", result$surname))
  )

  full_name <- trimws(full_name)
  if (full_name == "") {
    if (!is.na(result$organization_name)) {
      full_name <- result$organization_name
    } else {
      full_name <- "Unknown Name"
    }
  }

  list(
    party_name = shiny::renderUI({
      htmltools::tags$p(full_name)
    }),
    party_organization = shiny::renderUI({
      if (is.na(result$organization_name)) {
        htmltools::tags$p("No organization specified")
      } else {
        htmltools::tags$p(result$organization_name)
      }
    }),
    party_contact = safe_render_details(
      c("contact_instructions"),
      result
    ),
    party_projects = shiny::renderUI({
      # Note: This is a placeholder. In a real implementation, you would fetch
      # the projects associated with this party from the API
      htmltools::tags$p("Associated projects would be displayed here")
    })
  )
}


#' Coalesce Function for NULL or NA
#' Returns the first argument if it is not NULL, NA, "NA" or ""; otherwise returns the second argument
#' @param a First value to check
#' @param b Second value to return if the first is NULL, NA, or empty string
#' @return The first value if valid, otherwise the second value
#' @noRd
`%|||%` <- function(a, b) {
  # Handle NULL case first
  if (is.null(a)) {
    return(b)
  }

  # Handle vectors by taking the first element for comparison
  if (length(a) > 1) {
    a <- a[1]
  }

  # Check for NA, empty string, or "NA" string
  if (is.na(a) || a == "" || a == "NA") {
    return(b)
  }

  a
}


#' Build Plant Concept Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed plant concept information.
#'
#' @param result A data frame returned by get_plant_concept_mock()
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#' @importFrom jsonlite fromJSON
#' @noRd
build_plant_concept_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(list(
      plant_concept_name = shiny::renderUI({
        htmltools::tags$p("Plant concept details not available")
      }),
      plant_concept_details = shiny::renderUI({
        htmltools::tags$p("No concept details available")
      }),
      plant_party_perspective = shiny::renderUI({
        htmltools::tags$p("No party perspective available")
      }),
      plant_aliases = shiny::renderUI({
        htmltools::tags$p("No aliases available")
      })
    ))
  }

  list(
    plant_concept_name = shiny::renderUI({
      htmltools::div(
        htmltools::tags$i(result$plant_level %|||% "Unspecified level"),
        htmltools::tags$h5(
          result$plant_name,
          if (!is.na(result$plant_code)) {
            htmltools::tags$span(
              style = "color: gray;",
              paste0("(", result$plant_code, ")")
            )
          }
        ),
      )
    }),
    plant_concept_details = shiny::renderUI({
      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        htmltools::tags$tbody(
          htmltools::tags$tr(
            htmltools::tags$td("Plant Code"),
            htmltools::tags$td(class = "text-end", result$pc_code)
          ),
          htmltools::tags$tr(
            htmltools::tags$td("Reference"),
            htmltools::tags$td(class = "text-end", result$concept_rf_name %|||% "Not specified")
          ),
          htmltools::tags$tr(
            htmltools::tags$td("Observation Count"),
            htmltools::tags$td(class = "text-end", result$obs_count %|||% "0")
          )
        )
      )
    }),
    plant_party_perspective = create_party_perspective_ui(result),
    plant_aliases = create_plant_aliases_ui(result)
  )
}

#' Create Party Perspective UI for Plant Concept
#'
#' @param result Plant concept data frame row
#' @return A shiny::renderUI function
#' @noRd
create_party_perspective_ui <- function(result) {
  shiny::renderUI({
    # Parse children JSON if it exists and is not NA
    children_links <- NULL
    if (!is.na(result$children) && !is.null(result$children) && result$children != "") {
      tryCatch(
        {
          children_data <- jsonlite::fromJSON(result$children)
          if (length(children_data) > 0) {
            children_links <- lapply(names(children_data), function(child_code) {
              htmltools::tags$li(
                htmltools::tags$a(
                  href = "#",
                  onclick = sprintf(
                    "Shiny.setInputValue('plant_link_click', '%s', {priority: 'event'}); return false;",
                    child_code
                  ),
                  children_data[[child_code]]
                )
              )
            })
          }
        },
        error = function(e) {
          # If JSON parsing fails, show the raw children data
          children_links <- htmltools::tags$p(result$children)
        }
      )
    }

    htmltools::tags$table(
      class = "table table-sm table-striped table-hover",
      htmltools::tags$tbody(
        htmltools::tags$tr(
          htmltools::tags$td("Party"),
          htmltools::tags$td(class = "text-end", result$party %|||% "Not specified")
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Start Date"),
          htmltools::tags$td(class = "text-end", if (!is.na(result$start_date)) format(as.Date(result$start_date), "%Y-%m-%d") else "Not specified")
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Stop Date"),
          htmltools::tags$td(class = "text-end", if (!is.na(result$stop_date)) format(as.Date(result$stop_date), "%Y-%m-%d") else "Not specified")
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Status"),
          htmltools::tags$td(class = "text-end", result$status %|||% "Not specified")
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Parent"),
          htmltools::tags$td(
            class = "text-end",
            if (!is.na(result$parent_name)) {
              htmltools::tags$a(
                href = "#",
                onclick = sprintf(
                  "Shiny.setInputValue('plant_link_click', '%s', {priority: 'event'}); return false;",
                  result$parent_pc_code
                ),
                result$parent_name
              )
            } else {
              "None"
            }
          )
        ),
        if (!is.null(children_links)) {
          htmltools::tags$tr(
            htmltools::tags$td("Children"),
            htmltools::tags$td(class = "text-end", htmltools::tags$ul(children_links))
          )
        } else {
          htmltools::tags$tr(
            htmltools::tags$td("Children"),
            htmltools::tags$td(class = "text-end", "None")
          )
        }
      )
    )
  })
}

#' Create Plant Aliases UI
#'
#' @param result Plant concept data frame row
#' @return A shiny::renderUI function
#' @noRd
create_plant_aliases_ui <- function(result) {
  shiny::renderUI({
    # Parse usage_names JSON if it exists
    aliases_content <- NULL
    if (!is.na(result$usage_names) && !is.null(result$usage_names) && result$usage_names != "") {
      tryCatch(
        {
          usage_names_data <- jsonlite::fromJSON(result$usage_names)
          if (length(usage_names_data) > 0) {
            aliases_rows <- lapply(names(usage_names_data), function(usage_type) {
              htmltools::tags$tr(
                htmltools::tags$td(usage_type),
                htmltools::tags$td(class = "text-end", usage_names_data[[usage_type]])
              )
            })
            aliases_content <- htmltools::tags$table(
              class = "table table-sm table-striped table-hover",
              htmltools::tags$tbody(aliases_rows)
            )
          }
        },
        error = function(e) {
          # If JSON parsing fails, show the raw usage_names data
          aliases_content <- htmltools::tags$p(result$usage_names)
        }
      )
    }

    if (is.null(aliases_content)) {
      htmltools::tags$p("No aliases available")
    } else {
      aliases_content
    }
  })
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
  file_path <- system.file("shiny/www/display_name_lookup.txt", package = "vegbankweb")

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

#' Safely Render Detail Fields in a Shiny UI
#'
#' This function creates a Shiny renderUI output that displays data fields in a formatted table.
#' It handles missing values, validates field existence, and formats logical values for display.
#'
#' @param fields A character vector of field names to display from the dataframe
#' @param dataframe A dataframe containing the data to display
#'
#' @return A shiny::renderUI function that generates an HTML table with field names and values.
#'         Returns a message if no valid fields are found.
#' @noRd
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

#' Create an HTML Table for Detail View
#'
#' This function generates an HTML table to display field name/value pairs in a
#' formatted two-column table.
#'
#' @param details A named list of values to display
#' @param col_names A named list mapping internal column names to display names
#'
#' @return An htmltools table object with styled rows and columns
#' @noRd
create_detail_table <- function(details, col_names) {
  htmltools::tags$table(
    class = "table table-sm table-striped table-hover",
    htmltools::tags$tbody(
      lapply(names(details), function(name) {
        display_name <- if (name %in% names(col_names)) col_names[[name]] else name
        htmltools::tags$tr(
          htmltools::tags$td(display_name),
          htmltools::tags$td(class = "text-end", details[[name]])
        )
      })
    )
  )
}
