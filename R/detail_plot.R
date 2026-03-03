#' @noRd
plot_detail_output_names <- c(
  "plot_notification", "plot_header",
  "author_code_details", "date_details", "location_details", "layout_details",
  "environmental_details", "methods_details", "plot_quality_details", "plot_vegetation_details",
  "communities_details", "taxa_details", "disturbances_details", "soils_details", "plot_misc_details"
)

#' Normalize Plot Observation Result Payload
#'
#' Transforms the API payload for plot observations into a standardized structure
#' with separate components for plot metadata, taxa observations, and community
#' classifications. Extracts nested list-columns and removes them from the main
#' observation data frame.
#'
#' @param result A single-row data frame from vegbankr containing nested
#'   `top_taxon_observations` and `top_classifications` list-columns.
#' @return A list with four elements:
#'   - plot_observation = Data frame of plot-level metadata with nested
#'     columns removed
#'   - top_taxon_observations = Data frame of taxon observations
#'   - communities = Data frame of community classifications
#'   - has_data = Logical indicating whether valid data was found
#' @noRd
normalize_plot_obs_result <- function(result) {
  empty <- list(
    plot_observation = data.frame(),
    top_taxon_observations = data.frame(),
    communities = data.frame(),
    has_data = FALSE
  )

  if (is.null(result)) {
    return(empty)
  }

  if (!is.data.frame(result) || nrow(result) == 0) {
    return(empty)
  }

  plot_observation <- result[1, , drop = FALSE]
  taxa <- extract_nested_table(plot_observation, "top_taxon_observations")
  communities <- extract_nested_table(plot_observation, "top_classifications")
  disturbances <- extract_nested_table(plot_observation, "disturbances")
  soils <- extract_nested_table(plot_observation, "soils")

  nested_cols <- intersect(
    c("top_taxon_observations", "top_classifications", "disturbances", "soils"), names(plot_observation)
  )

  if (length(nested_cols) > 0) {
    plot_observation[nested_cols] <- NULL
  }

  list(
    plot_observation = plot_observation,
    top_taxon_observations = taxa,
    communities = communities,
    disturbances = disturbances,
    soils = soils,
    has_data = TRUE
  )
}

#' Prepare Taxa Data for Display
#'
#' Processes raw taxon observation data by normalizing stratum labels, sorting
#' by stratum group and then descending by cover, and formatting cover values for display.
#' Taxa with missing stratum are labeled as "Unspecified" and those with a stratum of "-all-"
#' are placed at the end. Cover values are rounded to two decimal places and suffixed with "%".
#'
#' @param taxa A data frame of taxon observations containing `stratum_name` and
#'   `cover` columns (these columns are added as NA if missing).
#' @return The input data frame with additional columns:
#'     - stratum_label = Normalized stratum name ("Unspecified" for missing values)
#'     - cover_numeric = Numeric cover value
#'     - cover_numeric_order = Cover value for sorting (missing values as -Inf)
#'     - cover_display = Formatted cover string (e.g., "25.40%") or "" for missing values
#' @noRd
prepare_taxa_display <- function(taxa) {
  if (is.null(taxa) || nrow(taxa) == 0) {
    return(data.frame())
  }

  if (!"stratum_name" %in% names(taxa)) {
    taxa$stratum_name <- NA_character_
  }
  if (!"cover" %in% names(taxa)) {
    taxa$cover <- NA_real_
  }

  stratum_values <- as.character(taxa$stratum_name)
  stratum_values[is.na(stratum_values)] <- ""
  stratum_values <- trimws(stratum_values)
  taxa$stratum_label <- ifelse(stratum_values == "", "Unspecified", stratum_values)

  taxa$cover_numeric <- suppressWarnings(as.numeric(taxa$cover))
  taxa$cover_numeric_order <- ifelse(is.na(taxa$cover_numeric), -Inf, taxa$cover_numeric)

  stratum_levels <- unique(taxa$stratum_label)
  stratum_levels <- stratum_levels[stratum_levels != "-all-"]
  if (any(taxa$stratum_label == "-all-")) {
    stratum_levels <- c(stratum_levels, "-all-")
  }

  order_index <- order(match(taxa$stratum_label, stratum_levels), -taxa$cover_numeric_order)
  sorted_taxa <- taxa[order_index, , drop = FALSE]

  sorted_taxa$cover_display <- ifelse(
    !is.na(sorted_taxa$cover_numeric),
    sprintf("%.2f%%", sorted_taxa$cover_numeric),
    ""
  )

  sorted_taxa
}

#' Build Plot Observation Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed plot observation information.
#'
#' @param result A single-row dataframe returned by vegbankr with nested
#'   `top_taxon_observations` and `top_classifications` list-columns.
#' @return A list of Shiny UI outputs.
#'
#' @importFrom htmltools tags
#' @importFrom shiny renderUI
#' @noRd
build_plot_obs_details_view <- function(result) {
  normalized <- normalize_plot_obs_result(result)

  if (!normalized$has_data) {
    return(create_empty_detail_view(plot_detail_output_names, "Plot observation details"))
  }

  plot_observation <- normalized$plot_observation

  ### Compute formatted fields for use in detail tables

  # Date range for date_details card
  plot_observation$observed_date <- format_date_range(
    plot_observation$obs_start_date,
    plot_observation$obs_end_date,
    worded = FALSE
  )
  # Format entered date for date_details card
  plot_observation$date_entered <- format_date(plot_observation$date_entered)

  # Format lat and lng to 7 dec place for location_details card
  if (has_valid_field_value(plot_observation, "latitude")) {
    plot_observation$latitude <- formatC(plot_observation$latitude, format = "f", digits = 7)
  }
  if (has_valid_field_value(plot_observation, "longitude")) {
    plot_observation$longitude <- formatC(plot_observation$longitude, format = "f", digits = 7)
  }

  # Elevation to two decimal places for environmental_details card
  if (has_valid_field_value(plot_observation, "elevation")) {
    plot_observation$elevation <- formatC(as.numeric(plot_observation$elevation), format = "f", digits = 2)
  }

  # Cover method display (link or fallback) for methods_details card
  # Must use list() to wrap HTML tag objects for tibble compatibility
  if (has_valid_field_value(plot_observation, "cm_code") &&
        has_valid_field_value(plot_observation, "cover_method_name")) {
    plot_observation$cover_method_display <- list(create_detail_link(
      "cover_method_link_click",
      plot_observation$cm_code,
      plot_observation$cover_method_name
    ))
  } else {
    plot_observation$cover_method_display <- NA_character_
  }

  # Stratum method display (link or fallback) for methods_details card
  # Must use list() to wrap HTML tag objects for tibble compatibility
  if (has_valid_field_value(plot_observation, "sm_code") &&
        has_valid_field_value(plot_observation, "stratum_method_name")) {
    plot_observation$stratum_method_display <- list(create_detail_link(
      "stratum_method_link_click",
      plot_observation$sm_code,
      plot_observation$stratum_method_name
    ))
  } else {
    plot_observation$stratum_method_display <- NA_character_
  }

  taxa_details_ui <- shiny::renderUI({
    tryCatch(
      {
        taxa <- normalized$top_taxon_observations
        if (is.null(taxa) || nrow(taxa) == 0) {
          return(htmltools::tags$p("No taxa recorded"))
        }

        sorted_taxa <- prepare_taxa_display(taxa)

        rows <- lapply(seq_len(nrow(sorted_taxa)), function(i) {
          row <- sorted_taxa[i, , drop = FALSE]

          htmltools::tags$tr(
            htmltools::tags$td(create_detail_link("plant_link_click", row$pc_code, row$plant_name)),
            htmltools::tags$td(row$stratum_label),
            htmltools::tags$td(style = "text-align: right;", row$cover_display)
          )
        })

        num_strata <- length(unique(sorted_taxa$stratum_label))

        htmltools::div(
          htmltools::tags$p(
            htmltools::tags$strong(plot_observation$taxon_count), " taxa observed across ",
            htmltools::tags$strong(num_strata), " strata."
          ),
          create_detail_table_with_headers(
            c("Name", "Stratum", "Cover"),
            rows,
            table_style = "width: 100%; table-layout: fixed; word-break: break-word;",
            header_styles = c("width: 40%; text-align: left;",
                              "width: 35%; text-align: left;",
                              "width: 25%; text-align: right;")
          )
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
        communities <- normalized$communities
        if (is.null(communities) || nrow(communities) == 0) {
          return(htmltools::tags$p("No communities recorded"))
        }

        rows <- lapply(seq_len(nrow(communities)), function(i) {
          row <- communities[i, ]
          htmltools::tags$tr(
            htmltools::tags$td(
              create_detail_link("comm_class_link_click", row$cl_code, row$comm_name)
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

  disturbances_details_ui <- shiny::renderUI({
    tryCatch(
      {
        disturbances <- normalized$disturbances

        if (is.null(disturbances) || nrow(disturbances) == 0) {
          return(htmltools::tags$p("No disturbances recorded"))
        }

        rows <- lapply(seq_len(nrow(disturbances)), function(i) {
          row <- disturbances[i, , drop = FALSE]

          type_val <- row$type %|||% ""
          intensity_val <- row$intensity %|||% ""
          comment_val <- row$comment %|||% ""

          htmltools::tags$tr(
            htmltools::tags$td(type_val),
            htmltools::tags$td(intensity_val),
            htmltools::tags$td(comment_val)
          )
        })

        create_detail_table_with_headers(
          c("Type", "Intensity", "Comment"),
          rows,
          table_style = "width: 100%; table-layout: fixed; word-break: break-word;",
          header_styles = c("width: 30%;", "width: 30%;", "width: 40%;")
        )
      },
      error = function(e) {
        message("Error in disturbances_details: ", e$message)
        htmltools::tags$p(paste("Error processing disturbances:", e$message))
      }
    )
  })

  soils_details_ui <- shiny::renderUI({
    tryCatch(
      {
        soils <- normalized$soils

        if (is.null(soils) || nrow(soils) == 0) {
          return(htmltools::tags$p("No soil data recorded"))
        }

        soil_sections <- lapply(seq_len(nrow(soils)), function(i) {
          soil <- soils[i, , drop = FALSE]
          horizon_label <- soil$horizon %|||% "Unspecified"

          # Get all non-null fields for this soil
          fields_to_show <- c(
            "description", "depth_top", "depth_bottom", "texture", "color",
            "ph", "organic", "sand", "silt", "clay", "coarse",
            "exchange_capacity", "base_saturation"
          )

          htmltools::div(
            style = "margin-bottom: 1rem;",
            create_section_header(paste("Horizon:", horizon_label)),
            format_fields_for_detail_table(
              fields_to_show,
              soil,
              skip_empty = TRUE,
              apply_units = TRUE
            )
          )
        })

        htmltools::div(soil_sections)
      },
      error = function(e) {
        message("Error in soils_details: ", e$message)
        htmltools::tags$p(paste("Error processing soils:", e$message))
      }
    )
  })

  list(
    plot_notification = shiny::renderUI({
      # Show notification if observation has been replaced
      if (isTRUE(plot_observation$has_observation_synonym) &&
            has_valid_field_value(plot_observation, "replaced_by_ob_code")) {
        return(htmltools::tags$div(
          class = "alert alert-warning",
          style = "padding: 0.5rem; margin-bottom: 0.75rem; font-size: 0.875rem;",
          htmltools::tags$b("This observation has been updated."),
          htmltools::tags$br(),
          create_detail_link(
            "plot_link_click",
            plot_observation$replaced_by_ob_code,
            "View the newer version."
          )
        ))
      }
      return(NULL)
    }),
    plot_header = shiny::renderUI({
      htmltools::div(
        htmltools::tags$h5(plot_observation$author_obs_code, style = "font-weight: 600; margin-bottom: 0px;"),
        htmltools::tags$h5(plot_observation$ob_code, style = "color: var(--vb-green); font-weight: 600;"),
        if (has_valid_field_value(plot_observation, "project_name")) {
          htmltools::tags$p(
            htmltools::tags$span("Project: "),
            create_detail_link("proj_link_click", plot_observation$pj_code, plot_observation$project_name),
            style = "margin-bottom: 0px;"
          )
        },
        if (has_valid_field_value(plot_observation, "obs_start_date")) {
          htmltools::tags$p(format_date_range(plot_observation$obs_start_date, plot_observation$obs_end_date))
        },
        if (has_valid_field_value(plot_observation, "rf_code") &&
              has_valid_field_value(plot_observation, "rf_label")) {
          htmltools::tags$p(
            htmltools::tags$span("Reference: "),
            create_detail_link("ref_link_click", plot_observation$rf_code, plot_observation$rf_label)
          )
        },
        if (has_valid_field_value(plot_observation, "previous_ob_code")) {
          htmltools::tags$p(
            htmltools::tags$span("Previous Observation: "),
            create_detail_link(
              "obs_link_click",
              plot_observation$previous_ob_code,
              plot_observation$previous_ob_code
            )
          )
        }
      )
    }),
    author_code_details = render_detail_table(
      c("author_obs_code", "author_plot_code", "author_datum"),
      plot_observation,
      skip_empty = TRUE
    ),
    date_details = render_detail_table(
      c("observed_date", "date_entered", "date_accuracy"),
      plot_observation,
      skip_empty = TRUE
    ),
    location_details = render_detail_table(
      c(
        "location_accuracy", "confidentiality_text", "latitude", "longitude", "author_location",
        "location_narrative", "state_province", "country"
      ),
      plot_observation,
      skip_empty = TRUE,
      apply_units = TRUE
    ),
    layout_details = render_detail_table(
      c("azimuth", "shape", "area", "permanence", "layout_narrative"),
      plot_observation,
      skip_empty = TRUE,
      apply_units = TRUE
    ),
    environmental_details = render_detail_table(
      c(
        "landscape_narrative", "homogeneity", "phenologic_aspect", "representativeness", "stand_maturity",
        "successional_status", "hydrologic_regime", "soil_moisture_regime", "soil_drainage", "water_salinity",
        "water_depth", "soil_depth", "organic_depth", "soil_taxon_src", "percent_bed_rock",
        "percent_rock_gravel", "percent_wood", "percent_litter", "percent_bare_soil", "percent_water",
        "percent_other", "name_other", "stand_size", "elevation", "elevation_accuracy", "elevation_range",
        "slope_aspect", "min_slope_aspect", "max_slope_aspect", "slope_gradient", "min_slope_gradient",
        "max_slope_gradient", "topographic_position", "landform", "surficial_deposits", "rock_type"
      ),
      plot_observation,
      skip_empty = TRUE,
      apply_units = TRUE
    ),
    methods_details = render_detail_table(
      c(
        "method_narrative", "placement_method", "cover_method_display", "cover_dispersion",
        "stratum_method_display", "stem_sample_method", "stem_observation_area", "stem_size_limit",
        "taxon_observation_area", "auto_taxon_cover"
      ),
      plot_observation,
      skip_empty = TRUE,
      apply_units = TRUE
    ),
    plot_quality_details = render_detail_table(
      c(
        "plot_validation_level_descr", "effort_level", "floristic_quality", "bryophyte_quality",
        "lichen_quality", "observation_narrative"
      ),
      plot_observation,
      skip_empty = TRUE
    ),
    plot_vegetation_details = render_detail_table(
      c(
        "basal_area", "tree_ht", "shrub_ht", "herb_ht", "field_ht", "nonvascular_ht",
        "submerged_ht", "tree_cover", "shrub_cover", "field_cover", "nonvascular_cover", "floating_cover",
        "submerged_cover", "dominant_stratum", "stratum_assignment", "growthform_1_type",
        "growthform_1_cover", "growthform_2_type", "growthform_2_cover", "growthform_3_type",
        "growthform_3_cover", "growthform_4_type", "growthform_4_cover", "total_cover"
      ),
      plot_observation,
      skip_empty = TRUE,
      apply_units = TRUE
    ),
    communities_details = communities_details_ui,
    taxa_details = taxa_details_ui,
    disturbances_details = disturbances_details_ui,
    soils_details = soils_details_ui,
    plot_misc_details = render_detail_table(
      c("original_data", "parent_pl_code", "pl_revisions", "pl_notes_public", "pl_notes_mgt"),
      plot_observation,
      skip_empty = TRUE
    )
  )
}
