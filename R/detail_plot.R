#' @noRd
plot_detail_output_names <- c(
  "plot_id_details", "location_details", "layout_details",
  "environmental_details", "methods_details", "plot_quality_details",
  "taxa_details", "communities_details"
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

  nested_cols <- intersect(c("top_taxon_observations", "top_classifications"), names(plot_observation))
  if (length(nested_cols) > 0) {
    plot_observation[nested_cols] <- NULL
  }

  list(
    plot_observation = plot_observation,
    top_taxon_observations = taxa,
    communities = communities,
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
#'     - cover_display = Formatted cover string (e.g., "25.40%") or "Not recorded"
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
    "Not recorded"
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

  # Format Dates and Elevation
  plot_observation$obs_start_date <- format_date(plot_observation$obs_start_date)
  plot_observation$elevation <- paste0(round(as.numeric(plot_observation$elevation)), " m")

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

        create_detail_table_with_headers(
          c("Name", "Stratum", "Cover"),
          rows,
          table_style = "width: 100%; table-layout: fixed; word-break: break-word;",
          header_styles = c("text-align: left;", "text-align: left;", "text-align: right;")
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

  list(
    plot_header = shiny::renderUI({
      htmltools::div(
        htmltools::tags$h5(plot_observation$author_obs_code, style = "font-weight: 600; margin-bottom: 0px;"),
        htmltools::tags$h5(plot_observation$ob_code, style = "color: #2c5443; font-weight: 600;"),
        if (has_valid_field_value(plot_observation, "project_name")) {
          htmltools::tags$p(
            htmltools::tags$span("Project: "),
            create_detail_link("proj_link_click", plot_observation$pj_code, plot_observation$project_name),
            style = "margin-bottom: 0px;"
          )
        },
        if (has_valid_field_value(plot_observation, "obs_start_date")) {
          htmltools::tags$p(format_date_range(plot_observation$obs_start_date, plot_observation$obs_end_date))
        }
      )
    }),
    plot_id_details = render_detail_table(
      c("author_obs_code", "author_plot_code"),
      plot_observation
    ),
    location_details = render_detail_table(
      c("confidentiality_text", "latitude", "longitude", "location_narrative", "state_province", "country"),
      plot_observation
    ),
    layout_details = render_detail_table(
      c("area", "permanence"),
      plot_observation
    ),
    environmental_details = render_detail_table(
      c("elevation", "slope_aspect", "slope_gradient"),
      plot_observation
    ),
    methods_details = render_detail_table(
      c(
        "obs_start_date", "project_name", "cover_type", "stratum_method_name", "stratum_method_description",
        "taxon_observation_area", "auto_taxon_cover"
      ),
      plot_observation
    ),
    plot_quality_details = render_detail_table(
      "plot_validation_level_descr",
      plot_observation
    ),
    taxa_details = taxa_details_ui,
    communities_details = communities_details_ui
  )
}
