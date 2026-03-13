#' Output names for Taxon Observation detail view
#' @noRd
taxon_obs_detail_output_names <- c(
  "taxon_obs_header",
  "taxon_obs_interpretations",
  "taxon_obs_details",
  "taxon_obs_importance"
)

#' Build Taxon Observation Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed taxon
#' observation information. Requires both the taxon observation data and
#' its associated taxon interpretations.
#'
#' @param taxon_obs A data frame returned by vegbankr::vb_get_taxon_observations()
#'   Expected columns: to_code, ob_code, author_plant_name, taxon_inference_area,
#'   rf_code, rf_label, and nested list-column taxon_importance.
#' @param taxon_interps A data frame returned by vegbankr::vb_get_taxon_interpretations()
#'   Expected columns: pc_code, plant_label, py_code, party_label,
#'   interpretation_date, interpretation_type, is_orig, is_curr,
#'   taxon_fit, taxon_confidence, group_type.
#' @return A named list of Shiny UI outputs for each card section
#' @noRd
build_taxon_obs_details_view <- function(taxon_obs, taxon_interps) {
  if (is.null(taxon_obs) || nrow(taxon_obs) == 0) {
    return(create_empty_detail_view(taxon_obs_detail_output_names, "Taxon observation details"))
  }

  obs <- taxon_obs[1, , drop = FALSE]

  list(
    taxon_obs_header = create_taxon_obs_header_ui(obs),
    taxon_obs_interpretations = create_taxon_obs_interpretations_ui(taxon_interps),
    taxon_obs_details = create_taxon_obs_details_ui(obs, taxon_interps),
    taxon_obs_importance = create_taxon_obs_importance_ui(obs)
  )
}

#' Create Taxon Observation Header UI
#'
#' Displays the author plant name, to_code, and a link back to the parent
#' plot observation.
#'
#' @param obs Single-row data frame of the taxon observation
#' @return A shiny.render.function
#' @noRd
create_taxon_obs_header_ui <- function(obs) {
  shiny::renderUI({
    plant_name <- obs$author_plant_name %|||% "Unspecified"
    to_code <- obs$to_code %|||% "Unspecified"
    ob_code <- obs$ob_code %|||% NULL

    ob_link <- if (!is.null(ob_code) && !is.na(ob_code)) {
      create_detail_link("plot_link_click", ob_code, ob_code)
    } else {
      "Unspecified"
    }

    htmltools::div(
      htmltools::tags$h5(plant_name, style = "font-weight: 600; margin-bottom: 0px;"),
      htmltools::tags$h5(to_code, " on ", ob_link, style = "color: var(--vb-green); font-weight: 600;")
    )
  })
}

#' Create Taxon Interpretations UI
#'
#' Renders all taxon interpretations in chronological order, each section
#' separated by a horizontal rule.
#'
#' @param taxon_interps Data frame of taxon interpretations
#' @return A shiny.render.function
#' @noRd
create_taxon_obs_interpretations_ui <- function(taxon_interps) {
  shiny::renderUI({
    tryCatch(
      {
        if (is.null(taxon_interps) || nrow(taxon_interps) == 0) {
          return(htmltools::tags$p("No taxon interpretations recorded"))
        }

        # Sort chronologically by interpretation_date
        interps <- taxon_interps
        if ("interpretation_date" %in% names(interps)) {
          sorted_idx <- order(interps$interpretation_date, na.last = TRUE)
          interps <- interps[sorted_idx, , drop = FALSE]
        }

        n <- nrow(interps)

        rows <- lapply(seq_len(n), function(i) {
          interp <- interps[i, , drop = FALSE]

          # Plant concept link
          pc_code <- if ("pc_code" %in% names(interp)) interp$pc_code else NULL
          plant_label <- if ("plant_label" %in% names(interp)) interp$plant_label %|||% "Unspecified" else "Unspecified"
          plant_link <- if (!is.null(pc_code) && !is.na(pc_code)) {
            create_detail_link("plant_link_click", pc_code, plant_label)
          } else {
            plant_label
          }

          # Party link
          py_code <- if ("py_code" %in% names(interp)) interp$py_code else NULL
          party_label <- if ("party_label" %in% names(interp)) interp$party_label %|||% "Unspecified" else "Unspecified"
          party_link <- if (!is.null(py_code) && !is.na(py_code)) {
            create_detail_link("party_link_click", py_code, party_label)
          } else {
            party_label
          }

          interp_date <- tryCatch({
            date_val <- if ("interpretation_date" %in% names(interp)) interp$interpretation_date else NA
            if (is.null(date_val) || (length(date_val) > 0 && is.na(date_val[1]))) {
              "Unspecified"
            } else if (inherits(date_val, c("POSIXct", "POSIXlt", "Date"))) {
              format(as.Date(date_val), "%Y-%m-%d")
            } else {
              format_date(date_val)
            }
          }, error = function(e) "Unspecified")

          interp_type <- if ("interpretation_type" %in% names(interp)) interp$interpretation_type %|||% "Unspecified" else "Unspecified"
          is_orig <- if ("is_orig" %in% names(interp)) format_boolean(interp$is_orig) else "Unspecified"
          is_curr <- if ("is_curr" %in% names(interp)) format_boolean(interp$is_curr) else "Unspecified"
          taxon_fit <- if ("taxon_fit" %in% names(interp)) interp$taxon_fit %|||% "Unspecified" else "Unspecified"
          taxon_confidence <- if ("taxon_confidence" %in% names(interp)) interp$taxon_confidence %|||% "Unspecified" else "Unspecified"
          group_type <- if ("group_type" %in% names(interp)) interp$group_type %|||% "Unspecified" else "Unspecified"

          separator_style <- if (i < n) {
            "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #eee;"
          } else {
            ""
          }

          htmltools::tags$div(
            style = separator_style,
            htmltools::tags$div(style = "font-weight: 600;", plant_link),
            htmltools::tags$table(
              class = "table table-sm table-striped table-hover",
              style = "margin-top: 5px; margin-bottom: 0; width: 100%; table-layout: fixed; word-break: break-word;",
              htmltools::tags$tbody(
                htmltools::tags$tr(
                  htmltools::tags$td("Interpretation Date"),
                  htmltools::tags$td(class = "text-end", interp_date)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Party"),
                  htmltools::tags$td(class = "text-end", party_link)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Interpretation Type"),
                  htmltools::tags$td(class = "text-end", interp_type)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Original Interpretation"),
                  htmltools::tags$td(class = "text-end", is_orig)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Current Interpretation"),
                  htmltools::tags$td(class = "text-end", is_curr)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Taxon Fit"),
                  htmltools::tags$td(class = "text-end", taxon_fit)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Taxon Confidence"),
                  htmltools::tags$td(class = "text-end", taxon_confidence)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Group Type"),
                  htmltools::tags$td(class = "text-end", as.character(group_type))
                )
              )
            )
          )
        })

        htmltools::tags$div(rows)
      },
      error = function(e) {
        htmltools::tags$p(paste("Error processing interpretations:", e$message))
      }
    )
  })
}

#' Create Taxon Observation Details UI
#'
#' Renders a summary table with inference area, original interpretation,
#' current interpretation, and reference.
#'
#' @param obs Single-row taxon observation data frame
#' @param taxon_interps Data frame of taxon interpretations
#' @return A shiny.render.function
#' @noRd
create_taxon_obs_details_ui <- function(obs, taxon_interps) {
  shiny::renderUI({
    # Taxon inference area with m² unit
    inference_area_val <- obs$taxon_inference_area %|||% NA
    inference_area_display <- if (!is.na(inference_area_val)) {
      paste0(inference_area_val, " m\u00B2")
    } else {
      "Unspecified"
    }

    # Original and current interpretation links from interpretations data
    orig_link <- "Unspecified"
    curr_link <- "Unspecified"

    if (!is.null(taxon_interps) && nrow(taxon_interps) > 0) {
      if ("is_orig" %in% names(taxon_interps)) {
        orig_rows <- taxon_interps[!is.na(taxon_interps$is_orig) & taxon_interps$is_orig == TRUE, , drop = FALSE]
        if (nrow(orig_rows) > 0) {
          pc_orig <- orig_rows$pc_code[1] %|||% NULL
          label_orig <- if ("plant_label" %in% names(orig_rows)) orig_rows$plant_label[1] %|||% "Unspecified" else "Unspecified"
          orig_link <- if (!is.null(pc_orig) && !is.na(pc_orig)) {
            create_detail_link("plant_link_click", pc_orig, label_orig)
          } else {
            label_orig
          }
        }
      }

      if ("is_curr" %in% names(taxon_interps)) {
        curr_rows <- taxon_interps[!is.na(taxon_interps$is_curr) & taxon_interps$is_curr == TRUE, , drop = FALSE]
        if (nrow(curr_rows) > 0) {
          pc_curr <- curr_rows$pc_code[1] %|||% NULL
          label_curr <- if ("plant_label" %in% names(curr_rows)) curr_rows$plant_label[1] %|||% "Unspecified" else "Unspecified"
          curr_link <- if (!is.null(pc_curr) && !is.na(pc_curr)) {
            create_detail_link("plant_link_click", pc_curr, label_curr)
          } else {
            label_curr
          }
        }
      }
    }

    # Reference link
    rf_code_val <- if (has_valid_field_value(obs, "rf_code")) obs$rf_code else NA
    rf_label_val <- if (has_valid_field_value(obs, "rf_label")) obs$rf_label else NA
    ref_display <- if (!is.na(rf_code_val) && !is.na(rf_label_val)) {
      rf_code_str <- if (is.numeric(rf_code_val)) {
        paste0("rf.", as.integer(rf_code_val))
      } else {
        as.character(rf_code_val)
      }
      create_detail_link("ref_link_click", rf_code_str, rf_label_val)
    } else if (!is.na(rf_label_val)) {
      rf_label_val
    } else {
      "Unspecified"
    }

    details <- list(
      taxon_inference_area = inference_area_display,
      original_interpretation = orig_link,
      current_interpretation = curr_link,
      reference = ref_display
    )

    col_names <- c(
      taxon_inference_area = "Taxon Inference Area",
      original_interpretation = "Original Interpretation",
      current_interpretation = "Current Interpretation",
      reference = "Reference"
    )

    create_detail_table(details, col_names = col_names)
  })
}

#' Create Taxon Importance UI
#'
#' Renders a multi-row table of taxon importance records with stratum,
#' cover, cover code, basal area, inference area, and biomass.
#'
#' @param obs Single-row taxon observation data frame containing taxon_importance list-column
#' @return A shiny.render.function
#' @noRd
create_taxon_obs_importance_ui <- function(obs) {
  shiny::renderUI({
    tryCatch(
      {
        importance <- extract_nested_table(obs, "taxon_importance")

        if (is.null(importance) || nrow(importance) == 0) {
          return(htmltools::tags$p("No taxon importance data recorded"))
        }

        rows <- lapply(seq_len(nrow(importance)), function(i) {
          imp <- importance[i, , drop = FALSE]

          stratum_name <- if ("stratum_name" %in% names(imp)) imp$stratum_name %|||% "Unspecified" else "Unspecified"

          cover_val <- if ("cover" %in% names(imp)) imp$cover else NA
          cover_display <- if (!is.null(cover_val) && !is.na(cover_val) && is.numeric(cover_val)) {
            sprintf("%.2f%%", cover_val)
          } else {
            "Unspecified"
          }

          cover_code <- if ("cover_code" %in% names(imp)) imp$cover_code %|||% "Unspecified" else "Unspecified"
          basal_area <- if ("basal_area" %in% names(imp)) imp$basal_area %|||% "Unspecified" else "Unspecified"
          inference_area <- if ("inference_area" %in% names(imp)) imp$inference_area %|||% "Unspecified" else "Unspecified"
          biomass <- if ("biomass" %in% names(imp)) imp$biomass %|||% "Unspecified" else "Unspecified"

          htmltools::tags$tr(
            htmltools::tags$td(as.character(stratum_name)),
            htmltools::tags$td(cover_display),
            htmltools::tags$td(as.character(cover_code)),
            htmltools::tags$td(as.character(basal_area)),
            htmltools::tags$td(as.character(inference_area)),
            htmltools::tags$td(as.character(biomass))
          )
        })

        create_detail_table_with_headers(
          c("Stratum", "Cover", "Cover Code", "Basal Area", "Inference Area", "Biomass"),
          rows,
          table_style = "width: 100%; table-layout: fixed; word-break: break-word;"
        )
      },
      error = function(e) {
        htmltools::tags$p(paste("Error processing taxon importance data:", e$message))
      }
    )
  })
}
