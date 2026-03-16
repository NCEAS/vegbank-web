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

        # Sort reverse-chronologically by interpretation_date (most recent first)
        interps <- taxon_interps
        if ("interpretation_date" %in% names(interps)) {
          sorted_idx <- order(interps$interpretation_date, decreasing = TRUE, na.last = TRUE)
          interps <- interps[sorted_idx, , drop = FALSE]
        }

        n <- nrow(interps)

        rows <- lapply(seq_len(n), function(i) {
          interp <- interps[i, , drop = FALSE]

          # Plant concept link
          pc_code <- interp$pc_code %|||% NULL
          plant_label <- interp$plant_label %|||% "Unspecified"
          plant_link <- if (!is.null(pc_code) && !is.na(pc_code)) {
            create_detail_link("plant_link_click", pc_code, plant_label)
          } else {
            plant_label
          }

          # Party link
          py_code <- interp$py_code %|||% NULL
          party_label <- interp$party_label %|||% "Unspecified"
          party_link <- if (!is.null(py_code) && !is.na(py_code)) {
            create_detail_link("party_link_click", py_code, party_label)
          } else {
            party_label
          }
          party_role <- interp$role %|||% "Unspecified"

          interp_date <- tryCatch({
            date_val <- interp$interpretation_date %|||% NA
            if (is.null(date_val) || (length(date_val) > 0 && is.na(date_val[1]))) {
              "Unspecified"
            } else if (inherits(date_val, c("POSIXct", "POSIXlt", "Date"))) {
              format(as.Date(date_val), "%Y-%m-%d")
            } else {
              format_date(date_val)
            }
          }, error = function(e) "Unspecified")

          interp_type <- interp$interpretation_type %|||% "Unspecified"
          is_author <- interp_type == "Author" || interp_type == "author"
          interp_type_display <- if (is_author) "Authored" else cap_first(interp_type)
          is_orig <- isTRUE(interp$is_orig)
          is_curr <- isTRUE(interp$is_curr)
          taxon_fit <- interp$taxon_fit %|||% "Unspecified"
          taxon_confidence <- interp$taxon_confidence %|||% "Unspecified"
          group_type <- interp$group_type %|||% "Unspecified"

          separator_style <- if (i < n) {
            "margin-bottom: 16px; padding-bottom: 16px; border-bottom: 1px solid var(--bs-border-color);"
          } else {
            ""
          }

          # Badges for original / current flags
          badges <- list(
            if (is_orig) htmltools::tags$span(
              class = "badge rounded-pill me-1",
              style = "background-color: var(--no-status-bg); color: var(--no-status-text);",
              "Original"
            ),
            if (is_curr) htmltools::tags$span(
              class = "badge rounded-pill me-1",
              style = "background-color: var(--accepted-bg); color: var(--accepted-text);",
              "Current"
            )
          )
          badges <- Filter(Negate(is.null), badges)

          # Prose subtitle: "Type by Party" then "On date"
          subtitle_by <- if (interp_type_display != "Unspecified" && party_label != "Unspecified") {
            htmltools::tags$div(
              htmltools::HTML(paste0(htmltools::htmlEscape(interp_type_display), " by ")),
              party_link
            )
          } else if (party_label != "Unspecified") {
            htmltools::tags$div("By ", party_link)
          } else if (interp_type_display != "Unspecified") {
            htmltools::tags$div(interp_type_display)
          } else {
            NULL
          }

          subtitle_role <- if (!is_author && party_role != "Unspecified") {
            htmltools::tags$div(
              paste("As", party_role)
            )
          } else {
            NULL
          }

          subtitle_date <- if (interp_date != "Unspecified") {
            htmltools::tags$div(
              paste("On", interp_date)
            )
          } else {
            NULL
          }

          # Small table for only the detail fields
          detail_rows <- list(
            if (taxon_fit != "Unspecified") htmltools::tags$tr(
              htmltools::tags$td("Taxon Fit"),
              htmltools::tags$td(class = "text-end", taxon_fit)
            ),
            if (taxon_confidence != "Unspecified") htmltools::tags$tr(
              htmltools::tags$td("Taxon Confidence"),
              htmltools::tags$td(class = "text-end", taxon_confidence)
            ),
            if (group_type != "Unspecified") htmltools::tags$tr(
              htmltools::tags$td("Group Type"),
              htmltools::tags$td(class = "text-end", as.character(group_type))
            )
          )
          detail_rows <- Filter(Negate(is.null), detail_rows)

          detail_table <- if (length(detail_rows) > 0) {
            htmltools::tags$table(
              class = "table table-sm table-striped table-hover",
              style = "margin-top: 5px; margin-bottom: 0; width: 100%; table-layout: fixed; word-break: break-word;",
              htmltools::tags$tbody(detail_rows)
            )
          } else {
            NULL
          }

          htmltools::tags$div(
            style = separator_style,
            if (length(badges) > 0) htmltools::tags$div(style = "margin-bottom: 4px;", badges),
            htmltools::tags$div(style = "font-size: 1.05em; font-weight: 700; margin-bottom: 2px;", plant_link),
            subtitle_by,
            subtitle_role,
            subtitle_date,
            detail_table
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
    inference_area_display <- append_units("taxon_inference_area", obs$taxon_inference_area %|||% "Unspecified")

    # Original and current interpretation links from interpretations data
    orig_link <- "Unspecified"
    curr_link <- "Unspecified"

    if (!is.null(taxon_interps) && nrow(taxon_interps) > 0) {
      if ("is_orig" %in% names(taxon_interps)) {
        orig_rows <- taxon_interps[!is.na(taxon_interps$is_orig) & taxon_interps$is_orig == TRUE, , drop = FALSE]
        if (nrow(orig_rows) > 0) {
          pc_orig <- orig_rows$pc_code[1] %|||% NULL
          label_orig <- orig_rows$plant_label[1] %|||% "Unspecified"
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
          label_curr <- curr_rows$plant_label[1] %|||% "Unspecified"
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
      current_interpretation = curr_link,
      original_interpretation = orig_link,
      reference = ref_display
    )

    col_names <- c(
      taxon_inference_area = "Taxon Inference Area",
      current_interpretation = "Current Interpretation",
      original_interpretation = "Original Interpretation",
      reference = "Original Reference"
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

          stratum_name <- imp$stratum_name %|||% ""
          cover_val <- imp$cover %|||% NA
          cover_display <- if (!is.null(cover_val) && !is.na(cover_val) && is.numeric(cover_val)) {
            sprintf("%.2f%%", cover_val)
          } else {
            ""
          }

          cover_code <- imp$cover_code %|||% ""
          basal_area <- append_units("basal_area", imp$basal_area %|||% "")
          inference_area <- append_units("inference_area", imp$inference_area %|||% "")
          biomass <- imp$biomass %|||% ""

          htmltools::tags$tr(
            htmltools::tags$td(as.character(stratum_name)),
            htmltools::tags$td(cover_display),
            htmltools::tags$td(as.character(cover_code)),
            htmltools::tags$td(as.character(inference_area)),
            htmltools::tags$td(as.character(basal_area)),
            htmltools::tags$td(as.character(biomass))
          )
        })

        create_detail_table_with_headers(
          c("Stratum", "Cover", "Cover Code", "Infer Area", "Basal Area", "Bio-mass"),
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
