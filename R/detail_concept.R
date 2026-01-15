#' Build Concept Details View (for both Plant and Community Concepts)
#' @noRd
build_concept_details_view <- function(result, concept_type = "plant") {
  is_plant <- concept_type == "plant"
  name_field <- if (is_plant) "plant_name" else "comm_name"
  code_field <- if (is_plant) "plant_code" else "comm_code"
  level_field <- if (is_plant) "plant_level" else "comm_level"
  description_field <- if (is_plant) "plant_description" else "comm_description"
  id_field <- if (is_plant) "pc_code" else "cc_code"
  parent_id_field <- if (is_plant) "parent_pc_code" else "parent_cc_code"
  link_input_id <- if (is_plant) "plant_link_click" else "comm_link_click"

  name_output <- if (is_plant) "plant_concept_header" else "community_concept_header"
  details_output <- if (is_plant) "plant_concept_details" else "community_concept_details"
  perspective_output <- if (is_plant) "plant_party_perspective" else "community_party_perspective"

  if (is.null(result) || nrow(result) == 0) {
    output_names <- c(name_output, details_output, perspective_output)
    return(create_empty_detail_view(
      output_names,
      paste0(tools::toTitleCase(concept_type), " concept details")
    ))
  }

  outputs <- list()

  outputs[[name_output]] <- shiny::renderUI({
    htmltools::div(
      htmltools::tags$i(tools::toTitleCase(as.character(result[[level_field]])) %|||% "Unspecified level"),
      htmltools::tags$h5(result[[name_field]], style = "font-weight: 600; margin-bottom: 0px;"),
      if (!is.na(result[[code_field]])) htmltools::tags$p(paste0("(", result[[code_field]], ")"), style = "margin-bottom: 0px;"),
      htmltools::tags$h5(result[[id_field]], style = "color: var(--vb-green); font-weight: 600;")
    )
  })

  outputs[[details_output]] <- shiny::renderUI({
    htmltools::tags$div(
      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        htmltools::tags$tbody(
          htmltools::tags$tr(
            htmltools::tags$td("Reference"),
            htmltools::tags$td(
              class = "text-end",
              if (!is.na(result$concept_rf_code) && !is.na(result$concept_rf_label)) {
                create_detail_link("ref_link_click", result$concept_rf_code, result$concept_rf_label)
              } else {
                result$concept_rf_label %|||% "Not specified"
              }
            )
          ),
          htmltools::tags$tr(
            htmltools::tags$td("Observation Count"),
            htmltools::tags$td(
              class = "text-end",
              create_obs_count_link(
                result$obs_count,
                result[[id_field]],
                result[[name_field]]
              )
            )
          )
        )
      ),
      if (!is.na(result[[description_field]])) {
        htmltools::tags$div(
          style = "margin-top: 15px;",
          create_section_header("Description", "10px"),
          htmltools::tags$div(
            id = "concept-description",
            htmltools::htmlEscape(result[[description_field]] %|||% "No description available")
          )
        )
      }
    )
  })

  outputs[[perspective_output]] <- create_party_perspective_ui(
    result,
    concept_type,
    id_field,
    parent_id_field,
    link_input_id
  )

  outputs
}


#' Create Concept Aliases UI (for Plant or Community)
#' @noRd
create_concept_aliases_ui <- function(result, is_plant = TRUE) {
  usages_data <- extract_nested_table(result, "usages")

  if (nrow(usages_data) == 0) {
    return(htmltools::tags$p("No aliases available"))
  }

  tryCatch(
    {
      class_system_col <- if ("class_system" %in% names(usages_data)) "class_system" else NULL
      if (is_plant && "plant_name" %in% names(usages_data)) {
        name_col <- "plant_name"
      } else if (!is_plant && "comm_name" %in% names(usages_data)) {
        name_col <- "comm_name"
      } else {
        stop("No suitable name column found in usages_data.
        Expected either 'plant_name' or 'comm_name'")
      }

      if (!is.null(class_system_col)) {
        usages_data <- usages_data[order(usages_data[[class_system_col]]), ]
      }

      aliases_rows <- lapply(seq_len(nrow(usages_data)), function(i) {
        class_system <- if (!is.null(class_system_col)) usages_data[[class_system_col]][i] else "Usage"
        usage_name <- usages_data[[name_col]][i]
        htmltools::tags$tr(
          htmltools::tags$td(class_system),
          htmltools::tags$td(class = "text-end", usage_name)
        )
      })

      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        htmltools::tags$tbody(aliases_rows)
      )
    },
    error = function(e) {
      htmltools::tags$p(paste("Error parsing aliases:", e$message))
    }
  )
}

#' Create Party Perspective UI for Concept (Plant or Community)
#' @noRd
create_party_perspective_ui <- function(result, concept_type, id_field, parent_id_field, link_input_id) {
  is_plant <- concept_type == "plant"

  shiny::renderUI({
    # Extract children data
    children_data <- extract_nested_table(result, "children")
    children_links <- NULL

    if (nrow(children_data) > 0) {
      tryCatch(
        {
          children_links <- lapply(seq_len(nrow(children_data)), function(i) {
            child_code <- children_data[[id_field]][i]
            child_name <- children_data[[if (is_plant) "plant_name" else "comm_name"]][i]
            htmltools::tags$li(
              create_detail_link(link_input_id, child_code, child_name)
            )
          })
        },
        error = function(e) {
          children_links <- NULL
        }
      )
    }

    # Extract correlations data
    corr_data <- extract_nested_table(result, "correlations")
    correlations_links <- NULL

    if (nrow(corr_data) > 0) {
      tryCatch(
        {
          correlations_links <- lapply(seq_len(nrow(corr_data)), function(i) {
            corr_code <- corr_data[[id_field]][i]
            corr_name <- corr_data[[if (is_plant) "plant_name" else "comm_name"]][i]
            corr_type <- if ("correlation_type" %in% names(corr_data)) {
              paste0(" (", corr_data$correlation_type[i], ")")
            } else {
              ""
            }
            htmltools::tags$li(
              create_detail_link(link_input_id, corr_code, paste0(corr_name, corr_type))
            )
          })
        },
        error = function(e) {
          correlations_links <- NULL
        }
      )
    }

    htmltools::tags$div(
      htmltools::tags$b({
        if (!is.na(result$py_code) && !is.na(result$party_label)) {
          create_detail_link("party_link_click", result$py_code, result$party_label)
        } else {
          result$party_label %|||% "Party not recorded"
        }
      }),
      htmltools::tags$span(
        if (!is.na(result$status)) {
          htmltools::tags$i(paste0(" (", result$status, ")"))
        } else {
          " (Status not recorded)"
        }
      ),
      htmltools::tags$p(format_date_range(result$start_date, result$stop_date)),
      create_section_header("Aliases"),
      create_concept_aliases_ui(result, is_plant),
      create_section_header(if (is_plant) "Taxonomic Hierarchy" else "Classification Hierarchy"),
      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        htmltools::tags$tbody(
          htmltools::tags$tr(
            htmltools::tags$td("Parent"),
            htmltools::tags$td(
              class = "text-end",
              if (!is.na(result$parent_name)) {
                create_detail_link(link_input_id, result[[parent_id_field]], result$parent_name)
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
          },
          if (!is.null(correlations_links)) {
            htmltools::tags$tr(
              htmltools::tags$td("Correlations"),
              htmltools::tags$td(class = "text-end", htmltools::tags$ul(correlations_links))
            )
          }
        )
      )
    )
  })
}

build_plant_concept_details_view <- function(result) {
  build_concept_details_view(result, concept_type = "plant")
}

build_comm_concept_details_view <- function(result) {
  build_concept_details_view(result, concept_type = "community")
}
