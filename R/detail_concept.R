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

  name_output <- if (is_plant) "plant_concept_name" else "community_concept_name"
  details_output <- if (is_plant) "plant_concept_details" else "community_concept_details"
  perspective_output <- if (is_plant) "plant_party_perspective" else "community_party_perspective"

  if (is.null(result) || nrow(result) == 0) {
    empty_outputs <- list()
    empty_outputs[[name_output]] <- shiny::renderUI({
      htmltools::tags$p(paste0(tools::toTitleCase(concept_type), " concept details not available"))
    })
    empty_outputs[[details_output]] <- shiny::renderUI({
      htmltools::tags$p("No details available")
    })
    empty_outputs[[perspective_output]] <- shiny::renderUI({
      htmltools::tags$p("No party perspective available")
    })
    return(empty_outputs)
  }

  outputs <- list()

  outputs[[name_output]] <- shiny::renderUI({
    htmltools::div(
      htmltools::tags$i(tools::toTitleCase(as.character(result[[level_field]])) %|||% "Unspecified level"),
      htmltools::tags$h5(result[[name_field]], style = "font-weight: 600; margin-bottom: 0px;"),
      if (!is.na(result[[code_field]])) htmltools::tags$p(paste0("(", result[[code_field]], ")"))
    )
  })

  outputs[[details_output]] <- shiny::renderUI({
    htmltools::tags$div(
      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        htmltools::tags$tbody(
          htmltools::tags$tr(
            htmltools::tags$td(paste0(tools::toTitleCase(concept_type), " Code")),
            htmltools::tags$td(class = "text-end", result[[id_field]])
          ),
          htmltools::tags$tr(
            htmltools::tags$td("Reference"),
            htmltools::tags$td(
              class = "text-end",
              if (!is.na(result$concept_rf_code) && !is.na(result$concept_rf_name)) {
                htmltools::tags$a(
                  href = "#",
                  onclick = sprintf(
                    "Shiny.setInputValue('ref_link_click', '%s', {priority: 'event'}); return false;",
                    result$concept_rf_code
                  ),
                  result$concept_rf_name
                )
              } else {
                result$concept_rf_name %|||% "Not specified"
              }
            )
          ),
          htmltools::tags$tr(
            htmltools::tags$td("Observation Count"),
            htmltools::tags$td(class = "text-end", result$obs_count %|||% "0")
          )
        )
      ),
      if (!is.na(result[[description_field]])) {
        htmltools::tags$div(
          style = "margin-top: 15px;",
          htmltools::tags$div(
            "Description",
            style = "width: 100%; border-bottom: 1px solid #2c5443; margin-bottom: 10px;"
          ),
          htmltools::tags$div(
            id = "concept-description",
            htmltools::HTML(result[[description_field]] %|||% "No description available")
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
  aliases_content <- NULL
  usages_field <- "usages"

  if (!is.na(result[[usages_field]]) &&
    !is.null(result[[usages_field]]) &&
    length(result[[usages_field]][[1]]) > 0) {
    tryCatch(
      {
        usages_data <- result[[usages_field]][[1]]
        if (is.data.frame(usages_data) && nrow(usages_data) > 0) {
          class_system_col <- if ("class_system" %in% names(usages_data)) "class_system" else NULL
          name_col <- if ("name" %in% names(usages_data)) {
            "name"
          } else if (is_plant && "plant_name" %in% names(usages_data)) {
            "plant_name"
          } else if (!is_plant && "comm_name" %in% names(usages_data)) {
            "comm_name"
          } else {
            names(usages_data)[2]
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

          aliases_content <- htmltools::tags$table(
            class = "table table-sm table-striped table-hover",
            htmltools::tags$tbody(aliases_rows)
          )
        }
      },
      error = function(e) {
        aliases_content <- htmltools::tags$p(paste("Error parsing aliases:", e$message))
      }
    )
  }

  if (is.null(aliases_content)) {
    htmltools::tags$p("No aliases available")
  } else {
    aliases_content
  }
}

#' Create Party Perspective UI for Concept (Plant or Community)
#' @noRd
create_party_perspective_ui <- function(result, concept_type, id_field, parent_id_field, link_input_id) {
  is_plant <- concept_type == "plant"

  shiny::renderUI({
    children_links <- NULL
    if (!is.na(result$children) &&
      !is.null(result$children) &&
      result$children != "" &&
      length(result$children[[1]]) > 0) {
      tryCatch(
        {
          children_data <- result$children[[1]]
          if (is.data.frame(children_data) && nrow(children_data) > 0) {
            children_links <- lapply(seq_len(nrow(children_data)), function(i) {
              child_code <- children_data[[id_field]][i]
              child_name <- children_data[[if (is_plant) "plant_name" else "comm_name"]][i]
              htmltools::tags$li(
                htmltools::tags$a(
                  href = "#",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                    link_input_id,
                    child_code
                  ),
                  child_name
                )
              )
            })
          } else if (is.list(children_data) && length(children_data) > 0) {
            children_links <- lapply(names(children_data), function(child_code) {
              htmltools::tags$li(
                htmltools::tags$a(
                  href = "#",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                    link_input_id,
                    child_code
                  ),
                  children_data[[child_code]]
                )
              )
            })
          }
        },
        error = function(e) {
          children_links <- NULL
        }
      )
    }

    correlations_links <- NULL
    if (!is.na(result$correlations) &&
      !is.null(result$correlations) &&
      result$correlations != "" &&
      length(result$correlations[[1]]) > 0) {
      tryCatch(
        {
          corr_data <- result$correlations[[1]]
          if (is.data.frame(corr_data) && nrow(corr_data) > 0) {
            correlations_links <- lapply(seq_len(nrow(corr_data)), function(i) {
              corr_code <- corr_data[[id_field]][i]
              corr_name <- corr_data[[if (is_plant) "plant_name" else "comm_name"]][i]
              corr_type <- if ("correlation_type" %in% names(corr_data)) {
                paste0(" (", corr_data$correlation_type[i], ")")
              } else {
                ""
              }
              htmltools::tags$li(
                htmltools::tags$a(
                  href = "#",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                    link_input_id,
                    corr_code
                  ),
                  paste0(corr_name, corr_type)
                )
              )
            })
          }
        },
        error = function(e) {
          correlations_links <- NULL
        }
      )
    }

    htmltools::tags$div(
      htmltools::tags$b({
        if (!is.na(result$py_code) && !is.na(result$party)) {
          htmltools::tags$a(
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('party_link_click', '%s', {priority: 'event'}); return false;",
              result$py_code
            ),
            result$party
          )
        } else {
          result$party %|||% "Party not recorded"
        }
      }),
      htmltools::tags$span(
        if (!is.na(result$status)) {
          htmltools::tags$i(paste0(" (", result$status, ")"))
        } else {
          " (Status not recorded)"
        }
      ),
      htmltools::tags$p({
        start_parsed <- safe_parse_date(result$start_date)
        stop_parsed <- safe_parse_date(result$stop_date)
        if (!is.na(start_parsed) && !is.na(stop_parsed)) {
          paste0("From ", format(start_parsed, "%Y-%m-%d"), " to ", format(stop_parsed, "%Y-%m-%d"))
        } else if (!is.na(start_parsed)) {
          paste0("From ", format(start_parsed, "%Y-%m-%d"))
        } else if (!is.na(stop_parsed)) {
          paste0("Until ", format(stop_parsed, "%Y-%m-%d"))
        } else {
          "Date not recorded"
        }
      }),
      htmltools::tags$div(
        "Aliases",
        style = "font-weight: bold; width: 100%; border-bottom: 1px solid #2c5443;"
      ),
      create_concept_aliases_ui(result, is_plant),
      htmltools::tags$div(
        if (is_plant) "Taxonomic Hierarchy" else "Classification Hierarchy",
        style = "font-weight: bold; width: 100%; border-bottom: 1px solid #2c5443;"
      ),
      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        htmltools::tags$tbody(
          htmltools::tags$tr(
            htmltools::tags$td("Parent"),
            htmltools::tags$td(
              class = "text-end",
              if (!is.na(result$parent_name)) {
                htmltools::tags$a(
                  href = "#",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                    link_input_id,
                    result[[parent_id_field]]
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
