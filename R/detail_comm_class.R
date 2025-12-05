#' Output names for Community Classification detail view
#' @noRd
comm_class_detail_output_names <- c(
  "comm_class_header",
  "comm_class_details",
  "comm_class_interpretations",
  "comm_class_contributors"
)

#' Build Community Classification Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed community
#' classification information. This includes the classification header,
#' observation details, analysis methods, community interpretations, and
#' contributors.
#'
#' @param result A data frame returned by vegbankr::get_community_classification()
#'   Expected columns at top level:
#'   - cl_code, ob_code, class_start_date, class_stop_date
#'   - inspection, table_analysis, multivariate_analysis, expert_system
#'   - class_publication_rf_code, class_publication_rf_label, class_notes
#'   Nested list-columns:
#'   - interpretations: data frame with cc_code, ci_code, class_confidence,
#'     class_fit, comm_authority_name, comm_authority_rf_code, comm_code,
#'     comm_name, nomenclatural_type, notes, type
#'   - contributors: data frame with party, py_code, role
#' @return A list of Shiny UI outputs for each card section
#' @noRd
build_comm_class_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      comm_class_detail_output_names,
      "Community classification details"
    ))
  }

  list(
    comm_class_header = create_comm_class_header_ui(result),
    comm_class_details = create_comm_class_details_ui(result),
    comm_class_interpretations = create_comm_class_interpretations_ui(result),
    comm_class_contributors = create_comm_class_contributors_ui(result)
  )
}
#' Create Community Classification Header UI
#'
#' Displays the community name, CEGL code, observation link, and date range.
#'
#' @param result The classification data frame
#' @return A shiny.render.function
#' @noRd
create_comm_class_header_ui <- function(result) {
  shiny::renderUI({
    htmltools::div(
      htmltools::tags$h5(
        result$cl_code,
        "of ",
        create_detail_link("plot_link_click", result$ob_code, result$ob_code),
        style = "font-weight: 600;"
      ),
      if (has_valid_field_value(result, "class_start_date") ||
          has_valid_field_value(result, "class_stop_date")) {
        htmltools::tags$p(
          format_date_range(result$class_start_date, result$class_stop_date)
        )
      }
    )
  })
}

#' Create Community Classification Methods UI
#'
#' Displays the analysis methods used for the classification, including
#' publication reference, notes, and expert system fields.
#'
#' @param result The classification data frame
#' @return A shiny.render.function
#' @noRd
create_comm_class_details_ui <- function(result) {
  shiny::renderUI({
    rows <- list(
      htmltools::tags$tr(
        htmltools::tags$td("Inspection"),
        htmltools::tags$td(class = "text-end", format_boolean(result$inspection))
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Table Analysis"),
        htmltools::tags$td(class = "text-end", format_boolean(result$table_analysis))
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Multivariate Analysis"),
        htmltools::tags$td(class = "text-end", format_boolean(result$multivariate_analysis))
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Expert System"),
        htmltools::tags$td(class = "text-end", format_boolean(result$expert_system))
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Classification Publication"),
        htmltools::tags$td(
          class = "text-end",
          if (has_valid_field_value(result, "class_publication_rf_code") &&
            has_valid_field_value(result, "class_publication_rf_label")) {
            create_detail_link(
              "ref_link_click",
              result$class_publication_rf_code,
              result$class_publication_rf_label
            )
          } else {
            "Not provided"
          }
        )
      )
    )

    if (has_valid_field_value(result, "class_notes")) {
      rows <- append(rows, list(
        htmltools::tags$tr(
          htmltools::tags$td("Notes"),
          htmltools::tags$td(class = "text-end", result$class_notes)
        )
      ))
    }

    htmltools::tags$table(
      class = "table table-sm table-striped table-hover",
      style = "width: 100%; table-layout: fixed; word-break: break-word;",
      htmltools::tags$tbody(rows)
    )

  })
}

#' Create Community Classification Interpretations UI
#'
#' Displays the community interpretations from the nested interpretations data.
#'
#' @param result The classification data frame containing interpretations list-column
#' @return A shiny.render.function
#' @noRd
create_comm_class_interpretations_ui <- function(result) {
  shiny::renderUI({
    tryCatch(
      {
        interpretations <- extract_nested_table(result, "interpretations")

        if (nrow(interpretations) == 0) {
          return(htmltools::tags$p("No community interpretations recorded"))
        }

        rows <- lapply(seq_len(nrow(interpretations)), function(i) {
          interp <- interpretations[i, , drop = FALSE]

          comm_name <- interp$comm_name %|||% "Unknown community"
          cc_code <- if ("cc_code" %in% names(interp)) interp$cc_code else NULL

          confidence <- interp$class_confidence %|||% "Not recorded"
          fit <- interp$class_fit %|||% "Not recorded"
          typal_value <- if ("type" %in% names(interp)) format_boolean(interp$type) else "Not recorded"

          authority <- if ("comm_authority_name" %in% names(interp)) {
            interp$comm_authority_name %|||% "Not specified"
          } else {
            "Not specified"
          }

          authority_rf_code <- if ("comm_authority_rf_code" %in% names(interp)) {
            interp$comm_authority_rf_code
          } else {
            NULL
          }

          htmltools::tags$div(
            style = ifelse(nrow(interpretations) > 1, "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #eee;", ""),
            htmltools::tags$div(
              style = "font-weight: 600;",
              if (!is.null(cc_code) && !is.na(cc_code)) {
                create_detail_link("comm_link_click", cc_code, comm_name)
              } else {
                comm_name
              }
            ),
            htmltools::tags$table(
              class = "table table-sm table-striped table-hover",
              style = "margin-top: 5px; margin-bottom: 0; width: 100%; table-layout: fixed; word-break: break-word;",
              htmltools::tags$tbody(
                htmltools::tags$tr(
                  htmltools::tags$td("Class Fit"),
                  htmltools::tags$td(class = "text-end", fit)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Class Confidence"),
                  htmltools::tags$td(class = "text-end", confidence)
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Authority Reference"),
                  htmltools::tags$td(
                    class = "text-end",
                    if (!is.null(authority_rf_code) && !is.na(authority_rf_code)) {
                      create_detail_link("ref_link_click", authority_rf_code, authority)
                    } else {
                      authority
                    }
                  )
                ),
                htmltools::tags$tr(
                  htmltools::tags$td("Typal"),
                  htmltools::tags$td(class = "text-end", typal_value)
                ),
                if ("nomenclatural_type" %in% names(interp) &&
                  has_valid_field_value(interp, "nomenclatural_type")) {
                  htmltools::tags$tr(
                    htmltools::tags$td("Nomenclatural Type"),
                    htmltools::tags$td(class = "text-end", format_boolean(interp$nomenclatural_type))
                  )
                },
                if ("notes" %in% names(interp) && has_valid_field_value(interp, "notes")) {
                  htmltools::tags$tr(
                    htmltools::tags$td("Notes"),
                    htmltools::tags$td(class = "text-end", interp$notes)
                  )
                }
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

#' Create Community Classification Contributors UI
#'
#' Displays the contributors who created this classification.
#'
#' @param result The classification data frame containing contributors list-column
#' @return A shiny.render.function
#' @noRd
create_comm_class_contributors_ui <- function(result) {
  shiny::renderUI({
    tryCatch(
      {
        contributors <- extract_nested_table(result, "contributors")

        if (nrow(contributors) == 0) {
          return(htmltools::tags$p("No contributors recorded"))
        }

        rows <- lapply(seq_len(nrow(contributors)), function(i) {
          contrib <- contributors[i, , drop = FALSE]

          party_name <- contrib$party %|||% "Unknown"
          py_code <- if ("py_code" %in% names(contrib)) contrib$py_code else NULL
          role <- contrib$role %|||% "Not specified"

          htmltools::tags$tr(
            htmltools::tags$td(
              if (!is.null(py_code) && !is.na(py_code)) {
                create_detail_link("party_link_click", py_code, party_name)
              } else {
                party_name
              }
            ),
            htmltools::tags$td(class = "text-end", role)
          )
        })

        create_detail_table_with_headers(
          c("Party", "Role"),
          rows,
          table_style = "width: 100%; table-layout: fixed; word-break: break-word;",
          header_styles = c("text-align: left;", "text-align: right;")
        )
      },
      error = function(e) {
        htmltools::tags$p(paste("Error processing contributors:", e$message))
      }
    )
  })
}
