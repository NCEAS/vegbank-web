#' Overview Module
#'
#' Provides functions for rendering the Overview page statistics and visualizations.
#'
#' @name overview
#' @noRd
NULL

#' Initialize Overview Data Reactive
#'
#' Creates a reactive expression that fetches overview data from the vegbankr API.
#' Data is cached to avoid repeated API calls.
#'
#' @param state Reactive values containing application state
#' @param session Shiny session object
#'
#' @return A reactive expression that returns overview data
#' @noRd
init_overview_data <- function(state, session) {
  # Initialize cache variable in parent environment
  .overview_data_cache <- NULL

  # Create reactive
  overview_data <- shiny::reactive({
    # Return cached data if available (overlay already hidden)
    if (!is.null(.overview_data_cache)) {
      return(.overview_data_cache)
    }

    tryCatch(
      {
        data <- vegbankr::vb_overview()
        .overview_data_cache <<- data
        # Hide loading overlay after data loads
        session$sendCustomMessage("hideLoadingOverlay", list(type = "overview"))
        data
      },
      error = function(e) {
        warning("Failed to fetch overview data: ", conditionMessage(e))
        session$sendCustomMessage("hideLoadingOverlay", list(type = "overview"))
        NULL
      }
    )
  })

  overview_data
}

#' Render Core Counts Card
#'
#' Renders the core counts as a nested hierarchical list.
#'
#' @param output Shiny output object
#' @param overview_data Reactive expression containing overview data
#' @noRd
render_core_counts <- function(output, overview_data) {
  output$core_counts_list <- shiny::renderUI({
    data <- overview_data()
    if (is.null(data) || is.null(data$core_counts)) {
      return(htmltools::tags$p("No data available"))
    }

    counts <- data$core_counts
    # Create a lookup for easy access
    count_map <- setNames(counts$count, counts$name)

    htmltools::tags$div(
      style = "font-size: 15px; line-height: 2;",
      htmltools::tags$div(
        htmltools::tags$strong("Observations"),
        htmltools::tags$span(
          class = "float-end text-primary fw-bold",
          format(count_map["Observations"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        style = "padding-left: 20px;",
        htmltools::tags$span("Classified observations"),
        htmltools::tags$span(
          class = "float-end text-muted",
          format(count_map["Classified observations"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        style = "padding-left: 40px;",
        htmltools::tags$span("NVC classified observations"),
        htmltools::tags$span(
          class = "float-end text-muted",
          format(count_map["NVC classified observations"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        htmltools::tags$strong("Plants"),
        htmltools::tags$span(
          class = "float-end text-primary fw-bold",
          format(count_map["Plants"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        style = "padding-left: 20px;",
        htmltools::tags$span("Accepted USDA plants"),
        htmltools::tags$span(
          class = "float-end text-muted",
          format(count_map["Accepted USDA plants"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        style = "padding-left: 40px;",
        htmltools::tags$span("Observed accepted USDA plants"),
        htmltools::tags$span(
          class = "float-end text-muted",
          format(count_map["Observed accepted USDA plants"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        htmltools::tags$strong("Communities"),
        htmltools::tags$span(
          class = "float-end text-primary fw-bold",
          format(count_map["Communities"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        style = "padding-left: 20px;",
        htmltools::tags$span("Accepted US NVC communities"),
        htmltools::tags$span(
          class = "float-end text-muted",
          format(count_map["Accepted US NVC communities"], big.mark = ",")
        )
      ),
      htmltools::tags$div(
        style = "padding-left: 40px;",
        htmltools::tags$span("Observed accepted US NVC communities"),
        htmltools::tags$span(
          class = "float-end text-muted",
          format(count_map["Observed accepted US NVC communities"], big.mark = ",")
        )
      )
    )
  })
}

#' Render Top Projects Card
#'
#' Renders a horizontal bar chart showing top projects by observation count.
#'
#' @param output Shiny output object
#' @param overview_data Reactive expression containing overview data
#' @noRd
render_top_projects <- function(output, overview_data) {
  output$top_projects_plot <- shiny::renderUI({
    data <- overview_data()
    if (is.null(data) || is.null(data$top_n_projects)) {
      return(NULL)
    }

    projects <- data$top_n_projects[order(-data$top_n_projects$count), ]
    max_count <- max(projects$count)

    rows <- lapply(seq_len(nrow(projects)), function(i) {
      width_pct <- (projects$count[i] / max_count) * 100
      htmltools::tags$div(
        class = "mb-3",
        htmltools::tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          htmltools::tags$a(
            href = "#",
            style = "font-size: 0.9rem; word-break: break-word;",
            onclick = sprintf(
              "Shiny.setInputValue('proj_link_click', '%s', {priority: 'event'}); return false;",
              projects$pj_code[i]
            ),
            projects$name[i]
          ),
          htmltools::tags$a(
            href = "#",
            class = "ms-2",
            style = "font-size: 0.85rem; white-space: nowrap;",
            onclick = sprintf(
              "Shiny.setInputValue('obs_count_click', {code: '%s', label: '%s'}, {priority: 'event'}); return false;",
              projects$pj_code[i],
              gsub("'", "\\'", projects$name[i])
            ),
            format(projects$count[i], big.mark = ",")
          )
        ),
        htmltools::tags$div(
          class = "overview-bar-track",
          role = "progressbar",
          `aria-valuenow` = projects$count[i],
          `aria-valuemin` = "0",
          `aria-valuemax` = max_count,
          htmltools::tags$div(
            class = "overview-bar-fill overview-bar-projects",
            style = sprintf("width: %s%%;", width_pct)
          )
        )
      )
    })

    htmltools::tags$div(rows)
  })
}

#' Render Top Communities Card
#'
#' Renders a horizontal bar chart showing top community concepts by observation count.
#'
#' @param output Shiny output object
#' @param overview_data Reactive expression containing overview data
#' @noRd
render_top_communities <- function(output, overview_data) {
  output$top_communities_plot <- shiny::renderUI({
    data <- overview_data()
    if (is.null(data) || is.null(data$top_n_community_concepts)) {
      return(NULL)
    }

    communities <- data$top_n_community_concepts[order(-data$top_n_community_concepts$count), ]
    max_count <- max(communities$count)

    rows <- lapply(seq_len(nrow(communities)), function(i) {
      width_pct <- (communities$count[i] / max_count) * 100
      htmltools::tags$div(
        class = "mb-3",
        htmltools::tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          htmltools::tags$a(
            href = "#",
            style = "font-size: 0.9rem; word-break: break-word;",
            onclick = sprintf(
              "Shiny.setInputValue('comm_link_click', '%s', {priority: 'event'}); return false;",
              communities$cc_code[i]
            ),
            communities$name[i]
          ),
          htmltools::tags$a(
            href = "#",
            class = "ms-2",
            style = "font-size: 0.85rem; white-space: nowrap;",
            onclick = sprintf(
              "Shiny.setInputValue('obs_count_click', {code: '%s', label: '%s'}, {priority: 'event'}); return false;",
              communities$cc_code[i],
              gsub("'", "\\'", communities$name[i])
            ),
            format(communities$count[i], big.mark = ",")
          )
        ),
        htmltools::tags$div(
          class = "overview-bar-track",
          role = "progressbar",
          `aria-valuenow` = communities$count[i],
          `aria-valuemin` = "0",
          `aria-valuemax` = max_count,
          htmltools::tags$div(
            class = "overview-bar-fill overview-bar-communities",
            style = sprintf("width: %s%%;", width_pct)
          )
        )
      )
    })

    htmltools::tags$div(rows)
  })
}

#' Render Top Contributors Card
#'
#' Renders a horizontal bar chart showing top contributors by observation count.
#'
#' @param output Shiny output object
#' @param overview_data Reactive expression containing overview data
#' @noRd
render_top_contributors <- function(output, overview_data) {
  output$top_contributors_plot <- shiny::renderUI({
    data <- overview_data()
    if (is.null(data) || is.null(data$top_n_contributors)) {
      return(NULL)
    }

    contributors <- data$top_n_contributors[order(-data$top_n_contributors$count), ]
    max_count <- max(contributors$count)

    rows <- lapply(seq_len(nrow(contributors)), function(i) {
      width_pct <- (contributors$count[i] / max_count) * 100
      htmltools::tags$div(
        class = "mb-3",
        htmltools::tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          htmltools::tags$a(
            href = "#",
            style = "font-size: 0.9rem;",
            onclick = sprintf(
              "Shiny.setInputValue('party_link_click', '%s', {priority: 'event'}); return false;",
              contributors$py_code[i]
            ),
            contributors$name[i]
          ),
          htmltools::tags$a(
            href = "#",
            class = "ms-2",
            style = "font-size: 0.85rem; white-space: nowrap;",
            onclick = sprintf(
              "Shiny.setInputValue('obs_count_click', {code: '%s', label: '%s'}, {priority: 'event'}); return false;",
              contributors$py_code[i],
              contributors$name[i]
            ),
            format(contributors$count[i], big.mark = ",")
          )
        ),
        htmltools::tags$div(
          class = "overview-bar-track",
          role = "progressbar",
          `aria-valuenow` = contributors$count[i],
          `aria-valuemin` = "0",
          `aria-valuemax` = max_count,
          htmltools::tags$div(
            class = "overview-bar-fill overview-bar-contributors",
            style = sprintf("width: %s%%;", width_pct)
          )
        )
      )
    })

    htmltools::tags$div(rows)
  })
}

#' Render Top Plants Card
#'
#' Renders a horizontal bar chart showing top 5 plant concepts by observation count.
#'
#' @param output Shiny output object
#' @param overview_data Reactive expression containing overview data
#' @noRd
render_top_plants <- function(output, overview_data) {
  output$top_plants_plot <- shiny::renderUI({
    data <- overview_data()
    if (is.null(data) || is.null(data$top_n_plant_concepts)) {
      return(NULL)
    }

    plants <- data$top_n_plant_concepts[order(-data$top_n_plant_concepts$count), ]
    # Ensure exactly 5 entries
    plants <- head(plants, 5)
    max_count <- max(plants$count)

    rows <- lapply(seq_len(nrow(plants)), function(i) {
      width_pct <- (plants$count[i] / max_count) * 100
      htmltools::tags$div(
        class = "mb-3",
        htmltools::tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          htmltools::tags$a(
            href = "#",
            style = "font-size: 0.9rem; word-break: break-word;",
            onclick = sprintf(
              "Shiny.setInputValue('plant_link_click', '%s', {priority: 'event'}); return false;",
              plants$pc_code[i]
            ),
            plants$name[i]
          ),
          htmltools::tags$a(
            href = "#",
            class = "ms-2",
            style = "font-size: 0.85rem; white-space: nowrap;",
            onclick = sprintf(
              "Shiny.setInputValue('obs_count_click', {code: '%s', label: '%s'}, {priority: 'event'}); return false;",
              plants$pc_code[i],
              gsub("'", "\\'", plants$name[i])
            ),
            format(plants$count[i], big.mark = ",")
          )
        ),
        htmltools::tags$div(
          class = "overview-bar-track",
          role = "progressbar",
          `aria-valuenow` = plants$count[i],
          `aria-valuemin` = "0",
          `aria-valuemax` = max_count,
          htmltools::tags$div(
            class = "overview-bar-fill overview-bar-plants",
            style = sprintf("width: %s%%;", width_pct)
          )
        )
      )
    })

    htmltools::tags$div(rows)
  })
}

#' Render Latest Projects Card
#'
#' Renders a table showing recently updated projects.
#'
#' @param output Shiny output object
#' @param overview_data Reactive expression containing overview data
#' @noRd
render_latest_projects <- function(output, overview_data) {
  output$latest_projects_table <- shiny::renderUI({
    data <- overview_data()
    if (is.null(data) || is.null(data$latest_n_projects)) {
      return(htmltools::tags$p("No data available"))
    }

    projects <- data$latest_n_projects
    rows <- lapply(seq_len(nrow(projects)), function(i) {
      full_name <- projects$name[i]
      htmltools::tags$tr(
        htmltools::tags$td(
          style = "min-width: 0;",
          htmltools::tags$a(
            href = "#",
            class = "overview-project-name",
            title = full_name,
            onclick = sprintf(
              "Shiny.setInputValue('proj_link_click', '%s', {priority: 'event'}); return false;",
              projects$pj_code[i]
            ),
            full_name
          )
        ),
        htmltools::tags$td(
          class = "overview-date",
          as.Date(projects$last_date_added[i], format = "%a, %d %b %Y")
        ),
        htmltools::tags$td(
          class = "text-end",
          htmltools::tags$a(
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('obs_count_click', {code: '%s', label: '%s'}, {priority: 'event'}); return false;",
              projects$pj_code[i],
              gsub("'", "\\'", projects$name[i])
            ),
            format(projects$count[i], big.mark = ",")
          )
        )
      )
    })

    htmltools::tags$table(
      class = "table table-sm table-hover",
      style = "table-layout: fixed; width: 100%;",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th("Project"),
          htmltools::tags$th("Last Added", style = "width: 120px;"),
          htmltools::tags$th("Obs", class = "text-end", style = "width: 80px;")
        )
      ),
      htmltools::tags$tbody(rows)
    )
  })
}

#' Render Data Summary
#'
#' Renders the introductory text and instructions for the Overview page.
#'
#' @param output Shiny output object
#' @noRd
render_data_summary <- function(output) {
  output$dataSummary <- shiny::renderUI({
    htmltools::tagList(
      htmltools::tags$div(
        htmltools::strong(
          "Now with some filtering, some citations, and downloading plot observations!"
        ),
        htmltools::tags$p(
          "If you're new to VegBank, check out our ",
          htmltools::tags$a(
            href = "?tab=Getting Started",
            "getting started guide"
          ),
          " or our ",
          htmltools::tags$a(
            href = "?tab=FAQ",
            "FAQ"
          ),
          " page for common questions and troubleshooting tips."
        )
      ),
      htmltools::tags$p(
        "For more information, see the previous ",
        htmltools::tags$a(href = "http://vegbank.org", target = "_blank", " VegBank site"),
        " or the ",
        htmltools::tags$a(
          href = "https://github.com/NCEAS/vegbankr",
          target = "_blank",
          "vegbankr R package"
        ),
        " for programmatic data access."
      ),
      htmltools::tags$br(),
      htmltools::tags$h5("Beta Notice"),
      htmltools::tags$p(
        htmltools::tags$em(
          " More features are coming soon and this page will change. This is still a beta release; ",
          " things may be slow and buggy. Please ",
          htmltools::tags$a(href = "mailto:help@vegbank.org", "report bugs here"),
          "with details about what you were doing when the problem occurred."
        ),
      )
    )
  })
}

#' Initialize Overview Module
#'
#' Main function to set up all overview page outputs.
#'
#' @param output Shiny output object
#' @param state Reactive values containing application state
#' @param session Shiny session object
#'
#' @return A reactive expression containing overview data
#' @noRd
init_overview <- function(output, state, session) {
  # Initialize data reactive
  overview_data <- init_overview_data(state, session)

  # Render all cards
  render_core_counts(output, overview_data)
  render_top_projects(output, overview_data)
  render_top_communities(output, overview_data)
  render_top_contributors(output, overview_data)
  render_top_plants(output, overview_data)
  render_latest_projects(output, overview_data)
  render_data_summary(output)

  overview_data
}
