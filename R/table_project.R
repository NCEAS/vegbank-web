#' Project Table Module
#'
#' Provides a remote (server-side) DataTable for VegBank projects.

#' @noRd
PROJECT_TABLE_FIELDS <- c(
  "pj_code",
  "project_name",
  "project_description",
  "obs_count",
  "start_date",
  "stop_date",
  "last_plot_added_date"
)

PROJECT_TABLE_SCHEMA_TEMPLATE <- build_schema_template(
  column_names = PROJECT_TABLE_FIELDS,
  integer_columns = "obs_count"
)

PROJECT_TABLE_DISPLAY_TEMPLATE <- build_display_template(
  column_names = c("Actions", "VegBank Code", "Project", "Plots", "Started", "Ended", "Last Plot Added", "Description")
)

create_project_column_defs <- function() {
  list(
    list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"), # Actions
    list(targets = 1, width = "12%", orderable = TRUE), # pj_code (VegBank Code)
    list(targets = 2, width = "23%", orderable = TRUE), # Project Name
    list(targets = 3, width = "8%", className = "dt-right", type = "num", orderable = TRUE), # Plots (obs_count)
    list(targets = 4, width = "12%", orderable = FALSE), # Started
    list(targets = 5, width = "12%", orderable = FALSE), # Ended
    list(targets = 6, width = "13%", orderable = FALSE), # Last Plot Added
    list(targets = 7, width = "25%", orderable = FALSE) # Description
  )
}

#' Build Project Table
#'
#' Configures the projects table to fetch rows via AJAX, mirroring other remote tables.
#'
#' @return A DT::datatable object
#' @noRd
build_project_table <- function() {
  build_table_from_spec(PROJECT_TABLE_SPEC)
}

#' Transform normalized project data into display rows
#'
#' @param project_data Data frame with normalized project columns
#' @return Data frame formatted for DataTables consumption
#' @noRd
process_project_data <- function(project_data) {
  if (is.null(project_data)) {
    project_data <- PROJECT_TABLE_SCHEMA_TEMPLATE
  }

  row_count <- nrow(project_data)
  if (!row_count) {
    return(PROJECT_TABLE_DISPLAY_TEMPLATE)
  }

  actions <- create_action_buttons("proj_link_click", "Details", project_data$pj_code)

  names <- clean_column_data(project_data, "project_name")
  pj_codes <- project_data$pj_code

  obs_counts <- suppressWarnings(as.integer(project_data$obs_count))
  obs_counts[is.na(obs_counts)] <- 0L

  # Create clickable obs_count links for cross-resource filtering
  obs_count_links <- create_all_obs_count_links(
    obs_counts,
    pj_codes,
    names
  )

  starts <- clean_column_dates(project_data, "start_date")
  stops <- clean_column_dates(project_data, "stop_date")
  last_added <- clean_column_dates(project_data, "last_plot_added_date")

  descriptions <- clean_column_data(project_data, "project_description")

  data.frame(
    "Actions" = actions,
    "VegBank Code" = vapply(pj_codes, htmltools::htmlEscape, character(1)),
    "Project" = vapply(names, htmltools::htmlEscape, character(1)),
    "Plots" = obs_count_links,
    "Started" = vapply(starts, htmltools::htmlEscape, character(1)),
    "Ended" = vapply(stops, htmltools::htmlEscape, character(1)),
    "Last Plot Added" = vapply(last_added, htmltools::htmlEscape, character(1)),
    "Description" = vapply(seq_along(descriptions), function(i) {
      safe_desc <- htmltools::htmlEscape(descriptions[i])
      safe_code <- htmltools::htmlEscape(as.character(pj_codes[i]), attribute = TRUE)
      sprintf(
        '<div class="dt-description-container"><div class="dt-description">%s</div><a href="#" class="dt-read-more dt-shiny-action" data-input-id="proj_link_click" data-value="%s">Read more</a></div>',
        safe_desc, safe_code
      )
    }, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Normalize project API responses into a consistent schema
#'
#' @param df Raw data frame or list from vegbankr
#' @return Normalized data frame containing PROJECT_TABLE_FIELDS
#' @noRd
normalize_project_data <- create_normalizer(PROJECT_TABLE_SCHEMA_TEMPLATE, na_to_zero_fields = "obs_count")

#' Coerce VegBank project response to a data frame
#' @noRd
coerce_project_page <- create_coercer(PROJECT_TABLE_SCHEMA_TEMPLATE)

.PROJECT_TABLE_HELP_CONTENT <- local({
  html <- as.character(htmltools::tagList(
    htmltools::tags$p("This table lists all VegBank projects. Projects group related plot observations collected under a common study or survey."),
    htmltools::tags$ul(
      htmltools::tags$li(htmltools::tags$strong("Search:"), " use the search box to filter by name, description, or VegBank code."),
      htmltools::tags$li(htmltools::tags$strong("Show plots:"), " click the number in the Plots column to filter the Plots table to observations belonging to this project."),
      htmltools::tags$li(htmltools::tags$strong("Sort:"), " click a column header to sort; Project name and Plots support sorting. Sort by multiple columns by holding shift and clicking multiple headers."),
      htmltools::tags$li(htmltools::tags$strong("Open details:"), " the Details button in the Actions column opens additional information about the project in an overlay."),
    )
  ))
  html <- gsub("\n", "", html, fixed = TRUE)
  gsub("'", "\\'", html, fixed = TRUE)
})

PROJECT_TABLE_SPEC <- list(
  table_id = "proj_table",
  resource = "projects",
  loading_label = "projects",
  column_defs = create_project_column_defs(),
  schema_fields = PROJECT_TABLE_FIELDS,
  schema_template = PROJECT_TABLE_SCHEMA_TEMPLATE,
  coerce_fn = coerce_project_page,
  normalize_fn = normalize_project_data,
  display_fn = process_project_data,
  data_source = list(
    detail = "full",
    clean_names = FALSE,
    clean_rows_fn = sanitize_dt_rows,
    sort_field_map = list(
      "1" = "default",           # VegBank Code
      "2" = "project_name",      # Project (sort by name)
      "3" = "obs_count"          # Plots (sort by obs_count)
    )
  ),
  page_length = NULL,
  search_placeholder = "by name, description, or VegBank code…",
  options = list(
    dom = "Bfrtip",
    buttons = I(list(make_help_button_js("Projects Table", .PROJECT_TABLE_HELP_CONTENT)))
  ),
  datatable_args = list(extensions = "Buttons"),
  initial_display = PROJECT_TABLE_DISPLAY_TEMPLATE
)
