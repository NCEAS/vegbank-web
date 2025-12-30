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
  column_names = c("Actions", "Vegbank Code", "Project", "Plots", "Started", "Ended", "Last Plot Added", "Description"),
  column_types = list("Plots" = integer())
)

create_project_column_defs <- function() {
  list(
    list(
      targets = 0,
      orderable = FALSE,
      searchable = FALSE,
      width = "10%",
      render = create_action_button_renderer("proj_link_click", "Details")
    ), # Actions
    list(targets = 1, width = "12%", orderable = TRUE), # pj_code (Vegbank Code)
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

  action_codes <- project_data$pj_code
  action_codes <- if (is.null(action_codes)) rep("", row_count) else as.character(action_codes)
  action_codes[is.na(action_codes)] <- ""

  names <- clean_column_data(project_data, "project_name")
  pj_codes <- project_data$pj_code

  obs_counts <- suppressWarnings(as.integer(project_data$obs_count))
  obs_counts[is.na(obs_counts)] <- 0L

  starts <- clean_column_dates(project_data, "start_date")
  stops <- clean_column_dates(project_data, "stop_date")
  last_added <- clean_column_dates(project_data, "last_plot_added_date")

  descriptions <- clean_column_data(project_data, "project_description")
  descriptions <- truncate_text_with_ellipsis(descriptions, max_chars = 680L)

  data.frame(
    "Actions" = action_codes,
    "Vegbank Code" = pj_codes,
    "Project" = names,
    "Plots" = obs_counts,
    "Started" = starts,
    "Ended" = stops,
    "Last Plot Added" = last_added,
    "Description" = descriptions,
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

PROJECT_TABLE_SPEC <- list(
  table_id = "proj_table",
  resource = "projects",
  remote_label = "project records",
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
      "1" = "default",           # Vegbank Code
      "2" = "project_name",      # Project (sort by name)
      "3" = "obs_count"          # Plots (sort by obs_count)
    )
  ),
  page_length = NULL,
  options = list(),
  datatable_args = list(),
  initial_display = PROJECT_TABLE_DISPLAY_TEMPLATE
)
