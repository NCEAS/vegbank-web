#' Project Table Functions Module
#'
#' Provides functions for creating and manipulating the project data table.

#' Main function to process and build the project table
#'
#' @param project_data Data frame of project data
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_project_table <- function(project_data) {
  data_sources <- list(
    project_data = project_data
  )

  required_sources <- c("project_data")

  table_config <- list(
    # TODO: Target columns by name instead of index so they are more robust to order changes
    column_defs = list(
      list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
      list(targets = 1, width = "30%"),
      list(targets = 2, width = "8%", className = "dt-right"),
      list(targets = 3, width = "10%"),
      list(targets = 4, width = "10%"),
      list(targets = 5, width = "10%"),
      list(targets = 6, width = "20%")
    ),
    progress_message = "Processing project table data"
  )

  create_table(
    data_sources = data_sources,
    required_sources = required_sources,
    process_function = process_project_data,
    table_config = table_config
  )
}

#' Process project data into display format
#'
#' @param data_sources List containing project_data
#' @returns A data frame ready for display
#' @noRd
process_project_data <- function(data_sources) {
  project_data <- data_sources$project_data

  shiny::incProgress(0.15, detail = "Cleaning project names")
  proj_names <- clean_column_data(project_data, "project_name")

  shiny::incProgress(0.15, detail = "Cleaning project descriptions")
  proj_desc <- clean_column_data(project_data, "project_description")

  shiny::incProgress(0.15, detail = "Cleaning observation counts")
  obs_count <- clean_column_data(project_data, "obs_count")

  shiny::incProgress(0.15, detail = "Cleaning start dates")
  start_date <- clean_column_data(project_data, "start_date")

  shiny::incProgress(0.15, detail = "Cleaning end dates")
  stop_date <- clean_column_data(project_data, "stop_date") # Fix: Use correct column name "stop_date" instead of "end_date"

  shiny::incProgress(0.15, detail = "Cleaning most recent dates")
  last_added_date <- clean_column_data(project_data, "last_plot_added_date")

  # TODO: Classified Plots & States / Provinces columns

  shiny::incProgress(0.1, detail = "Creating action buttons")
  action_buttons <- create_action_buttons(project_data, list(
    list(input_id = "proj_link_click", input_value = 'project_accession_code', label = "Details", class = "btn-outline-primary")
  ))

  shiny::incProgress(0.2, detail = "Building table...")
  data.frame(
    "Actions" = action_buttons,
    "Name" = proj_names,
    "Plots" = obs_count,
    "Started" = start_date,
    "Ended" = end_date,
    "Last Plot Added" = last_added_date,
    "Description" = proj_desc,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' TODO: Make obs_count, states/provinces, and classified_plots links...