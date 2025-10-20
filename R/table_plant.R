#' Plant Table Functions Module
#'
#' Provides functions for creating and manipulating the plant concepts data table.

#' Main function to process and build the plant table
#'
#' @param plant_data Data frame of plant concept data
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_plant_table <- function(plant_data) {
  data_sources <- list(
    plant_data = plant_data
  )

  required_sources <- c("plant_data")

  table_config <- list(
    column_defs = list(
      list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
      list(targets = 1, width = "25%"), # Plant Name
      list(targets = 2, width = "12%", className = "dt-center"), # Status
      list(targets = 3, width = "20%"), # Reference Source
      list(targets = 4, width = "10%", type = "num", className = "dt-right"), # Observations
      list(targets = 5, width = "28%") # Description
    ),
    progress_message = "Processing plant concepts table data"
  )

  create_table(
    data_sources = data_sources,
    required_sources = required_sources,
    process_function = process_plant_data,
    table_config = table_config
  )
}

#' Process plant data into display format
#'
#' @param data_sources List containing plant_data
#' @returns A data frame ready for display
#' @noRd
process_plant_data <- function(data_sources) {
  plant_data <- data_sources$plant_data

  shiny::incProgress(0.15, detail = "Cleaning plant names")
  plant_names <- clean_column_data(plant_data, "plant_name")

  shiny::incProgress(0.1, detail = "Creating status badges")
  status_badges <- create_status_badges(plant_data$current_accepted)

  shiny::incProgress(0.15, detail = "Cleaning concept reference names")
  concept_rf_names <- clean_column_data(plant_data, "concept_rf_name")

  shiny::incProgress(0.15, detail = "Cleaning observation counts")
  obs_counts <- as.numeric(clean_column_data(plant_data, "obs_count", "0"))

  shiny::incProgress(0.15, detail = "Cleaning plant descriptions")
  plant_descriptions <- clean_column_data(plant_data, "plant_description")

  shiny::incProgress(0.1, detail = "Creating action buttons")
  action_buttons <- create_action_buttons(plant_data, list(
    list(input_id = "plant_link_click", input_value = 'pc_code', label = "Details", class = "btn-outline-primary")
  ))

  shiny::incProgress(0.15, detail = "Building table...")
  data.frame(
    "Actions" = action_buttons,
    "Plant Name" = plant_names,
    "Status" = status_badges,
    "Reference Source" = concept_rf_names,
    "Observations" = obs_counts,
    "Description" = plant_descriptions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
