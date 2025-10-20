#' Concept Table Functions Module
#'
#' Provides generalized functions for creating and manipulating concept data tables
#' (both plant and community concepts).

#' Main function to process and build a concept table
#'
#' @param concept_data Data frame of concept data (plant or community)
#' @param concept_type Either "plant" or "community"
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_concept_table <- function(concept_data, concept_type = "plant") {
  is_plant <- concept_type == "plant"
  data_key <- if (is_plant) "plant_data" else "community_data"

  data_sources <- list()
  data_sources[[data_key]] <- concept_data

  required_sources <- c(data_key)

  table_config <- list(
    column_defs = list(
      list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
      list(targets = 1, width = "25%"), # Name
      list(targets = 2, width = "12%", className = "dt-center"), # Status
      list(targets = 3, width = "20%"), # Reference Source
      list(targets = 4, width = "10%", type = "num", className = "dt-right"), # Observations
      list(targets = 5, width = "28%") # Description
    ),
    progress_message = paste0("Processing ", concept_type, " concepts table data")
  )

  create_table(
    data_sources = data_sources,
    required_sources = required_sources,
    process_function = function(ds) process_concept_data(ds, concept_type),
    table_config = table_config
  )
}

#' Process concept data into display format
#'
#' @param data_sources List containing concept data
#' @param concept_type Either "plant" or "community"
#' @returns A data frame ready for display
#' @noRd
process_concept_data <- function(data_sources, concept_type = "plant") {
  is_plant <- concept_type == "plant"

  # Get the data from the appropriate key
  data_key <- if (is_plant) "plant_data" else "community_data"
  concept_data <- data_sources[[data_key]]

  # Determine field names based on concept type
  name_field <- if (is_plant) "plant_name" else "comm_name"
  description_field <- if (is_plant) "plant_description" else "comm_description"
  code_field <- if (is_plant) "pc_code" else "cc_code"
  link_input_id <- if (is_plant) "plant_link_click" else "comm_link_click"
  name_label <- if (is_plant) "Plant Name" else "Community Name"

  shiny::incProgress(0.15, detail = paste0("Cleaning ", concept_type, " names"))
  names <- clean_column_data(concept_data, name_field)

  shiny::incProgress(0.1, detail = "Creating status badges")
  status_badges <- create_status_badges(concept_data$current_accepted)

  shiny::incProgress(0.15, detail = "Cleaning reference names")
  concept_rf_names <- clean_column_data(concept_data, "concept_rf_name")

  shiny::incProgress(0.15, detail = "Cleaning observation counts")
  obs_counts <- as.numeric(clean_column_data(concept_data, "obs_count", "0"))

  shiny::incProgress(0.15, detail = paste0("Cleaning ", concept_type, " descriptions"))
  descriptions <- clean_column_data(concept_data, description_field)

  shiny::incProgress(0.1, detail = "Creating action buttons")
  action_buttons <- create_action_buttons(concept_data, list(
    list(input_id = link_input_id, input_value = code_field, label = "Details", class = "btn-outline-primary")
  ))

  shiny::incProgress(0.15, detail = "Building table...")
  result <- data.frame(
    "Actions" = action_buttons,
    "Status" = status_badges,
    "Reference Source" = concept_rf_names,
    "Observations" = obs_counts,
    "Description" = descriptions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add the name column with the appropriate label
  result <- cbind(
    Actions = result$Actions,
    setNames(data.frame(names, stringsAsFactors = FALSE), name_label),
    result[, c("Status", "Reference Source", "Observations", "Description")]
  )

  result
}