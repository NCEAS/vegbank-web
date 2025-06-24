#' Community Table Functions Module
#'
#' Provides functions for creating and manipulating the community data table.

#' Main function to process and build the community table
#'
#' @param community_data Data frame of community data
#' @returns A DT datatable object ready for display in a Shiny app
#' @export
build_community_table <- function(community_data) {
  data_sources <- list(
    community_data = community_data
  )

  required_sources <- c("community_data")

  table_config <- list(
    column_defs = list(
      list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
      list(targets = 1, width = "30%"),
      list(targets = 2, width = "20%"),
      list(targets = 3, width = "40%")
    ),
    progress_message = "Processing community table data"
  )

  create_table(
    data_sources = data_sources,
    required_sources = required_sources,
    process_function = process_community_data,
    table_config = table_config
  )
}

#' Process community data into display format
#'
#' @param data_sources List containing community_data
#' @returns A data frame ready for display
#' @noRd
process_community_data <- function(data_sources) {
  community_data <- data_sources$community_data

  shiny::incProgress(0.2, detail = "Cleaning community names")
  comm_names <- clean_column_data(community_data, "default_name")

  shiny::incProgress(0.2, detail = "Cleaning community descriptions")
  comm_desc <- clean_column_data(community_data, "comm_description")

  shiny::incProgress(0.2, detail = "Cleaning observation counts")
  obs_count <- clean_column_data(community_data, "obs_count")

  shiny::incProgress(0.1, detail = "Creating action buttons")
  action_buttons <- create_action_buttons(community_data, list(
    list(input_id = "comm_link_click", input_value = 'accession_code', label = "Details", class = "btn-outline-primary")
  ))

  shiny::incProgress(0.2, detail = "Building table...")
  data.frame(
    "Actions" = action_buttons,
    "Name" = comm_names,
    "Observations" = obs_count,
    "Description" = comm_desc,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' TODO: Create HTML vectors for reference lists
#'
#' @param community_data Data frame of community data
#' @param reference_data Data frame of reference data
#' @returns A character vector of HTML strings for references
#' @noRd
create_reference_vectors <- function(community_data, reference_data) {
  # Implementation would be similar to create_taxa_vectors but for references
  # This is a placeholder for the actual implementation
  merged <- dplyr::left_join(community_data, reference_data, by = "comm_concept_id")
  # Process reference data into HTML...
  rep("References would appear here", nrow(community_data))
}
