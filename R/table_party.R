#' Party Table Module
#'
#' Provides functions for creating and manipulating the party table.


#' Build Party Table
#'
#' Creates a data table for displaying party information.
#'
#' @param party_data A data frame containing party information
#' @return A DT::datatable object
#' @noRd
#'
build_party_table <- function(party_data) {

  party_table_config <- list(
    column_defs = list(
      list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
      list(targets = 1, width = "20%"), # Given Name
      list(targets = 2, width = "20%"), # Surname
      list(targets = 3, width = "30%"), # Organization
      list(targets = 4, width = "20%") # Contact
    ),
    progress_message = "Processing party table data"
  )

  create_table(
    data_sources = list(party_data = party_data),
    required_sources = "party_data",
    process_function = process_party_data,
    table_config = party_table_config
  )
}

#' Process Party Data for Display
#'
#' Processes party data for display in the data table.
#'
#' @param data_sources A list containing party data
#' @return A processed data frame ready for display
#'
#' @importFrom shiny incProgress
#' @noRd
process_party_data <- function(data_sources) {
  shiny::incProgress(0.2, detail = "Preparing party data")

  # Extract party data
  party_data <- data_sources$party_data

  # Process and clean the columns needed for display
  # TODO: Plot contributed column
  shiny::incProgress(0.15, detail = "Cleaning names")
  given_names <- clean_column_data(party_data, "given_name")
  surnames <- clean_column_data(party_data, "surname")
  shiny::incProgress(0.15, detail = "Cleaning organizations")
  organizations <- clean_column_data(party_data, "organization_name")
  shiny::incProgress(0.15, detail = "Cleaning contact instructions")
  contact_info <- clean_column_data(party_data, "contact_instructions")

  # Create action buttons for each row
  action_buttons <- create_action_buttons(
    party_data,
    list(
      list(
        input_id = "party_link_click",
        input_value = "party_accession_code",
        label = "Details",
        class = "btn-outline-primary"
      )
    )
  )

  shiny::incProgress(0.2, detail = "Building table...")
  # Combine all columns into the final data frame
  data.frame(
    "Actions" = action_buttons,
    "Given Name" = given_names,
    "Surname" = surnames,
    "Organization" = organizations,
    "Contact" = contact_info,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
