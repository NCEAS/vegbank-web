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

  # Determine input ID based on concept type
  link_input_id <- if (is_plant) "plant_link_click" else "comm_link_click"

  data_sources <- list()
  data_sources[[data_key]] <- concept_data

  required_sources <- c(data_key)

  table_config <- list(
    # Resorted to hidden columns to get proper sorting behavior for status badge and reference source
    # columns because server-side processing with R's DT library (which is reccommended for data
    # of our size) is incompatible with the orthogonal data structure expected by the base JS library.
    # Server-side processing requires R to sort data, and so even if the data is prepared perfectly
    # in a list or object with values for display, sort, and filter, R is fundamentally unable to compare
    # lists and throws an DataTables warning: table id=DataTables_Table_O - Error in xi == xj: comparison
    # of these types is not implemented https://datatables.net/manual/data/orthogonal-data
    column_defs = list(
      # Actions
      list(
        targets = 0, width = "10%", orderable = FALSE, searchable = FALSE,
        render = create_action_button_renderer(link_input_id, "Details")
      ),
      # Name
      list(targets = 1, width = "25%"),
      # Status - render client-side, sort by hidden column
      list(
        targets = 2, width = "12%", className = "dt-center",
        render = create_status_badge_renderer(), orderData = 3
      ),
      list(targets = 3, visible = FALSE, searchable = FALSE), # Hidden: status sort values (0, 1, 2)
      # Reference Source - render client-side, sort by hidden column
      list(
        targets = 4, width = "20%",
        render = create_reference_link_renderer(), orderData = 5
      ),
      list(targets = 5, visible = FALSE, searchable = FALSE), # Hidden: reference names for sorting
      # Observations
      list(targets = 6, width = "10%", type = "num", className = "dt-right"),
      # Description
      list(targets = 7, width = "28%")
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
  name_label <- if (is_plant) "Plant Name" else "Community Name"

  shiny::incProgress(0.15, detail = paste0("Cleaning ", concept_type, " names"))
  names <- clean_column_data(concept_data, name_field)

  shiny::incProgress(0.1, detail = "Preparing status data")
  status_raw <- concept_data$current_accepted
  # Sort order: Accepted (0) < Not Current (1) < No Status (2)
  status_sort <- ifelse(is.na(status_raw), 2, ifelse(status_raw == TRUE, 0, 1))

  shiny::incProgress(0.15, detail = "Preparing reference data")
  # Pass reference codes and names for client-side rendering
  reference_codes <- concept_data$concept_rf_code
  reference_names <- clean_column_data(concept_data, "concept_rf_name")

  shiny::incProgress(0.15, detail = "Cleaning observation counts")
  obs_counts <- as.numeric(clean_column_data(concept_data, "obs_count", "0"))

  shiny::incProgress(0.15, detail = paste0("Cleaning ", concept_type, " descriptions"))
  descriptions <- clean_column_data(concept_data, description_field)

  shiny::incProgress(0.1, detail = "Preparing action codes")
  # Pass the code value directly - will be rendered by JavaScript
  action_codes <- concept_data[[code_field]]

  result <- data.frame(
    Actions = action_codes,
    Name = names,
    Status = status_raw,
    status_sort = status_sort,
    "Reference Source" = reference_codes,
    ref_sort = reference_names,
    Observations = obs_counts,
    Description = descriptions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Insert the appropriate label for the Name column
  names(result)[names(result) == "Name"] <- name_label

  result
}

# -------------- JS Renderers ---------------------

#' Create JavaScript renderer for action buttons
#' Creates Details button with click handler for concept detail view
#'
#' @param input_id The Shiny input ID for the button click event
#' @param button_label The label text for the button (default: "Details")
#' @returns A DT::JS object containing the JavaScript renderer function
#' @noRd
create_action_button_renderer <- function(input_id = "link_click", button_label = "Details") {
  # Create the JavaScript function with sprintf
  js_code <- sprintf(
    "function(data, type, row, meta) {
      if (type === 'display') {
        // data should be the code value
        if (!data || data === '') return '<span>No Data</span>';

        return '<div class=\"btn-group btn-group-sm\">' +
               '<button class=\"btn btn-sm btn-outline-primary\"' +
               ' onclick=\"Shiny.setInputValue(\\'%s\\', \\'' + data + '\\', {priority: \\'event\\'})\">' +
               '%s' +
               '</button>' +
               '</div>';
      }
      return data;
    }",
    input_id,
    button_label
  )

  DT::JS(js_code)
}

#' Create JavaScript renderer for status badges
#' Renders badges client-side from raw boolean values
#'
#' @returns A DT::JS object containing the JavaScript renderer function
#' @noRd
create_status_badge_renderer <- function() {
  js_code <- "function(data, type, row, meta) {
      if (type === 'display') {
        // data is boolean or null
        if (data === null || data === '' || typeof data === 'undefined') {
          return '<span class=\"badge rounded-pill\" style=\"background-color: var(--no-status-bg); ' +
                    'color: var(--no-status-text);\">No Status</span>';
        } else if (data === true || data === 'true' || data === 'TRUE') {
          return '<span class=\"badge rounded-pill\" style=\"background-color: var(--accepted-bg); ' +
                    'color: var(--accepted-text);\">Accepted</span>';
        } else {
          return '<span class=\"badge rounded-pill\" style=\"background-color: var(--not-current-bg); ' +
                    'color: var(--not-current-text);\">Not Current</span>';
        }
      }
      // For sort/filter/type, return the raw data (sorting handled by hidden column)
      return data;
    }"

  DT::JS(js_code)
}

#' Create JavaScript renderer for reference links
#' Renders clickable links client-side from reference data objects
#'
#' @returns A DT::JS object containing the JavaScript renderer function
#' @noRd
create_reference_link_renderer <- function() {
  js_code <- "function(data, type, row, meta) {
      if (type === 'display') {
        // data is the reference code; the display name is in the hidden sort column
        var code = data;
        var name = row && row.length > 5 ? row[5] : null;

        if (name === null || name === undefined) {
          name = '';
        }
        name = String(name);
        if (name.trim() === '') {
          name = 'Not provided';
        }

        // If \"Not Provided\" or no code, show as plain text
        if (name === 'Not Provided' || !code || code === '') {
          return '<span>' + name + '</span>';
        }

        // Create clickable link
        return '<a href=\"#\" data-code=\"' + code + 
                  '\" onclick=\"Shiny.setInputValue(\\'ref_link_click\\', \\'' + code + 
                  '\\', {priority: \\'event\\'}); return false;\">' + name + '</a>';
      }
      // For sort/filter/type, return the raw data (sorting handled by hidden column)
      return data;
    }"

  DT::JS(js_code)
}