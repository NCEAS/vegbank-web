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
    column_defs = list(
      list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%", render = create_action_button_renderer(link_input_id, "Details")),
      list(targets = 1, width = "25%"), # Name
      list(targets = 2, width = "12%", className = "dt-center", render = create_status_badge_renderer()), # Status
      list(targets = 3, width = "20%", type = "string", render = create_reference_link_renderer()), # Reference Source
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

  shiny::incProgress(0.1, detail = "Preparing status data")
  # Pass raw boolean values - will be rendered by JavaScript
  status_values <- concept_data$current_accepted

  shiny::incProgress(0.15, detail = "Preparing reference data")
  # Create list objects with code and name for JavaScript renderer
  reference_data <- create_reference_objects(concept_data)

  shiny::incProgress(0.15, detail = "Cleaning observation counts")
  obs_counts <- as.numeric(clean_column_data(concept_data, "obs_count", "0"))

  shiny::incProgress(0.15, detail = paste0("Cleaning ", concept_type, " descriptions"))
  descriptions <- clean_column_data(concept_data, description_field)

  shiny::incProgress(0.1, detail = "Preparing action codes")
  # Pass the code value directly - will be rendered by JavaScript
  action_codes <- concept_data[[code_field]]

  shiny::incProgress(0.15, detail = "Building table...")
  result <- data.frame(
    "Actions" = action_codes,
    "Status" = status_values,
    "Reference Source" = I(reference_data),
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

create_reference_objects <- function(concept_data) { 
  mapply(
    function(code, name) list(code = code, name = name),
    concept_data$concept_rf_code,
    concept_data$concept_rf_name,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
}

# ' Create status badges for a vector of status values
#'  Allows for client-side filtering by status in table
#'
# ' @param status_vector A vector of logical values (TRUE, FALSE, NA)
# ' @returns A character vector of HTML strings representing status badges
# ' @noRd
create_status_badges <- function(status_vector) {
  vapply(status_vector, function(status) {
    # Could be made fasterer with pre-generated HTML strings, but this is clearer
    if (is.na(status)) {
      as.character(
        htmltools::span(
          class = "badge rounded-pill",
          style = "background-color: var(--no-status-bg); color: var(--no-status-text);",
          "No Status"
        )
      )
    } else if (status == TRUE) {
      as.character(
        htmltools::span(
          class = "badge rounded-pill",
          style = "background-color: var(--accepted-bg); color: var(--accepted-text);",
          "Accepted"
        )
      )
    } else {
      as.character(
        htmltools::span(
          class = "badge rounded-pill",
          style = "background-color: var(--not-current-bg); color: var(--not-current-text);",
          "Not Current"
        )
      )
    }
  }, character(1))
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
               '<button class=\"btn btn-sm btn-outline-primary\" onclick=\"Shiny.setInputValue(\\'%s\\', \\'' + data + '\\', {priority: \\'event\\'})\">' +
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

#' Create JavaScript renderer for reference links
#' Creates clickable links to reference sources using concept_rf_code and concept_rf_name
#'
#' @returns A DT::JS object containing the JavaScript renderer function
#' @noRd
create_reference_link_renderer <- function() {
  # Create the JavaScript function
  js_code <- sprintf(
    "function(data, type, row, meta) {  
      // Handle display - create clickable link
      if (type === 'display') {
        // data should be an object with {code: '...', name: '...'}
        if (!data || typeof data !== 'object') return '<span>Not provided</span>';
        if (!data.code || !data.name) return '<span>Not provided</span>';
        
        var code = data.code;
        var name = data.name;
        
        return '<a href=\"#\" data-code=\"' + code + '\" onclick=\"Shiny.setInputValue(\\'ref_link_click\\', \\'' + code + '\\', {priority: \\'event\\'}); return false;\">' + name + '</a>';
      }
    }"
  )
  
  DT::JS(js_code)
}#' Create JavaScript renderer for status badges
#' Creates badges client side so we don't have to iterate over the whole table in R
#' Doesn't allow for client-side filtering by status
#'
#' @returns A DT::JS object containing the JavaScript renderer function
#' @noRd
create_status_badge_renderer <- function() {
  # Could be made fasterer with pre-generated HTML strings, but this is clearer
  no_status_badge <- as.character(
    htmltools::span(
      class = "badge rounded-pill",
      style = "background-color: var(--no-status-bg); color: var(--no-status-text);",
      "No Status"
    )
  )

  accepted_badge <- as.character(
    htmltools::span(
      class = "badge rounded-pill",
      style = "background-color: var(--accepted-bg); color: var(--accepted-text);",
      "Accepted"
    )
  )

  not_current_badge <- as.character(
    htmltools::span(
      class = "badge rounded-pill",
      style = "background-color: var(--not-current-bg); color: var(--not-current-text);",
      "Not Current"
    )
  )

  # Now create the JavaScript function using the R-generated HTML
  js_code <- sprintf(
    "function(data, type, row, meta) {
      if (type === 'display') {
        if (data === null || data === '') {
          return '%s';
        } else if (data === true || data === 'true' || data === 'TRUE') {
          return '%s';
        } else {
          return '%s';
        }
      }
      return data;
    }",
    no_status_badge,
    accepted_badge,
    not_current_badge
  )

  DT::JS(js_code)
}
