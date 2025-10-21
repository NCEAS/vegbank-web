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
      list(targets = 2, width = "12%", className = "dt-center", orderData = 3, searchData = 4), # Status - sort by column 3, search in column 4
      list(targets = 3, visible = FALSE, searchable = FALSE), # Hidden: status sort values (0, 1, 2)
      list(targets = 4, visible = FALSE), # Hidden: status filter text (Accepted, Not Current, No Status)
      list(targets = 5, width = "20%", orderData = 6), # Reference Source - sort using hidden column
      list(targets = 6, visible = FALSE, searchable = FALSE), # Hidden: reference names for sorting
      list(targets = 7, width = "10%", type = "num", className = "dt-right"), # Observations
      list(targets = 8, width = "28%") # Description
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
  # Create status badges with HTML for display and text for sorting/filtering
  # Pre-generate badge HTML strings once for performance
  status_raw <- concept_data$current_accepted
  
  accepted_badge <- '<span class="badge rounded-pill" style="background-color: var(--accepted-bg); color: var(--accepted-text);">Accepted</span>'
  not_current_badge <- '<span class="badge rounded-pill" style="background-color: var(--not-current-bg); color: var(--not-current-text);">Not Current</span>'
  no_status_badge <- '<span class="badge rounded-pill" style="background-color: var(--no-status-bg); color: var(--no-status-text);">No Status</span>'
  
  # Vectorized assignment of badges
  status_display <- ifelse(is.na(status_raw), no_status_badge, 
                           ifelse(status_raw == TRUE, accepted_badge, not_current_badge))
  
  # Sort order: Accepted (0) < Not Current (1) < No Status (2)
  status_sort <- ifelse(is.na(status_raw), 2, ifelse(status_raw == TRUE, 0, 1))
  
  # Text for filtering
  status_filter <- ifelse(is.na(status_raw), "No Status", 
                          ifelse(status_raw == TRUE, "Accepted", "Not Current"))

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
  # Extract reference codes and names for display and sorting
  reference_codes <- vapply(reference_data, function(x) x$code, character(1))
  reference_names <- vapply(reference_data, function(x) x$name, character(1))
  
  # Create display HTML for reference column
  is_not_provided <- reference_names == "Not Provided" | is.na(reference_codes) | reference_codes == ""
  reference_display <- ifelse(
    is_not_provided,
    sprintf('<span>%s</span>', reference_names),
    sprintf(
      '<a href="#" data-code="%s" onclick="Shiny.setInputValue(\'ref_link_click\', \'%s\', {priority: \'event\'}); return false;">%s</a>',
      reference_codes, reference_codes, reference_names
    )
  )

  result <- data.frame(
    "Actions" = action_codes,
    "Status" = status_display,
    "status_sort" = status_sort,  # Hidden column for sorting (0=Accepted, 1=Not Current, 2=No Status)
    "status_filter" = status_filter,  # Hidden column for filtering
    "Reference Source" = reference_display,
    "ref_sort" = reference_names,  # Hidden column for sorting references
    "Observations" = obs_counts,
    "Description" = descriptions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add the name column with the appropriate label
  result <- cbind(
    Actions = result$Actions,
    setNames(data.frame(names, stringsAsFactors = FALSE), name_label),
    result[, c("Status", "status_sort", "status_filter", "Reference Source", "ref_sort", "Observations", "Description")]
  )

  result
}

create_reference_objects <- function(concept_data) {
  # Clean the reference names first (vectorized operation)
  cleaned_names <- clean_column_data(concept_data, "concept_rf_name")

  # Use mapply to create simple list objects
  mapply(
    function(code, name) list(code = code, name = name),
    concept_data$concept_rf_code,
    cleaned_names,
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
#' Creates clickable links to reference sources
#' Uses orthogonal data structure with display/sort/filter properties
#'
#' @returns A DT::JS object containing the JavaScript renderer function
#' @noRd
create_reference_link_renderer <- function() {
  js_code <- "function(data, type, row, meta) {
      // data is an object with: {display: '...', sort: '...', filter: '...'}
      if (!data || typeof data !== 'object') {
        return '';
      }

      // Return the appropriate property based on the render type
      if (type === 'display') {
        return data.display || '';
      } else if (type === 'sort') {
        return data.sort || '';
      } else if (type === 'filter') {
        return data.filter || '';
      } else if (type === 'type') {
        return 'string';
      }

      // Default to sort value
      return data.sort || '';
    }"

  DT::JS(js_code)
} #' Create JavaScript renderer for status badges
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
