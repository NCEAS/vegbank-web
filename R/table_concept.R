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
    # Use orthogonal data objects for Status and Reference Source columns
    # so display and sort/filter values can be provided without hidden columns.
    column_defs = list(
      # Actions
      list(
        targets = 0, width = "10%", orderable = FALSE, searchable = FALSE,
        render = create_action_button_renderer(link_input_id, "Details")
      ),
      # Name
      list(targets = 1, width = "25%"),
      # Status - uses orthogonal data object {display, sort}
      list(
        targets = 2, width = "12%", className = "dt-center",
        render = create_status_badge_renderer()
      ),
      # Reference Source - uses orthogonal data object {display, sort}
      list(
        targets = 3, width = "20%",
        render = create_reference_link_renderer()
      ),
      # Observations
      list(targets = 4, width = "10%", type = "num", className = "dt-right"),
      # Description
      list(targets = 5, width = "28%")
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
  # Build orthogonal status objects with display HTML and sort values
  # Keep the raw boolean values for creating the display & sort entries
  status_raw <- concept_data$current_accepted
  status_cols <- create_status_columns(status_raw)
  status_objects <- mapply(
    function(disp, sortv, filterv) list(display = disp, sort = sortv, filter = filterv),
    status_cols$display, status_cols$sort, status_cols$filter,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  shiny::incProgress(0.15, detail = "Preparing reference data")
  # Build orthogonal reference objects with display HTML and sort values
  reference_cols <- create_reference_columns(concept_data)
  reference_objects <- mapply(
    function(disp, sortv, filterv) list(display = disp, sort = sortv, filter = filterv),
    reference_cols$display, reference_cols$sort, reference_cols$filter,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  shiny::incProgress(0.15, detail = "Cleaning observation counts")
  obs_counts <- as.numeric(clean_column_data(concept_data, "obs_count", "0"))

  shiny::incProgress(0.15, detail = paste0("Cleaning ", concept_type, " descriptions"))
  descriptions <- clean_column_data(concept_data, description_field)

  shiny::incProgress(0.1, detail = "Preparing action codes")
  # Pass the code value directly - will be rendered by JavaScript
  action_codes <- concept_data[[code_field]]

  result <- data.frame(
    "Actions" = action_codes,
    # Status and Reference will be orthogonal list objects (display + sort)
    "Status" = I(status_objects),
    "Reference Source" = I(reference_objects),
    "Observations" = obs_counts,
    "Description" = descriptions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add the name column with the appropriate label
  result <- cbind(
    Actions = result$Actions,
    setNames(data.frame(names, stringsAsFactors = FALSE), name_label),
    result[, c(
      "Status",
      "Reference Source", "Observations", "Description"
    )]
  )

  result
}

#' Create status column data with display HTML, sort values, and filter text
#' This vectorized version seems even more performant than client-side rendering, though that
#' may become necessary if we move to a paginated API.
#'
#' @param status_vector A vector of logical values (TRUE, FALSE, NA)
#' @returns A list with display (HTML badges), sort (numeric), and filter (text) vectors
#' @noRd
create_status_columns <- function(status_vector) {
  # Pre-generate badge HTML strings once for performance
  accepted_badge <- '<span class="badge rounded-pill" style="background-color: var(--accepted-bg); 
                     color: var(--accepted-text);">Accepted</span>'
  not_current_badge <- '<span class="badge rounded-pill" style="background-color: var(--not-current-bg); 
                        color: var(--not-current-text);">Not Current</span>'
  no_status_badge <- '<span class="badge rounded-pill" style="background-color: var(--no-status-bg); 
                      color: var(--no-status-text);">No Status</span>'

  # Vectorized assignment of badges
  display <- ifelse(is.na(status_vector), no_status_badge,
    ifelse(status_vector == TRUE, accepted_badge, not_current_badge)
  )

  # Sort order: Accepted (0) < Not Current (1) < No Status (2)
  sort <- ifelse(is.na(status_vector), 2, ifelse(status_vector == TRUE, 0, 1))

  filter <- ifelse(is.na(status_vector), "No Status",
    ifelse(status_vector == TRUE, "Accepted", "Not Current")
  )

  list(display = display, sort = sort, filter = filter)
}

#' Create reference column data with display HTML and sort values
#'
#' @param concept_data Data frame with concept_rf_code and concept_rf_name columns
#' @returns A list with display (HTML links/spans) and sort (plain text names) vectors
#' @noRd
create_reference_columns <- function(concept_data) {
  # Create reference objects with cleaned names
  reference_data <- create_reference_objects(concept_data)

  # Extract codes and names
  codes <- vapply(reference_data, function(x) x$code, character(1))
  names <- vapply(reference_data, function(x) x$name, character(1))

  # Determine which references should be plain text vs links
  is_not_provided <- names == "Not Provided" | is.na(codes) | codes == ""

  # Create display HTML vectorized
  display <- ifelse(
    is_not_provided,
    sprintf("<span>%s</span>", names),
    sprintf(
      '<a href="#" data-code="%s" onclick="Shiny.setInputValue(\'ref_link_click\', \'%s\', {priority: \'event\'}); return false;">%s</a>',
      codes, codes, names
    )
  )

  list(display = display, sort = names, filter = names)
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

#' Create JavaScript renderer for status badges
#' Renders badges client-side from raw boolean values
#'
#' @returns A DT::JS object containing the JavaScript renderer function
#' @noRd
create_status_badge_renderer <- function() {
  # Now the cell data will be an object: {display: '<span>...</span>', sort: <numeric>, filter: 'Text'}
  js_code <- "function(data, type, row, meta) {
      var fallbackDisplay = '<span class=\"badge rounded-pill\" style=\"background-color: var(--no-status-bg); color: var(--no-status-text);\">No Status</span>';
      var fallbackFilter = 'No Status';
      var fallbackSort = 2;

      if (!data || typeof data !== 'object') {
        if (type === 'display') {
          return fallbackDisplay;
        }
        if (type === 'filter') {
          return fallbackFilter;
        }
        if (type === 'sort' || type === 'type') {
          return fallbackSort;
        }
        return fallbackFilter;
      }

      if (type === 'display') {
        return data.display || fallbackDisplay;
      }

      if (type === 'filter') {
        if (data.filter !== undefined && data.filter !== null) {
          return data.filter;
        }
        if (data.sort !== undefined && data.sort !== null) {
          return data.sort;
        }
        return fallbackFilter;
      }

      if (type === 'sort' || type === 'type') {
        if (data.sort !== undefined && data.sort !== null) {
          return data.sort;
        }
        if (data.filter !== undefined && data.filter !== null) {
          return data.filter;
        }
        return fallbackSort;
      }

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
  # Expect data object: {display: '<a ...>name</a>', sort: 'Name', filter: 'Name'}
  js_code <- "function(data, type, row, meta) {
      var fallbackDisplay = '<span>Not provided</span>';
      var fallbackText = 'Not provided';

      if (!data || typeof data !== 'object') {
        if (type === 'display') {
          return fallbackDisplay;
        }
        return fallbackText;
      }

      if (type === 'display') {
        return data.display || fallbackDisplay;
      }

      if (type === 'filter') {
        if (data.filter !== undefined && data.filter !== null && data.filter !== '') {
          return data.filter;
        }
        if (data.sort !== undefined && data.sort !== null) {
          return data.sort;
        }
        return fallbackText;
      }

      if (type === 'sort' || type === 'type') {
        if (data.sort !== undefined && data.sort !== null) {
          return data.sort;
        }
        if (data.filter !== undefined && data.filter !== null) {
          return data.filter;
        }
        return fallbackText;
      }

      return data;
    }"

  DT::JS(js_code)
}