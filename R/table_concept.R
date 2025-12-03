#' Concept Table Functions Module
#'
#' Provides server-side DataTable builders for plant and community concepts.
#' @noRd
CONCEPT_CONFIG <- list(
  plant = build_table_module_config(
    type = "plant",
    table_id = "plant_table",
    endpoint = "plant-concepts",
    data_key = "plant_data",
    remote_label = "plant concepts",
    fields = c(
      "pc_code",
      "plant_name",
      "current_accepted",
      "plant_level",
      "concept_rf_code",
      "concept_rf_name",
      "obs_count",
      "plant_description"
    ),
    extra = list(
      concept_type = "plant",
      code_field = "pc_code",
      name_field = "plant_name",
      level_field = "plant_level",
      description_field = "plant_description",
      name_label = "Plant Name"
    )
  ),
  community = build_table_module_config(
    type = "community",
    table_id = "comm_table",
    endpoint = "community-concepts",
    data_key = "community_data",
    remote_label = "community concepts",
    fields = c(
      "cc_code",
      "comm_name",
      "current_accepted",
      "comm_level",
      "concept_rf_code",
      "concept_rf_name",
      "obs_count",
      "comm_description"
    ),
    extra = list(
      concept_type = "community",
      code_field = "cc_code",
      name_field = "comm_name",
      level_field = "comm_level",
      description_field = "comm_description",
      name_label = "Community Name"
    )
  )
)

#' Build the concept DataTable for a given concept type
#'
#' @param concept_type Either "plant" or "community"
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_concept_table <- function(concept_type = c("plant", "community")) {
  concept_type <- match.arg(concept_type)
  spec <- CONCEPT_TABLE_SPECS[[concept_type]]
  if (is.null(spec)) {
    stop("No concept table spec registered for type: ", concept_type)
  }

  build_table_from_spec(spec)
}

#' Process concept data into display format
#'
#' @param data_sources List containing concept data keyed by concept type
#' @param concept_type Either "plant" or "community"
#' @returns A data frame ready for display
#' @noRd
process_concept_data <- function(data_sources, concept_type = "plant") {
  config <- get_concept_config(concept_type)

  concept_data <- data_sources
  if (is.list(data_sources) && !is.data.frame(data_sources)) {
    concept_data <- data_sources[[config$data_key]]
  }

  if (is.null(concept_data)) {
    concept_data <- build_schema_template(config$fields)
  }

  if (!is.data.frame(concept_data)) {
    concept_data <- tryCatch(
      as.data.frame(concept_data, stringsAsFactors = FALSE),
      error = function(e) create_empty_source_df(concept_type)
    )
  }

  display_names <- clean_column_data(concept_data, config$name_field)

  status_raw <- concept_data$current_accepted
  status_sort <- ifelse(is.na(status_raw), 2, ifelse(status_raw == TRUE, 0, 1))

  levels <- clean_column_data(concept_data, config$level_field)

  reference_codes <- concept_data$concept_rf_code
  reference_names <- clean_column_data(concept_data, "concept_rf_name")

  obs_counts <- suppressWarnings(as.numeric(clean_column_data(concept_data, "obs_count", "0")))
  obs_counts[is.na(obs_counts)] <- 0

  descriptions <- clean_column_data(concept_data, config$description_field)
  descriptions <- truncate_text_with_ellipsis(descriptions, max_chars = 680L)

  action_codes <- concept_data[[config$code_field]]

  result <- data.frame(
    Actions = action_codes,
    Name = display_names,
    Status = status_raw,
    status_sort = status_sort,
    Level = levels,
    "Reference Source" = reference_codes,
    ref_sort = reference_names,
    Observations = obs_counts,
    Description = descriptions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(result)[names(result) == "Name"] <- config$name_label

  result
}

#' Create column definitions for concept DataTables
#'
#' Defines column-specific rendering, visibility, width, and sorting behavior for concept tables.
#' Includes hidden columns for proper server-side sorting of status badges and reference links.
#'
#' @param detail_input_id The Shiny input ID for the detail button click handler
#' @returns A list of DataTables columnDefs configuration objects
#' @noRd
create_concept_column_defs <- function(detail_input_id) {
  list(
    list(
      targets = 0,
      width = "10%",
      orderable = FALSE,
      searchable = FALSE,
      render = create_action_button_renderer(detail_input_id, "Details")
    ),
    list(targets = 1, width = "25%"),
    list(
      targets = 2,
      width = "10%",
      className = "dt-center",
      render = create_status_badge_renderer(),
      orderData = 3
    ),
    list(targets = 3, visible = FALSE, searchable = FALSE),
    list(targets = 4, width = "10%", className = "dt-center"),
    list(
      targets = 5,
      width = "10%",
      render = create_reference_link_renderer(),
      orderData = 6
    ),
    list(targets = 6, visible = FALSE, searchable = FALSE),
    list(targets = 7, width = "10%", type = "num", className = "dt-right"),
    list(targets = 8, width = "25%")
  )
}

#' Normalize and validate concept data from API response
#'
#' Ensures API response data contains all required fields with proper types.
#' Uses shared normalization helper with special handling for logical and numeric fields.
#'
#' @param df A data frame (or coercible object) from the VegBank API
#' @param concept_type Either "plant" or "community"
#' @returns A validated and normalized data frame with all expected columns
#' @noRd
normalize_concepts <- function(df, concept_type) {
  config <- get_concept_config(concept_type)
  schema_template <- build_schema_template(config$fields)
  
  normalized <- normalize_table_data(df, schema_template)
  
  # Special handling for fields that need type coercion beyond API defaults
  if ("current_accepted" %in% names(normalized)) {
    normalized$current_accepted <- parse_logical_vector(normalized$current_accepted)
  }
  if ("obs_count" %in% names(normalized)) {
    normalized$obs_count[is.na(normalized$obs_count)] <- 0
  }
  
  normalized
}

#' Coerce API response into a data frame
#'
#' Uses shared coercion helper with concept-specific empty template.
#'
#' @param parsed The parsed API response object
#' @param concept_type Either "plant" or "community"
#' @returns A data frame, or an empty concept-specific data frame on failure
#' @noRd
coerce_api_page <- function(parsed, concept_type) {
  config <- get_concept_config(concept_type)
  schema_template <- build_schema_template(config$fields)
  coerce_api_response(parsed, schema_template)
}

#' Retrieve configuration for a given concept type
#'
#' Looks up the configuration list from CONCEPT_CONFIG for the specified concept type.
#' Throws an error if the concept type is not recognized.
#'
#' @param concept_type Either "plant" or "community"
#' @returns The configuration list for the specified concept type
#' @noRd
get_concept_config <- function(concept_type) {
  config <- CONCEPT_CONFIG[[concept_type]]
  if (is.null(config)) {
    stop("Unsupported concept type: ", concept_type)
  }
  config
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
  # Use JSON.stringify to safely encode the data value
  js_code <- sprintf(
    "function(data, type, row, meta) {
      if (type === 'display') {
        if (!data || data === '') return '<span>No Data</span>';
        
        return '<div class=\"btn-group btn-group-sm\">' +
               '<button class=\"btn btn-sm btn-outline-primary\" ' +
               \"onclick='Shiny.setInputValue(\" + JSON.stringify('%s') + ', ' + JSON.stringify(data) + \", {priority: \\\"event\\\"}); return false;'>\" +
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
        var name = row && row.length > 6 ? row[6] : null;

        if (name === null || name === undefined) {
          name = '';
        }
        name = String(name);
        if (name.trim() === '') {
          name = 'Not provided';
        }

        // If \"Not provided\" or no code, show as plain text
        if (name === 'Not provided' || !code || code === '') {
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

CONCEPT_TABLE_SPECS <- local({
  specs <- lapply(CONCEPT_CONFIG, function(config) {
    schema_template <- build_schema_template(config$fields)
    link_input_id <- if (config$concept_type == "plant") "plant_link_click" else "comm_link_click"

    list(
      table_id = config$table_id,
      endpoint = config$endpoint,
      remote_label = config$remote_label,
      column_defs = create_concept_column_defs(link_input_id),
      schema_fields = config$fields,
      schema_template = schema_template,
      coerce_fn = function(parsed) coerce_api_page(parsed, config$concept_type),
      normalize_fn = function(data) normalize_concepts(data, config$concept_type),
      display_fn = function(normalized) process_concept_data(normalized, concept_type = config$concept_type),
      data_source = list(
        page_length = config$page_length %||% TABLE_PAGE_LENGTH,
        clean_rows_fn = sanitize_dt_rows,
        count_clean_names = FALSE
      ),
      page_length = config$page_length,
      options = list(),
      datatable_args = list(),
      initial_display = process_concept_data(schema_template, concept_type = config$concept_type)
    )
  })

  names(specs) <- names(CONCEPT_CONFIG)
  specs
})
