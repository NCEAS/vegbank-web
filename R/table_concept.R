#' Concept Table Functions Module
#'
#' Provides server-side DataTable builders for plant and community concepts.

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

clean_dt_frame <- get("cleanDataFrame", envir = asNamespace("DT"))

#' Build the concept DataTable for a given concept type
#'
#' @param concept_type Either "plant" or "community"
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_concept_table <- function(concept_type = c("plant", "community")) {
  concept_type <- match.arg(concept_type)
  config <- get_concept_config(concept_type)

  table_config <- create_concept_table_config(config)

  create_table(
    data_sources = list(),
    required_sources = character(0),
    process_function = NULL,
    table_config = table_config
  )
}

#' Process concept data into display format
#'
#' @param data_sources List containing concept data keyed by concept type
#' @param concept_type Either "plant" or "community"
#' @returns A data frame ready for display
#' @noRd
process_concept_data <- function(data_sources, concept_type = "plant") {
  config <- get_concept_config(concept_type)

  concept_data <- data_sources[[config$data_key]]
  if (is.null(concept_data)) {
    concept_data <- create_empty_source_df(concept_type)
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

#' Build table configuration for remote concept data tables
#'
#' Creates the complete configuration object needed for server-side concept tables,
#' including column definitions, AJAX setup, and remote data options.
#'
#' @param config A concept configuration list from CONCEPT_CONFIG
#' @returns A list containing table_config elements for create_table()
#' @noRd
create_concept_table_config <- function(config) {
  link_input_id <- if (config$concept_type == "plant") "plant_link_click" else "comm_link_click"

  column_defs <- create_concept_column_defs(link_input_id)
  empty_source <- create_empty_source_df(config$concept_type)
  empty_sources <- setNames(list(empty_source), config$data_key)
  initial_display <- process_concept_data(empty_sources, concept_type = config$concept_type)

  data_source_spec <- build_data_source_spec(
    table_id = config$table_id,
    endpoint = config$endpoint,
    coerce_fn = function(parsed) coerce_api_page(parsed, config$concept_type),
    normalize_fn = function(data) normalize_concepts(data, config$concept_type),
    display_fn = function(normalized) {
      data_sources <- setNames(list(normalized), config$data_key)
      process_concept_data(data_sources, concept_type = config$concept_type)
    },
    label = config$remote_label,
    schema_fields = config$fields,
    empty_factory = function() create_empty_source_df(config$concept_type),
    page_length = config$page_length %||% TABLE_PAGE_LENGTH,
    clean_rows_fn = clean_dt_frame,
    count_clean_names = FALSE
  )

  build_remote_table_config(
    column_defs = column_defs,
    initial_data = initial_display,
    data_source_spec = data_source_spec,
    remote_label = config$remote_label,
    page_length = config$page_length,
    options = list()
  )
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
#' Fills missing fields with NA, coerces types, and ensures consistent structure
#' for downstream processing. Returns an empty data frame if input is invalid.
#'
#' @param df A data frame (or coercible object) from the VegBank API
#' @param concept_type Either "plant" or "community"
#' @returns A validated and normalized data frame with all expected columns
#' @noRd
normalize_concepts <- function(df, concept_type) {
  if (is.null(df)) {
    return(create_empty_source_df(concept_type))
  }
  if (!is.data.frame(df)) {
    df <- tryCatch(
      as.data.frame(df, stringsAsFactors = FALSE),
      error = function(e) create_empty_source_df(concept_type)
    )
  }
  if (!nrow(df)) {
    return(create_empty_source_df(concept_type))
  }

  config <- get_concept_config(concept_type)
  missing_fields <- setdiff(config$fields, names(df))
  for (field in missing_fields) {
    df[[field]] <- NA_character_
  }

  df <- df[, config$fields, drop = FALSE]

  df[[config$code_field]] <- as.character(df[[config$code_field]])
  df[[config$name_field]] <- as.character(df[[config$name_field]])
  df[[config$level_field]] <- as.character(df[[config$level_field]])
  df[[config$description_field]] <- as.character(df[[config$description_field]])
  df$concept_rf_code <- as.character(df$concept_rf_code)
  df$concept_rf_name <- as.character(df$concept_rf_name)
  df$current_accepted <- parse_logical_vector(df$current_accepted)
  suppressWarnings(df$obs_count <- as.numeric(df$obs_count))
  df$obs_count[is.na(df$obs_count)] <- 0

  rownames(df) <- NULL
  df
}

#' Create an empty data frame with concept-specific schema
#'
#' Generates a zero-row data frame with all required columns for a given concept type.
#' Used as a template for empty tables and to ensure consistent structure.
#'
#' @param concept_type Either "plant" or "community"
#' @returns A zero-row data frame with all expected concept columns
#' @noRd
create_empty_source_df <- function(concept_type) {
  config <- get_concept_config(concept_type)
  build_zero_row_df(config$fields)
}

#' Coerce API response into a data frame
#'
#' Recursively extracts and converts various API response structures into a data frame.
#' Handles nested lists, single-element wrappers, and already-formatted data frames.
#' Returns an empty concept-specific data frame if coercion fails.
#'
#' @param parsed The parsed API response object
#' @param concept_type Either "plant" or "community"
#' @returns A data frame, or an empty concept-specific data frame on failure
#' @noRd
coerce_api_page <- function(parsed, concept_type) {
  if (is.null(parsed)) {
    return(create_empty_source_df(concept_type))
  }
  if (is.data.frame(parsed)) {
    return(parsed)
  }
  if (is.list(parsed)) {
    if (!is.null(parsed$data)) {
      return(coerce_api_page(parsed$data, concept_type))
    }
    if (length(parsed) == 1) {
      return(coerce_api_page(parsed[[1]], concept_type))
    }
  }
  tryCatch(
    as.data.frame(parsed, stringsAsFactors = FALSE),
    error = function(e) create_empty_source_df(concept_type)
  )
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
