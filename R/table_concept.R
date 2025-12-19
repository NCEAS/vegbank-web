#' Concept Table Functions Module
#'
#' Provides server-side DataTable builders for plant and community concepts.
#' @noRd
CONCEPT_CONFIG <- list(
  plant = build_table_module_config(
    type = "plant",
    table_id = "plant_table",
    resource = "plant-concepts",
    data_key = "plant_data",
    remote_label = "plant concepts",
    fields = c(
      "pc_code",
      "plant_name",
      "current_accepted",
      "plant_level",
      "concept_rf_code",
      "concept_rf_label",
      "obs_count",
      "plant_description"
    ),
    extra = list(
      concept_type = "plant",
      code_field = "pc_code",
      name_field = "plant_name",
      level_field = "plant_level",
      description_field = "plant_description",
      name_label = "Plant Concept"
    )
  ),
  community = build_table_module_config(
    type = "community",
    table_id = "comm_table",
    resource = "community-concepts",
    data_key = "community_data",
    remote_label = "community concepts",
    fields = c(
      "cc_code",
      "comm_name",
      "current_accepted",
      "comm_level",
      "concept_rf_code",
      "concept_rf_label",
      "obs_count",
      "comm_description"
    ),
    extra = list(
      concept_type = "community",
      code_field = "cc_code",
      name_field = "comm_name",
      level_field = "comm_level",
      description_field = "comm_description",
      name_label = "Community Concept"
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
  concept_codes <- concept_data[[config$code_field]]

  # Name column is now just the name (no code)
  formatted_names <- display_names

  status_raw <- concept_data$current_accepted
  status_sort <- ifelse(is.na(status_raw), 2, ifelse(status_raw == TRUE, 0, 1))

  levels <- clean_column_data(concept_data, config$level_field)

  reference_codes <- concept_data$concept_rf_code
  reference_names <- clean_column_data(concept_data, "concept_rf_label")

  obs_counts <- suppressWarnings(as.numeric(clean_column_data(concept_data, "obs_count", "0")))
  obs_counts[is.na(obs_counts)] <- 0

  descriptions <- clean_column_data(concept_data, config$description_field)
  descriptions <- truncate_text_with_ellipsis(descriptions, max_chars = 680L)

  action_codes <- concept_data[[config$code_field]]

  result <- data.frame(
    Actions = action_codes,
    `Vegbank Code` = concept_codes,
    Name = formatted_names,
    Status = status_raw,
    status_sort = status_sort,
    Level = levels,
    `Reference Source` = reference_codes,
    ref_sort = reference_names,
    Observations = obs_counts,
    Description = descriptions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(result)[names(result) == "Name"] <- config$name_label

  result
}

#' Build HTML-friendly name string with concept code
#'
#' Creates multi-line HTML name strings with the concept name on top and
#' the concept code (pc_code or cc_code) in green below.
#'
#' @param names Character vector of concept names
#' @param codes Character vector of concept codes (pc_code or cc_code)
#' @return Character vector of HTML-formatted name strings
#' @noRd
format_concept_name_column <- function(names, codes) {
  vapply(seq_along(names), function(idx) {
    name <- names[[idx]]
    code <- codes[[idx]]

    lines <- character(0)

    if (!is.null(name) && !is.na(name) && nzchar(name)) {
      lines <- c(lines, as.character(htmltools::htmlEscape(name)))
    } else {
      lines <- c(lines, "Not provided")
    }

    if (!is.null(code) && !is.na(code) && nzchar(code)) {
      code_line <- sprintf(
        '<span style="color: #2c5443; font-size: small;">%s</span>',
        as.character(htmltools::htmlEscape(code))
      )
      lines <- c(lines, code_line)
    }

    paste(lines, collapse = "<br>")
  }, character(1), USE.NAMES = FALSE)
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
    ), # Actions
    list(targets = 1, width = "12%", orderable = TRUE), # Vegbank Code
    list(targets = 2, width = "23%", orderable = TRUE), # Name
    list(
      targets = 3,
      width = "10%",
      className = "dt-center",
      render = create_status_badge_renderer(),
      orderable = FALSE
    ), # Status
    list(targets = 4, visible = FALSE, searchable = FALSE), # Hidden sort column for status badges
    list(targets = 5, width = "10%", className = "dt-center", orderable = FALSE), # Level
    list(
      targets = 6,
      width = "10%",
      render = create_reference_link_renderer(),
      orderable = FALSE
    ), # Reference Source
    list(targets = 7, visible = FALSE, searchable = FALSE), # Hidden sort column for reference names
    list(targets = 8, width = "10%", type = "num", className = "dt-right", orderable = TRUE), # Observations
    list(targets = 9, width = "25%", orderable = FALSE) # Description
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

  # Custom transform for concept-specific type handling
  parse_current_accepted <- function(normalized) {
    if ("current_accepted" %in% names(normalized)) {
      normalized$current_accepted <- parse_logical_vector(normalized$current_accepted)
    }
    normalized
  }

  create_normalizer(
    schema_template,
    na_to_zero_fields = "obs_count",
    custom_transforms = list(parse_current_accepted)
  )(df)
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
  create_coercer(schema_template)(parsed)
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
        if (!data || data === '') {
          return '<span class=\"text-muted\">No Data</span>';
        }

        var inputId = %s;
        var label = %s;

        var escapeAttr = function(value) {
          return String(value)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\"/g, '&quot;')
            .replace(/'/g, '&#39;');
        };

        var safeValue = escapeAttr(data);
        var safeId = escapeAttr(inputId);
        var safeLabel = escapeAttr(label);

        return '<div class=\"btn-group btn-group-sm\" role=\"group\">' +
               '<button type=\"button\" class=\"btn btn-sm btn-outline-primary dt-shiny-action\" ' +
               'data-input-id=\"' + safeId + '\" data-value=\"' + safeValue + '\">' + safeLabel + '</button>' +
               '</div>';
      }
      return data;
    }",
    jsonlite::toJSON(input_id, auto_unbox = TRUE),
    jsonlite::toJSON(button_label, auto_unbox = TRUE)
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
        var escapeHtml = function(value) {
          return String(value)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\"/g, '&quot;')
            .replace(/'/g, '&#39;');
        };

        var escapeAttr = function(value) {
          return String(value)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\"/g, '&quot;')
            .replace(/'/g, '&#39;');
        };

        // data is the reference code; the display name is in the hidden sort column (index 7)
        var code = data === null || typeof data === 'undefined' ? '' : String(data);
        var name = row && row.length > 7 ? row[7] : '';

        if (name === null || name === undefined) {
          name = '';
        }

        name = String(name);
        if (name.trim() === '') {
          name = 'Not provided';
        }

        var safeName = escapeHtml(name);

        // If \"Not provided\" or no code, show as plain text
        if (name === 'Not provided' || !code) {
          return '<span>' + safeName + '</span>';
        }

        var safeCodeAttr = escapeAttr(code);
        return '<a href=\"#\" class=\"dt-shiny-action\" data-input-id=\"ref_link_click\" data-value=\"' +
               safeCodeAttr + '\">' + safeName + '</a>';
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

    # Map: col index -> field for sorting
    sort_field_map <- list(
      "1" = "default", # Vegbank Code
      "2" = config$name_field, # Name
      "8" = "obs_count" # Observations
    )

    list(
      table_id = config$table_id,
      resource = config$resource,
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
        count_clean_names = FALSE,
        sort_field_map = sort_field_map,
        default_order = list(list(column = 2, dir = "asc")) # Default: Vegbank Code
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
