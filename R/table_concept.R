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

  statuses <- concept_data$current_accepted
  levels <- clean_column_data(concept_data, config$level_field)
  reference_codes <- concept_data$concept_rf_code
  reference_names <- clean_column_data(concept_data, "concept_rf_label")
  obs_counts <- suppressWarnings(as.numeric(clean_column_data(concept_data, "obs_count", "0")))
  obs_counts[is.na(obs_counts)] <- 0
  descriptions <- clean_column_data(concept_data, config$description_field)
  descriptions <- truncate_text_with_ellipsis(descriptions, max_chars = 680L)
  actions <- create_action_buttons(
    if (concept_type == "plant") "plant_link_click" else "comm_link_click",
    "Details",
    concept_data[[config$code_field]]
  )

  # Build reference links in R using create_detail_link
  ref_links <- vapply(seq_along(reference_codes), function(i) {
    code <- reference_codes[[i]]
    label <- reference_names[[i]]
    if (!is.null(code) && !is.na(code) && nzchar(code)) {
      as.character(create_detail_link("ref_link_click", code, label %|||% code))
    } else {
      htmltools::htmlEscape(label %|||% "Not provided")
    }
  }, character(1))

  status_badges <- vapply(statuses, create_status_badge, character(1))

  result <- data.frame(
    Actions = actions,
    `Vegbank Code` = vapply(concept_codes, htmltools::htmlEscape, character(1)),
    Name = vapply(display_names, htmltools::htmlEscape, character(1)),
    Status = status_badges,
    Level = vapply(levels, htmltools::htmlEscape, character(1)),
    `Reference Source` = ref_links,
    Observations = obs_counts,
    Description = vapply(descriptions, htmltools::htmlEscape, character(1)),
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
    list(targets = 0, width = "10%", orderable = FALSE, searchable = FALSE), # Actions
    list(targets = 1, width = "12%", orderable = TRUE), # Vegbank Code
    list(targets = 2, width = "23%", orderable = TRUE), # Name
    list(targets = 3, width = "10%", className = "dt-center", orderable = FALSE), # Status
    list(targets = 4, width = "10%", className = "dt-center", orderable = FALSE), # Level
    list(targets = 5, width = "10%", orderable = FALSE), # Reference Source
    list(targets = 6, width = "10%", type = "num", className = "dt-right", orderable = TRUE), # Observations
    list(targets = 7, width = "25%", orderable = FALSE) # Description
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

CONCEPT_TABLE_SPECS <- local({
  specs <- lapply(CONCEPT_CONFIG, function(config) {
    schema_template <- build_schema_template(config$fields)
    link_input_id <- if (config$concept_type == "plant") "plant_link_click" else "comm_link_click"

    # Map: col index -> field for sorting
    sort_field_map <- list(
      "1" = "default", # Vegbank Code
      "2" = config$name_field, # Name
      "6" = "obs_count" # Observations
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
        sort_field_map = sort_field_map
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
