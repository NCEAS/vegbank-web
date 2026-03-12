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
    loading_label = "plant concepts",
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
    loading_label = "community concepts",
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

#' Build concept DataTable with optional citation filter
#'
#' For citation filters, directly fetches and displays the single cited entity.
#'
#' @param concept_type Either "plant" or "community"
#' @param vb_code Optional VegBank code for citation filtering (e.g., "cc.1234")
#' @param filter_type Optional filter type ("single-entity-citation")
#' @return A DT::datatable object
#' @noRd
build_concept_table_with_filter <- function(concept_type = c("plant", "community"), 
                                            vb_code = NULL, 
                                            filter_type = NULL) {
  concept_type <- match.arg(concept_type)
  spec <- CONCEPT_TABLE_SPECS[[concept_type]]
  if (is.null(spec)) {
    stop("No concept table spec registered for type: ", concept_type)
  }

  has_filter <- !is.null(vb_code) && !is.na(vb_code) && nzchar(vb_code)

  # For citation filters, fetch the specific concept directly
  if (has_filter && !is.null(filter_type) && filter_type == "single-entity-citation") {
    tryCatch(
      {
        # Fetch the single concept using the appropriate API function
        # Concept endpoints require detail = "full" (minimal not supported)
        concept_data <- if (concept_type == "community") {
          vegbankr::vb_get_community_concepts(vb_code, detail = "full", with_nested = FALSE)
        } else {
          vegbankr::vb_get_plant_concepts(vb_code, detail = "full", with_nested = FALSE)
        }

        if (!is.null(concept_data) && nrow(concept_data) > 0) {
          # Process through the same pipeline as AJAX
          coerced <- spec$coerce_fn(concept_data)
          normalized <- spec$normalize_fn(coerced)
          display_data <- spec$display_fn(normalized)

          # Build static table (no AJAX) with the single row
          static_config <- list(
            initial_data = display_data,
            column_defs = spec$column_defs,
            page_length = 1L,
            options = utils::modifyList(
              spec$options %||% list(),
              list(
                scrollY = "calc(100vh - 250px)",  # Accommodate citation alert notification
                serverSide = FALSE,
                searching = FALSE,
                paging = FALSE
              )
            ),
            datatable_args = spec$datatable_args,
            escape = FALSE
          )

          return(create_table(static_config))
        } else {
          shiny::showNotification(
            paste("Could not load cited", concept_type, "concept:", vb_code),
            type = "warning"
          )
        }
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error loading cited", concept_type, "concept:", conditionMessage(e)),
          type = "error"
        )
      }
    )
    # Fall through to normal AJAX table if fetch fails
  }

  # No filter or fetch failed - return normal AJAX table
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

  display_names <- clean_column_data(concept_data, config$name_field)
  concept_codes <- concept_data[[config$code_field]]

  statuses <- concept_data$current_accepted
  levels <- clean_column_data(concept_data, config$level_field)
  reference_codes <- concept_data$concept_rf_code
  reference_names <- clean_column_data(concept_data, "concept_rf_label")
  obs_counts <- suppressWarnings(as.numeric(clean_column_data(concept_data, "obs_count", "0")))
  obs_counts[is.na(obs_counts)] <- 0
  descriptions <- clean_column_data(concept_data, config$description_field)
  detail_input_id <- if (concept_type == "plant") "plant_link_click" else "comm_link_click"
  actions <- create_action_buttons(
    detail_input_id,
    "Details",
    concept_data[[config$code_field]]
  )

  # Create clickable obs_count links for cross-resource filtering
  obs_count_links <- create_all_obs_count_links(
    obs_counts,
    concept_codes,
    display_names
  )

  # Build reference links in R using create_detail_link
  ref_links <- vapply(seq_along(reference_codes), function(i) {
    code <- reference_codes[[i]]
    label <- reference_names[[i]]
    if (!is.null(code) && !is.na(code) && nzchar(code)) {
      as.character(create_detail_link("ref_link_click", code, label %|||% code))
    } else {
      htmltools::htmlEscape(label %|||% "Unspecified")
    }
  }, character(1))

  status_badges <- vapply(statuses, create_status_badge, character(1))

  result <- data.frame(
    Actions = actions,
    `VegBank Code` = vapply(concept_codes, htmltools::htmlEscape, character(1)),
    Name = vapply(display_names, htmltools::htmlEscape, character(1)),
    Status = status_badges,
    Level = vapply(levels, htmltools::htmlEscape, character(1)),
    `Reference Source` = ref_links,
    Plots = obs_count_links,
    Description = vapply(seq_along(descriptions), function(i) {
      safe_desc <- htmltools::htmlEscape(descriptions[i])
      safe_code <- htmltools::htmlEscape(as.character(concept_codes[[i]]), attribute = TRUE)
      sprintf(
        '<div class="dt-description-container"><div class="dt-description">%s</div><a href="#" class="dt-read-more dt-shiny-action" data-input-id="%s" data-value="%s">Read more</a></div>',
        safe_desc, detail_input_id, safe_code
      )
    }, character(1)),
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
    list(targets = 1, width = "12%", orderable = TRUE), # VegBank Code
    list(targets = 2, width = "23%", orderable = TRUE), # Name
    list(targets = 3, width = "10%", className = "dt-center", orderable = FALSE), # Status
    list(targets = 4, width = "10%", className = "dt-center", orderable = FALSE), # Level
    list(targets = 5, width = "10%", orderable = FALSE), # Reference Source
    list(targets = 6, width = "10%", type = "num", className = "dt-right", orderable = TRUE), # Plots
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

.PLANT_TABLE_HELP_CONTENT <- local({
  html <- as.character(htmltools::tagList(
    htmltools::tags$p("This table lists plant taxa in VegBank's taxonomic concept hierarchy. Each row is a single plant concept."),
    htmltools::tags$ul(
      htmltools::tags$li(htmltools::tags$strong("Search:"), " use the search box to filter by plant name or VegBank code."),
      htmltools::tags$li(htmltools::tags$strong("Show plots:"), " click the number in the Plots column to filter the Plots table to observations containing this taxon."),
      htmltools::tags$li(htmltools::tags$strong("Sort:"), " click a column header to sort; Name and Plots support sorting. Sort by multiple columns by holding shift and clicking multiple headers."),
      htmltools::tags$li(htmltools::tags$strong("Open details:"), " the Details button in the Actions column opens additional information about the plant in an overlay."),
      htmltools::tags$li(htmltools::tags$strong("Status:"), " indicates whether a concept is currently accepted, not current, or doesn't have a status."),
      htmltools::tags$li(htmltools::tags$strong("Level:"), " the taxonomic rank (species, genus, family, etc.)."),
      htmltools::tags$li(htmltools::tags$strong("Reference:"), " the authoritative classification source for this concept."),
    )
  ))
  html <- gsub("\n", "", html, fixed = TRUE)
  gsub("'", "\\'", html, fixed = TRUE)
})

.COMMUNITY_TABLE_HELP_CONTENT <- local({
  html <- as.character(htmltools::tagList(
    htmltools::tags$p("This table lists vegetation community concepts in VegBank's classification hierarchy. Each row is a single community type."),
    htmltools::tags$ul(
      htmltools::tags$li(htmltools::tags$strong("Search:"), " use the search box to filter by name, code, VegBank code, or description"),
      htmltools::tags$li(htmltools::tags$strong("Show plots:"), " click the number in the Plots column to filter the Plots table to observations classified to this community."),
      htmltools::tags$li(htmltools::tags$strong("Sort:"), " click a column header to sort; Name and Plots support sorting. Sort by multiple columns by holding shift and clicking multiple headers."),
      htmltools::tags$li(htmltools::tags$strong("Open details:"), " the Details button in the Actions column opens additional information about the community in an overlay."),
      htmltools::tags$li(htmltools::tags$strong("Status:"), " indicates whether a concept is currently accepted, not current, or doesn't have a status."),
      htmltools::tags$li(htmltools::tags$strong("Level:"), " the vegetation hierarchy level (class, group, formation, alliance, association, etc.)."),
      htmltools::tags$li(htmltools::tags$strong("Reference:"), " the authoritative classification source for this concept."),
    )
  ))
  html <- gsub("\n", "", html, fixed = TRUE)
  gsub("'", "\\'", html, fixed = TRUE)
})

CONCEPT_TABLE_SPECS <- local({
  specs <- lapply(CONCEPT_CONFIG, function(config) {
    schema_template <- build_schema_template(config$fields)
    link_input_id <- if (config$concept_type == "plant") "plant_link_click" else "comm_link_click"

    # Map: col index -> field for sorting
    sort_field_map <- list(
      "1" = "default", # VegBank Code
      "2" = config$name_field, # Name
      "6" = "obs_count" # Plots
    )

    list(
      table_id = config$table_id,
      resource = config$resource,
      loading_label = config$loading_label,
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
      options = list(
        dom = "Bfrtip",
        buttons = I(list(make_help_button_js(
          if (config$concept_type == "plant") "Plants Table" else "Communities Table",
          if (config$concept_type == "plant") .PLANT_TABLE_HELP_CONTENT else .COMMUNITY_TABLE_HELP_CONTENT
        )))
      ),
      datatable_args = list(extensions = "Buttons"),
      initial_display = process_concept_data(schema_template, concept_type = config$concept_type)
    )
  })

  names(specs) <- names(CONCEPT_CONFIG)
  specs
})
