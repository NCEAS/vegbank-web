#' Concept Table Functions Module
#'
#' Provides generalized functions for creating and manipulating concept data tables
#' (both plant and community concepts).

PLANT_TABLE_PAGE_LENGTH <- 100L
PLANT_TABLE_FIELDS <- c(
  "pc_code",
  "plant_name",
  "current_accepted",
  "plant_level",
  "concept_rf_code",
  "concept_rf_name",
  "obs_count",
  "plant_description"
)
PLANT_COUNT_CACHE <- new.env(parent = emptyenv())
SUPPORTS_REMOTE_SEARCH <- TRUE

empty_plant_source_df <- {
  cols <- stats::setNames(rep(list(character()), length(PLANT_TABLE_FIELDS)), PLANT_TABLE_FIELDS)
  as.data.frame(cols, stringsAsFactors = FALSE)
}

clean_dt_frame <- get("cleanDataFrame", envir = asNamespace("DT"))

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

  table_config <- list(
    # Resorted to hidden columns to get proper sorting behavior for status badge and reference source
    # columns because server-side processing with R's DT library (which is recommended for data
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
        targets = 2, width = "10%", className = "dt-center",
        render = create_status_badge_renderer(), orderData = 3
      ),
      list(targets = 3, visible = FALSE, searchable = FALSE), # Hidden: status sort values (0, 1, 2)
      # Level column (plant_level or comm_level)
      list(targets = 4, width = "10%", className = "dt-center"),
      # Reference Source - render client-side, sort by hidden column
      list(
        targets = 5, width = "20%",
        render = create_reference_link_renderer(), orderData = 6
      ),
      list(targets = 6, visible = FALSE, searchable = FALSE), # Hidden: reference names for sorting
      # Observations
      list(targets = 7, width = "10%", type = "num", className = "dt-right"),
      # Description
      list(targets = 8, width = "15%")
    ),
    progress_message = paste0("Processing ", concept_type, " concepts table data")
  )

  if (is_plant && !is.null(shiny::getDefaultReactiveDomain())) {
    table_config$page_length <- PLANT_TABLE_PAGE_LENGTH
    return(build_remote_plant_concept_table(table_config))
  }

  data_sources <- list()
  data_sources[[data_key]] <- concept_data
  required_sources <- c(data_key)

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
process_concept_data <- function(data_sources, concept_type = "plant", use_progress = TRUE) {
  is_plant <- concept_type == "plant"

  # Get the data from the appropriate key
  data_key <- if (is_plant) "plant_data" else "community_data"
  concept_data <- data_sources[[data_key]]

  # Determine field names based on concept type
  name_field <- if (is_plant) "plant_name" else "comm_name"
  description_field <- if (is_plant) "plant_description" else "comm_description"
  code_field <- if (is_plant) "pc_code" else "cc_code"
  level_field <- if (is_plant) "plant_level" else "comm_level"
  name_label <- if (is_plant) "Plant Name" else "Community Name"

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = paste0("Cleaning ", concept_type, " names"))
  }
  names <- clean_column_data(concept_data, name_field)

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.1, detail = "Preparing status data")
  }
  status_raw <- concept_data$current_accepted
  # Sort order: Accepted (0) < Not Current (1) < No Status (2)
  status_sort <- ifelse(is.na(status_raw), 2, ifelse(status_raw == TRUE, 0, 1))

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.1, detail = paste0("Cleaning ", concept_type, " levels"))
  }
  levels <- clean_column_data(concept_data, level_field)

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = "Preparing reference data")
  }
  # Pass reference codes and names for client-side rendering
  reference_codes <- concept_data$concept_rf_code
  reference_names <- clean_column_data(concept_data, "concept_rf_name")

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = "Cleaning observation counts")
  }
  obs_counts <- as.numeric(clean_column_data(concept_data, "obs_count", "0"))

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = paste0("Cleaning ", concept_type, " descriptions"))
  }
  descriptions <- clean_column_data(concept_data, description_field)

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.1, detail = "Preparing action codes")
  }
  # Pass the code value directly - will be rendered by JavaScript
  action_codes <- concept_data[[code_field]]

  result <- data.frame(
    Actions = action_codes,
    Name = names,
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

  # Insert the appropriate label for the Name column
  names(result)[names(result) == "Name"] <- name_label

  result
}

build_remote_plant_concept_table <- function(table_config) {
  page_length <- table_config$page_length %||% PLANT_TABLE_PAGE_LENGTH

  initial_display <- process_concept_data(
    list(plant_data = empty_plant_source_df[FALSE, , drop = FALSE]),
    concept_type = "plant",
    use_progress = FALSE
  )

  table_config$use_progress <- FALSE
  table_config$initial_data <- initial_display
  table_config$page_length <- page_length

  default_remote_options <- list(
    serverSide = TRUE,
    lengthChange = FALSE,
    ordering = FALSE,
    searching = TRUE,
    language = list(processing = "Loading plant concepts...")
  )

  table_config$options <- if (is.null(table_config$options)) {
    default_remote_options
  } else {
    utils::modifyList(default_remote_options, table_config$options)
  }

  table_config$ajax <- function(session) {
    if (is.null(session)) {
      stop("A Shiny session is required to initialize the plant concept table.")
    }

    if (is.null(session$userData$plant_total_cache)) {
      session$userData$plant_total_cache <- new.env(parent = emptyenv())
      session$userData$plant_total_cache$count <- NULL
    }

    total_cache <- session$userData$plant_total_cache

    remote_filter <- function(data, params) {
      draw <- as.integer(params$draw %||% 0L)

      offset <- as.integer(params$start %||% 0L)
      if (is.na(offset) || offset < 0L) {
        offset <- 0L
      }

      search_term <- NULL
      if (!is.null(params$search)) {
        search_term <- normalize_search_term(params$search$value)
      }
      if (!SUPPORTS_REMOTE_SEARCH) {
        search_term <- NULL
      }

      page <- fetch_plants_page(limit = page_length, offset = offset, search = search_term)
      normalized <- normalize_plants(page$data)

      display_rows <- process_concept_data(
        list(plant_data = normalized),
        concept_type = "plant",
        use_progress = FALSE
      )

      details <- page$details
      reported_total <- extract_reported_total(details)
      if (is.na(reported_total)) {
        reported_total <- fetch_plants_count(search_term)
      }

      if (is.null(search_term) && !is.na(reported_total)) {
        total_cache$count <- reported_total
      }

      total_records <- total_cache$count
      if (is.null(total_records) || !length(total_records) || is.na(total_records)) {
        total_records <- fetch_plants_count(NULL)
      }
      if (is.null(total_records) || !length(total_records) || is.na(total_records)) {
        total_records <- nrow(normalized)
      }
      total_records <- as.integer(round(total_records))

      filtered_records <- reported_total
      if (is.null(filtered_records) || !length(filtered_records) || is.na(filtered_records)) {
        filtered_records <- if (is.null(search_term)) total_records else nrow(normalized)
      }
      filtered_records <- as.integer(round(filtered_records))

      list(
        draw = draw,
        recordsTotal = total_records,
        recordsFiltered = filtered_records,
        data = clean_dt_frame(display_rows)
      )
    }

    ajax_url <- DT::dataTableAjax(
      session = session,
      data = initial_display,
      filter = remote_filter,
      outputId = "plant_table"
    )

    list(url = ajax_url)
  }

  create_table(
    data_sources = list(),
    required_sources = character(0),
    process_function = NULL,
    table_config = table_config
  )
}

normalize_search_term <- function(value) {
  term <- value %||% ""
  term <- httpuv::decodeURIComponent(term)
  term <- trimws(term)
  if (!nzchar(term)) {
    return(NULL)
  }
  term
}

extract_reported_total <- function(details) {
  if (is.null(details)) {
    return(NA_integer_)
  }
  val <- details["count_reported"]
  if (is.null(val) || !length(val)) {
    return(NA_integer_)
  }
  suppressWarnings(total <- as.numeric(val[1]))
  if (is.na(total)) {
    return(NA_integer_)
  }
  as.integer(round(total))
}

parse_logical_vector <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  parsed <- rep(NA, length(x))
  lower <- tolower(as.character(x))
  parsed[lower %in% c("true", "t", "1")] <- TRUE
  parsed[lower %in% c("false", "f", "0")] <- FALSE
  parsed
}

normalize_plants <- function(df) {
  if (is.null(df)) {
    return(empty_plant_source_df[FALSE, ])
  }
  if (!is.data.frame(df)) {
    df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) empty_plant_source_df[FALSE, ])
  }
  if (!nrow(df)) {
    return(empty_plant_source_df[FALSE, ])
  }

  missing_fields <- setdiff(PLANT_TABLE_FIELDS, names(df))
  for (field in missing_fields) {
    df[[field]] <- NA_character_
  }

  df <- df[, PLANT_TABLE_FIELDS, drop = FALSE]

  df$pc_code <- as.character(df$pc_code)
  df$plant_name <- as.character(df$plant_name)
  df$plant_level <- as.character(df$plant_level)
  df$concept_rf_code <- as.character(df$concept_rf_code)
  df$concept_rf_name <- as.character(df$concept_rf_name)
  df$plant_description <- as.character(df$plant_description)
  df$current_accepted <- parse_logical_vector(df$current_accepted)
  suppressWarnings(df$obs_count <- as.numeric(df$obs_count))
  df$obs_count[is.na(df$obs_count)] <- 0

  rownames(df) <- NULL
  df
}

coerce_api_page <- function(parsed) {
  if (is.null(parsed)) {
    return(empty_plant_source_df[FALSE, ])
  }
  if (is.data.frame(parsed)) {
    return(parsed)
  }
  if (is.list(parsed)) {
    if (!is.null(parsed$data)) {
      return(coerce_api_page(parsed$data))
    }
    if (length(parsed) == 1) {
      return(coerce_api_page(parsed[[1]]))
    }
  }
  tryCatch(
    as.data.frame(parsed, stringsAsFactors = FALSE),
    error = function(e) empty_plant_source_df[FALSE, ]
  )
}

fetch_plants_page <- function(limit, offset, search = NULL) {
  vb_result <- try(
    suppressWarnings(
      vegbankr:::get_all_resources(
        "plant-concepts",
        limit = limit,
        offset = offset,
        detail = "full",
        parquet = FALSE,
        search = search
      )
    ),
    silent = TRUE
  )

  if (inherits(vb_result, "try-error")) {
    vb_error <- attr(vb_result, "condition")
    warning(
      "vegbankr:::get_all_resources failed: ",
      if (!is.null(vb_error)) conditionMessage(vb_error) else "unknown error"
    )
    return(list(data = empty_plant_source_df[FALSE, ], details = NULL))
  }

  details <- tryCatch(vegbankr::get_page_details(vb_result), error = function(e) NULL)
  list(data = coerce_api_page(vb_result), details = details)
}

fetch_plants_count <- function(search = NULL) {
  key <- if (is.null(search)) "__all__" else search
  if (exists(key, envir = PLANT_COUNT_CACHE, inherits = FALSE)) {
    return(get(key, envir = PLANT_COUNT_CACHE, inherits = FALSE))
  }

  vb_result <- try(
    suppressWarnings(
      vegbankr:::get_all_resources(
        "plant-concepts",
        limit = 1,
        offset = 0,
        detail = "full",
        parquet = FALSE,
        search = search
      )
    ),
    silent = TRUE
  )

  if (inherits(vb_result, "try-error")) {
    return(NA_integer_)
  }

  details <- tryCatch(vegbankr::get_page_details(vb_result), error = function(e) NULL)
  total <- extract_reported_total(details)
  if (!is.na(total)) {
    assign(key, total, envir = PLANT_COUNT_CACHE)
  }
  total
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