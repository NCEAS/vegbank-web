#' Concept Table Functions Module
#'
#' Provides server-side DataTable builders for plant and community concepts.

CONCEPT_TABLE_PAGE_LENGTH <- 100L

CONCEPT_CONFIG <- list(
  plant = list(
    concept_type = "plant",
    table_id = "plant_table",
    endpoint = "plant-concepts",
    data_key = "plant_data",
    code_field = "pc_code",
    name_field = "plant_name",
    level_field = "plant_level",
    description_field = "plant_description",
    name_label = "Plant Name",
    session_cache_key = "plant_total_cache",
    remote_label = "plant concepts",
    page_length = CONCEPT_TABLE_PAGE_LENGTH,
    fields = c(
      "pc_code",
      "plant_name",
      "current_accepted",
      "plant_level",
      "concept_rf_code",
      "concept_rf_name",
      "obs_count",
      "plant_description"
    )
  ),
  community = list(
    concept_type = "community",
    table_id = "comm_table",
    endpoint = "community-concepts",
    data_key = "community_data",
    code_field = "cc_code",
    name_field = "comm_name",
    level_field = "comm_level",
    description_field = "comm_description",
    name_label = "Community Name",
    session_cache_key = "community_total_cache",
    remote_label = "community concepts",
    page_length = CONCEPT_TABLE_PAGE_LENGTH,
    fields = c(
      "cc_code",
      "comm_name",
      "current_accepted",
      "comm_level",
      "concept_rf_code",
      "concept_rf_name",
      "obs_count",
      "comm_description"
    )
  )
)

CONCEPT_COUNT_CACHE <- new.env(parent = emptyenv())

clean_dt_frame <- get("cleanDataFrame", envir = asNamespace("DT"))

#' Build the concept DataTable for a given concept type
#'
#' @param concept_data Optional data frame used when outside of a Shiny session (testing)
#' @param concept_type Either "plant" or "community"
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_concept_table <- function(concept_data = NULL, concept_type = c("plant", "community")) {
  concept_type <- match.arg(concept_type)
  config <- get_concept_config(concept_type)

  session <- shiny::getDefaultReactiveDomain()
  remote_enabled <- !is.null(session) && is.null(concept_data)

  table_config <- concept_table_config(config, remote_enabled)

  data_sources <- list()
  required_sources <- character(0)
  process_function <- NULL

  if (!remote_enabled) {
    data_key <- config$data_key
    if (is.null(concept_data)) {
      data_sources[[data_key]] <- concept_empty_source_df(concept_type)[FALSE, , drop = FALSE]
    } else {
      data_sources[[data_key]] <- concept_data
      required_sources <- data_key
    }
    process_function <- function(ds) process_concept_data(ds, concept_type)
  }

  create_table(
    data_sources = data_sources,
    required_sources = required_sources,
    process_function = process_function,
    table_config = table_config
  )
}

#' Process concept data into display format
#'
#' @param data_sources List containing concept data keyed by concept type
#' @param concept_type Either "plant" or "community"
#' @param use_progress Whether to increment the Shiny progress bar
#' @returns A data frame ready for display
#' @noRd
process_concept_data <- function(data_sources, concept_type = "plant", use_progress = TRUE) {
  config <- get_concept_config(concept_type)

  concept_data <- data_sources[[config$data_key]]
  if (is.null(concept_data)) {
    concept_data <- concept_empty_source_df(concept_type)[FALSE, , drop = FALSE]
  }

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = paste0("Cleaning ", concept_type, " names"))
  }
  names <- clean_column_data(concept_data, config$name_field)

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.1, detail = "Preparing status data")
  }
  status_raw <- concept_data$current_accepted
  status_sort <- ifelse(is.na(status_raw), 2, ifelse(status_raw == TRUE, 0, 1))

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.1, detail = paste0("Cleaning ", concept_type, " levels"))
  }
  levels <- clean_column_data(concept_data, config$level_field)

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = "Preparing reference data")
  }
  reference_codes <- concept_data$concept_rf_code
  reference_names <- clean_column_data(concept_data, "concept_rf_name")

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = "Cleaning observation counts")
  }
  obs_counts <- suppressWarnings(as.numeric(clean_column_data(concept_data, "obs_count", "0")))
  obs_counts[is.na(obs_counts)] <- 0

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.15, detail = paste0("Cleaning ", concept_type, " descriptions"))
  }
  descriptions <- clean_column_data(concept_data, config$description_field)

  if (isTRUE(use_progress)) {
    safe_inc_progress(0.1, detail = "Preparing action codes")
  }
  action_codes <- concept_data[[config$code_field]]

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

  names(result)[names(result) == "Name"] <- config$name_label

  result
}

concept_table_config <- function(config, remote_enabled) {
  link_input_id <- if (config$concept_type == "plant") "plant_link_click" else "comm_link_click"

  table_config <- list(
    column_defs = concept_column_defs(link_input_id),
    progress_message = paste0("Processing ", config$remote_label, " table data"),
    page_length = config$page_length
  )

  if (remote_enabled) {
    empty_sources <- setNames(list(concept_empty_source_df(config$concept_type)[FALSE, , drop = FALSE]), config$data_key)
    initial_display <- process_concept_data(empty_sources, concept_type = config$concept_type, use_progress = FALSE)

    table_config$use_progress <- FALSE
    table_config$initial_data <- initial_display
    table_config$options <- concept_remote_options(config)
    table_config$ajax <- function(session) {
      concept_ajax_config(session, config, initial_display)
    }
  }

  table_config
}

concept_column_defs <- function(detail_input_id) {
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
      width = "20%",
      render = create_reference_link_renderer(),
      orderData = 6
    ),
    list(targets = 6, visible = FALSE, searchable = FALSE),
    list(targets = 7, width = "10%", type = "num", className = "dt-right"),
    list(targets = 8, width = "15%")
  )
}

concept_remote_options <- function(config) {
  list(
    serverSide = TRUE,
    lengthChange = FALSE,
    ordering = FALSE,
    searching = TRUE,
    language = list(processing = paste0("Loading ", config$remote_label, "..."))
  )
}

concept_ajax_config <- function(session, config, initial_display) {
  if (is.null(session)) {
    stop("A Shiny session is required to initialize the concept table.")
  }

  page_length <- config$page_length %||% CONCEPT_TABLE_PAGE_LENGTH
  total_cache <- get_session_total_cache(session, config)

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

    page <- fetch_concept_page(config$concept_type, limit = page_length, offset = offset, search = search_term)
    normalized <- normalize_concepts(page$data, config$concept_type)

    data_sources <- setNames(list(normalized), config$data_key)
    display_rows <- process_concept_data(data_sources, concept_type = config$concept_type, use_progress = FALSE)

    details <- page$details
    reported_total <- extract_reported_total(details)
    if (is.null(search_term) && !is.na(reported_total)) {
      total_cache$count <- reported_total
    }

    total_records <- total_cache$count
    if (is.null(total_records) || !length(total_records) || is.na(total_records)) {
      total_records <- fetch_concept_count(config$concept_type, NULL)
    }
    if (is.null(total_records) || !length(total_records) || is.na(total_records)) {
      total_records <- nrow(normalized)
    }
    total_records <- as.integer(round(total_records))

    filtered_records <- reported_total
    if (is.null(filtered_records) || !length(filtered_records) || is.na(filtered_records)) {
      if (is.null(search_term)) {
        filtered_records <- total_records
      } else {
        filtered_records <- fetch_concept_count(config$concept_type, search_term)
        if (is.null(filtered_records) || !length(filtered_records) || is.na(filtered_records)) {
          filtered_records <- nrow(normalized)
        }
      }
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
    outputId = config$table_id
  )

  list(url = ajax_url)
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

normalize_concepts <- function(df, concept_type) {
  if (is.null(df)) {
    return(concept_empty_source_df(concept_type)[FALSE, , drop = FALSE])
  }
  if (!is.data.frame(df)) {
    df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) concept_empty_source_df(concept_type)[FALSE, , drop = FALSE])
  }
  if (!nrow(df)) {
    return(concept_empty_source_df(concept_type)[FALSE, , drop = FALSE])
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

concept_empty_source_df <- function(concept_type) {
  config <- get_concept_config(concept_type)
  cols <- stats::setNames(rep(list(character()), length(config$fields)), config$fields)
  as.data.frame(cols, stringsAsFactors = FALSE)
}

coerce_api_page <- function(parsed, concept_type) {
  if (is.null(parsed)) {
    return(concept_empty_source_df(concept_type)[FALSE, , drop = FALSE])
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
    error = function(e) concept_empty_source_df(concept_type)[FALSE, , drop = FALSE]
  )
}

fetch_concept_page <- function(concept_type, limit, offset, search = NULL) {
  config <- get_concept_config(concept_type)

  vb_result <- try(
    suppressWarnings(
      vegbankr:::get_all_resources(
        config$endpoint,
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
      "vegbankr:::get_all_resources failed for ", concept_type, ": ",
      if (!is.null(vb_error)) conditionMessage(vb_error) else "unknown error"
    )
    return(list(data = concept_empty_source_df(concept_type)[FALSE, , drop = FALSE], details = NULL))
  }

  details <- tryCatch(vegbankr::get_page_details(vb_result), error = function(e) NULL)
  list(data = coerce_api_page(vb_result, concept_type), details = details)
}

fetch_concept_count <- function(concept_type, search = NULL) {
  cache <- get_concept_count_cache(concept_type)
  key <- if (is.null(search)) "__all__" else search
  if (exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache, inherits = FALSE))
  }

  config <- get_concept_config(concept_type)

  vb_result <- try(
    suppressWarnings(
      vegbankr:::get_all_resources(
        config$endpoint,
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
    assign(key, total, envir = cache)
  }
  total
}

get_concept_config <- function(concept_type) {
  config <- CONCEPT_CONFIG[[concept_type]]
  if (is.null(config)) {
    stop("Unsupported concept type: ", concept_type)
  }
  config
}

get_concept_count_cache <- function(concept_type) {
  if (!exists(concept_type, envir = CONCEPT_COUNT_CACHE, inherits = FALSE)) {
    assign(concept_type, new.env(parent = emptyenv()), envir = CONCEPT_COUNT_CACHE)
  }
  get(concept_type, envir = CONCEPT_COUNT_CACHE, inherits = FALSE)
}

get_session_total_cache <- function(session, config) {
  key <- config$session_cache_key
  cache <- session$userData[[key]]
  if (is.null(cache)) {
    cache <- new.env(parent = emptyenv())
    cache$count <- NULL
    session$userData[[key]] <- cache
  }
  cache
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

