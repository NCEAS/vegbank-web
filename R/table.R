#' Generalized Table Functions Module
#'
#' Provides common table functions that can be used across different data types.

# Default number of rows per DataTable page when pagination is enabled
#' @noRd
TABLE_PAGE_LENGTH <- 100L

#' Create a table with the given configuration
#'
#' @param table_config List with table configuration options
#' @returns A DT datatable object ready for display in a Shiny app
#' @importFrom utils modifyList
#' @noRd
create_table <- function(table_config = list()) {
  if (is.null(table_config) || !is.list(table_config)) {
    stop("table_config must be a list")
  }

  session <- shiny::getDefaultReactiveDomain()

  display_data <- table_config$initial_data %||% data.frame()

  column_defs <- table_config$column_defs %||% list()

  options <- list(
    stateSave = TRUE,
    stateDuration = 0,
    stateLoadCallback = DT::JS("function(settings) {\n  if (window.vegbankLoadTableState) {\n    return window.vegbankLoadTableState(settings);\n  }\n  return null;\n}"),
    stateSaveCallback = DT::JS("function(settings, data) {\n  if (window.vegbankSaveTableState) {\n    window.vegbankSaveTableState(settings, data);\n  }\n}"),
    dom = table_config$dom %||% "frtip",
    pageLength = table_config$page_length %||% TABLE_PAGE_LENGTH,
    scrollY = table_config$scroll_y %||% "calc(100vh - 235px)",
    scrollX = table_config$scroll_x %||% TRUE,
    scrollCollapse = table_config$scroll_collapse %||% FALSE,
    deferRender = table_config$defer_render %||% TRUE,
    processing = table_config$processing %||% TRUE,
    order = table_config$order %||% list(list(1, "asc")), # Default sort by second column (vb code) ascending
    columnDefs = column_defs
  )

  if (!is.null(table_config$options)) {
    options <- utils::modifyList(options, table_config$options)
  }

  if (!is.null(table_config$ajax)) {
    ajax_config <- table_config$ajax
    if (is.function(ajax_config)) {
      ajax_config <- ajax_config(session)
    }
    options$ajax <- ajax_config
  }

  datatable_args <- list(
    data = display_data,
    rownames = table_config$rownames %||% FALSE,
    selection = table_config$selection %||% "none",
    # Note: DT's `escape` argument only applies to data passed directly to datatable();
    # when using server-side processing via `ajax`, any HTML must be pre-escaped in R
    escape = table_config$escape %||% TRUE,
    options = options
  )

  if (!is.null(table_config$datatable_args)) {
    datatable_args <- utils::modifyList(datatable_args, table_config$datatable_args)
  }

  widget <- do.call(DT::datatable, datatable_args)

  if (is.null(table_config$initial_data) && is.null(widget$x$data)) {
    # Ensure widget always carries a data frame, even when AJAX supplies rows later
    widget$x$data <- data.frame()
  }

  widget
}

#' Clean a column in data, replacing NA/empty with a default value and capitalizing first letter of first word
#'
#' @param data Data frame
#' @param column_name Name of the column to clean
#' @param default_value Value to use for NA/empty values
#' @returns A character vector with cleaned data
#' @noRd
clean_column_data <- function(data, column_name, default_value = "Unspecified") {
  if (column_name %in% colnames(data)) {
    cleaned <- dplyr::coalesce(dplyr::na_if(as.character(data[[column_name]]), ""), default_value)
    # Return early if the cleaned vector is empty (avoids unnecessary processing)
    if (length(cleaned) == 0) {
      return(cleaned)
    }
    # Column has data, capitalize first letter of first word of data (vectorized), but leave default_value
    cleaned <- ifelse(
      cleaned == default_value,
      cleaned,
      cap_first(cleaned)
    )
    cleaned
    # Column not present, return vector of default values
  } else {
    rep(default_value, nrow(data))
  }
}

#' Clean a GMT date column in data, replacing NA/empty with a default value
#' and formatting dates yyyy-mm-dd as format_date does for detail views
#'
#' @param data Data frame
#' @param column_name Name of the column to clean
#' @param default_value Value to use for NA/empty values
#' @param date_format Format string for output dates
#' @returns A character vector with cleaned data
#' @noRd
clean_column_dates <- function(data, column_name, default_value = "Unspecified", date_format = "%Y-%m-%d") {
  if (!(column_name %in% colnames(data))) {
    return(rep(default_value, nrow(data)))
  }

  column <- data[[column_name]]
  vapply(column, format_date, character(1),
    default_value = default_value,
    format_string = date_format, USE.NAMES = FALSE
  )
}


# Package-level button icon constants — read once at package load time.
# Button icon constants — no inline styles needed; all sizing/spacing is
# handled by CSS rules in vegbank_styles.css targeting .dt-icon-btn svg,
# .vb-plot-download svg, and .vb-help-btn svg.
.BTN_ICON_EYE      <- load_svg_icon("eye")
.BTN_ICON_PIN      <- load_svg_icon("pin")
.BTN_ICON_DOWNLOAD <- load_svg_icon("download")
.BTN_ICON_INFO     <- load_svg_icon("info_circle")
.BTN_ICON_CLOSE    <- load_svg_icon("close")

#' Generate a DT Buttons extension button definition for a toggleable help popover.
#'
#' @param title Plain-text table name shown bold in the popover header.
#' @param content_html Pre-escaped single-line HTML string for the popover body.
#'   Build with `htmltools::tagList`, collapse newlines, then escape single quotes.
#' @return A `DT::JS` character object for inclusion in a `buttons` list.
#' @noRd
make_help_button_js <- function(title, content_html) {
  safe_title <- gsub("'", "\\'", title, fixed = TRUE)
  DT::JS(paste0(
    "{text: '", .BTN_ICON_INFO, "',",
    "className: 'btn btn-sm btn-outline-secondary vb-help-btn',",
    "titleAttr: 'About this table',",
    "action: function() {},",
    "init: function(api, node, config) {",
    "window.vbHelpButton(node, '<strong>", safe_title, "</strong>', '",
    content_html, "');}}"
  ))
}

#' Create action buttons for each row (R version)
#'
#' @param input_id The Shiny input ID for the button click event
#' @param button_label The label text for the button (default: "Details")
#' @param code_values A character vector of values to send to the input (e.g., concept codes)
#' @return A character vector of HTML strings for action buttons
#' @noRd
create_action_buttons <- function(input_id, button_label = "Details", code_values) {
  vapply(code_values, function(code) {
    safe_code <- htmltools::htmlEscape(as.character(code))
    safe_label <- htmltools::htmlEscape(as.character(button_label))
    if (!is.null(code) && !is.na(code) && nzchar(code)) {
      as.character(htmltools::tags$div(
        class = "btn-group btn-group-sm", role = "group",
        htmltools::tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-primary dt-icon-btn dt-shiny-action",
          `data-input-id` = input_id,
          `data-value` = safe_code,
          htmltools::HTML(paste0(.BTN_ICON_EYE, safe_label))
        )
      ))
    } else {
      '<span class="text-muted">No Data</span>'
    }
  }, character(1))
}

#' Create clickable obs_count links for cross-resource filtering for an
#' entire vector of obs_counts
#'
#' @param obs_counts Integer vector of observation counts
#' @param entity_codes Character vector of entity codes (e.g., "pj.340")
#' @param entity_labels Character vector of entity display names
#' @return Character vector of HTML for obs_count links
#' @noRd
create_all_obs_count_links <- function(obs_counts, entity_codes, entity_labels) {
  vapply(seq_along(obs_counts), function(idx) {
    count <- obs_counts[[idx]]
    code <- entity_codes[[idx]]
    label <- entity_labels[[idx]]

    safe_count <- as.integer(count)
    if (is.na(safe_count)) safe_count <- 0L

    # Only make clickable if count > 0 and we have valid code and label
    if (safe_count > 0 && !is.null(code) && !is.na(code) && nzchar(code) &&
          !is.null(label) && !is.na(label) && nzchar(label)) {
      safe_code <- htmltools::htmlEscape(as.character(code), attribute = TRUE)
      safe_label <- htmltools::htmlEscape(as.character(label), attribute = TRUE)
      sprintf(
        '<a href="#" class="obs-count-link dt-shiny-action" data-input-id="obs_count_click" data-value="%s" data-label="%s">%d</a>',
        safe_code,
        safe_label,
        safe_count
      )
    } else {
      as.character(safe_count)
    }
  }, character(1))
}

#' Create a status badge for concept status
#'
#' @param status Logical or NA; TRUE for accepted, FALSE for not accepted, NA for unknown
#' @return HTML string for the badge
#' @noRd
create_status_badge <- function(status) {
  if (is.null(status) || identical(status, "") || is.na(status)) {
    '<span class="badge rounded-pill" style="background-color: var(--no-status-bg); color: var(--no-status-text);">No Status</span>'
  } else if (isTRUE(status) || identical(status, "true") || identical(status, "TRUE")) {
    '<span class="badge rounded-pill" style="background-color: var(--accepted-bg); color: var(--accepted-text);">Accepted</span>'
  } else {
    '<span class="badge rounded-pill" style="background-color: var(--not-current-bg); color: var(--not-current-text);">Not Current</span>'
  }
}

# Utility function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# -------------------- PAGINATED API TABLE HELPERS --------------------

#' Fetch a paginated VegBank resource page
#'
#' Provides a shared wrapper around vegbankr::vb_get with consistent
#' error handling and optional data coercion.
#'
#' @param resource VegBank resource type (e.g., "plant-concepts")
#' @param limit Number of rows to request
#' @param offset Row offset for pagination
#' @param detail API detail level ("minimal" or "full")
#' @param parquet Whether to request parquet output (default FALSE for JSON)
#' @param clean_names Whether to clean column names (passed to vegbankr)
#' @param search Optional search term to pass through to VegBank
#' @param coerce_fn Function that converts the raw vegbankr response into the
#'   structure required by the caller (defaults to identity)
#' @param empty_factory Optional function that returns an empty structure if the
#'   request fails or coercion errors
#' @param query Named list of additional query parameters to append
#' @returns A list containing `data`, `details`, and optional `error`
#' @noRd
fetch_remote_page <- function(resource,
                              limit,
                              offset,
                              detail = c("minimal", "full"),
                              parquet = FALSE,
                              clean_names = FALSE,
                              search = NULL,
                              coerce_fn = identity,
                              empty_factory = NULL,
                              query = list()) {
  detail <- match.arg(detail)

  args <- list(
    resource = resource,
    limit = limit,
    offset = offset,
    detail = detail,
    parquet = parquet,
    clean_names = clean_names
  )

  if (!is.null(search)) {
    args$search <- search
  }

  if (length(query)) {
    args <- c(args, query)
  }

  vb_result <- try(
    suppressWarnings(do.call(vegbankr::vb_get, args)),
    silent = TRUE
  )

  if (inherits(vb_result, "try-error")) {
    vb_error <- attr(vb_result, "condition")
    warning(
      "vegbankr::vb_get failed for ", resource, ": ",
      if (!is.null(vb_error)) conditionMessage(vb_error) else "unknown error"
    )
    data <- if (!is.null(empty_factory)) empty_factory() else NULL
    return(list(data = data, details = NULL, error = vb_error))
  }

  data <- tryCatch(
    coerce_fn(vb_result),
    error = function(e) {
      if (!is.null(empty_factory)) empty_factory() else NULL
    }
  )

  details <- tryCatch(vegbankr::get_page_details(vb_result), error = function(e) NULL)

  list(data = data, details = details, error = NULL)
}

#' Fetch total record count for a VegBank resource
#'
#' Convenience wrapper around `vegbankr::vb_count` to fetch the total record count
#' for a given VegBank resource, avoiding redundant implementations across tables.
#'
#' @param resource VegBank resource type (e.g., "plant-concepts")
#' @param search Optional search term to include in the count query
#' @param detail API detail level (defaults to "minimal" for efficiency)
#' @param parquet Whether to request parquet output (default FALSE)
#' @param clean_names Whether to clean column names
#' @param query Additional query parameters to pass through
#' @returns Integer count or NA_integer_ when unavailable
#' @noRd
fetch_total_count <- function(resource,
                              search = NULL,
                              detail = c("minimal", "full"),
                              parquet = FALSE,
                              clean_names = TRUE) {
  detail <- match.arg(detail)

  args <- list(
    resource = resource,
    search = search,
    detail = detail,
    parquet = parquet,
    clean_names = clean_names
  )

  vb_result <- try(
    suppressWarnings(do.call(vegbankr::vb_count, args)),
    silent = TRUE
  )

  if (inherits(vb_result, "try-error")) {
    vb_error <- attr(vb_result, "condition")
    warning(
      "vegbankr::vb_count failed for ", resource, ": ",
      if (!is.null(vb_error)) conditionMessage(vb_error) else "unknown error"
    )
    return(NA_integer_)
  }

  as.integer(vb_result)
}

#' Extract reported total count from VegBank API page details
#'
#' Safely extracts the total record count from the `count_reported` element of
#' a page details object, handling NULL or malformed values.
#'
#' @param details A list returned by `vegbankr::get_page_details()`
#' @returns Integer total or NA_integer_ if unavailable
#' @noRd
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

#' Build a standardized table module configuration entry
#'
#' Provides a consistent structure for table configuration maps (e.g., concepts,
#' plots, parties) so new table modules can share the same metadata contract.
#'
#' @param type Unique identifier for the table variant (e.g., "plant")
#' @param table_id Output ID of the DT widget
#' @param resource VegBank (or other API) resource type used for remote data
#' @param data_key Name of the data source entry used inside processing helpers
#' @param loading_label Friendly label for progress/loading messages
#' @param fields Character vector describing the canonical data schema
#' @param extra Named list merged into the base entry for module-specific values
#' @returns A list entry that can be stored inside a table config map
#' @noRd
build_table_module_config <- function(type,
                                      table_id,
                                      resource,
                                      data_key,
                                      loading_label,
                                      fields,
                                      extra = list()) {
  base <- list(
    table_type = type,
    table_id = table_id,
    resource = resource,
    data_key = data_key,
    loading_label = loading_label,
    fields = fields
  )

  utils::modifyList(base, extra)
}

#' Build a normalized data source specification
#'
#' Convenience helper that collects the functions and options required for
#' server-side paginated tables into a single validated structure. Future table
#' modules can call this once and pass the result to `build_remote_table_config()`.
#'
#' @param table_id Output ID of the DT widget
#' @param resource Remote resource type in vegbankr used for page fetches
#' @param coerce_fn Function converting raw API results into a data frame
#' @param normalize_fn Function enforcing schema/typing on the coerced data
#' @param display_fn Function transforming normalized data into display rows
#' @param label Optional friendly label for progress text
#' @param schema_fields Optional character vector describing the schema; when
#'   provided, the spec automatically supplies an `empty_factory` returning a
#'   zero-row data frame with those columns
#' @param empty_factory Optional custom function returning empty data (overrides
#'   the schema-driven factory when supplied)
#' @param ... Additional named options merged into the spec (detail, parquet,
#'   clean_names, search_normalizer, clean_rows_fn, count_* overrides, query, etc.)
#' @returns A list tagged with class `data_source_spec`
#' @noRd
build_data_source_spec <- function(table_id,
                                   resource,
                                   coerce_fn = identity,
                                   normalize_fn = identity,
                                   display_fn = identity,
                                   label = NULL,
                                   schema_fields = NULL,
                                   empty_factory = NULL,
                                   ...) {
  if (is.null(table_id) || !nzchar(table_id)) {
    stop("table_id is required for a data source specification")
  }
  if (is.null(resource) || !nzchar(resource)) {
    stop("resource is required for a data source specification")
  }

  if (!is.function(display_fn)) {
    stop("display_fn must be a function")
  }

  schema_factory <- empty_factory
  if (is.null(schema_factory) && !is.null(schema_fields)) {
    schema_factory <- function() build_zero_row_df(schema_fields)
  }

  spec <- list(
    table_id = table_id,
    resource = resource,
    label = label,
    coerce_fn = coerce_fn %||% identity,
    normalize_fn = normalize_fn %||% identity,
    display_fn = display_fn,
    empty_factory = schema_factory,
    ...
  )

  class(spec) <- c("data_source_spec", class(spec))
  spec
}

#' Build AJAX configuration for server-side DataTables
#'
#' Creates a reusable `remote_filter` closure that fetches paginated data from a
#' VegBank resource, processes it for display, and returns the response structure
#' expected by DataTables when operating in server-side mode.
#'
#' @param session Shiny session used by `DT::dataTableAjax`
#' @param table_id Output ID of the DT widget
#' @param initial_data Zero-row data frame desceribing the table schema
#' @param data_source_spec List describing the remote data behavior (resource, handlers, etc.).
#'   Expected entries include `resource`, optional handler overrides (e.g.,
#'   `coerce_fn`, `normalize_fn`, `display_fn`, `empty_factory`), and transport
#'   options such as `page_length`, `detail`, `parquet`, `query`, and `count_*` overrides.
#' @returns List with `url` entry suitable for `table_config$ajax`
#' @noRd
build_remote_ajax_config <- function(session,
                                     table_id,
                                     initial_data,
                                     data_source_spec) {
  if (is.null(session)) {
    stop("A Shiny session is required to initialize the remote DataTable.")
  }

  if (is.null(data_source_spec) || !is.list(data_source_spec)) {
    stop("data_source_spec must be a list")
  }

  resource <- data_source_spec$resource %||% stop("data_source_spec$resource is required")
  page_length <- data_source_spec$page_length %||% TABLE_PAGE_LENGTH
  coerce_fn <- data_source_spec$coerce_fn %||% identity
  normalize_fn <- data_source_spec$normalize_fn %||% identity
  display_fn <- data_source_spec$display_fn %||% identity
  empty_factory <- data_source_spec$empty_factory %||% function() initial_data[FALSE, , drop = FALSE]
  detail <- data_source_spec$detail %||% "full"
  parquet <- data_source_spec$parquet %||% FALSE
  clean_names <- data_source_spec$clean_names %||% FALSE
  search_normalizer <- data_source_spec$search_normalizer %||% normalize_search_term
  clean_rows_fn <- data_source_spec$clean_rows_fn %||% identity
  query <- data_source_spec$query %||% list()
  sort_field_map <- data_source_spec$sort_field_map %||% NULL

  if (!is.function(display_fn)) {
    stop("data_source_spec$display_fn must be a function")
  }

  remote_filter <- function(data, params) {
    draw <- as.integer(params$draw %||% 0L)

    offset <- as.integer(params$start %||% 0L)
    if (is.na(offset) || offset < 0L) {
      offset <- 0L
    }

    search_term <- NULL
    if (!is.null(params$search)) {
      search_term <- search_normalizer(params$search$value)
    }

    # Build dynamic sort param if mapping is provided and DataTables requests sorting
    query_actual <- query
    # Support reactive query by allowing query to be a function
    if (is.function(query_actual)) {
      query_actual <- query_actual()
    }
    if (is.null(query_actual)) {
      query_actual <- list()
    }
    if (!is.null(sort_field_map) && !is.null(params$order) && length(params$order) > 0) {
      ord <- params$order[[1]]
      col_idx <- as.integer(ord$column)
      dir <- ord$dir
      field <- sort_field_map[[as.character(col_idx)]]
      if (!is.null(field)) {
        query_actual$sort <- if (dir == "desc") paste0("-", field) else field
      }
    }
    page <- fetch_remote_page(
      resource = resource,
      limit = page_length,
      offset = offset,
      detail = detail,
      parquet = parquet,
      clean_names = clean_names,
      search = search_term,
      coerce_fn = coerce_fn,
      empty_factory = empty_factory,
      query = query_actual
    )

    normalized <- normalize_fn(page$data)
    display_rows <- display_fn(normalized)

    details <- page$details
    reported_total <- extract_reported_total(details)
    filtered_records <- reported_total

    # fetch total count each time the page is refreshed
    total_records <- fetch_total_count(
      resource = resource,
      search = NULL,
      detail = detail,
      parquet = parquet,
      clean_names = clean_names
    )

    list(
      draw = draw,
      recordsTotal = total_records,
      recordsFiltered = filtered_records,
      data = clean_rows_fn(display_rows)
    )
  }

  ajax_url <- DT::dataTableAjax(
    session = session,
    data = initial_data,
    filter = remote_filter,
    outputId = table_id
  )

  list(url = ajax_url)
}

#' Build a reusable remote table configuration
#'
#' Generates the list of settings consumed by `create_table()` for tables that
#' rely on server-side pagination and filtering via AJAX.
#'
#' @param column_defs Column definition list for DataTables
#' @param initial_data Zero-row data frame representing the table schema
#' @param data_source_spec List describing how to fetch, normalize, and display remote data.
#'   Must include `table_id`, `resource`, and optionally:
#'   `page_length`, `coerce_fn`, `normalize_fn`, `display_fn`, `empty_factory`,
#'   `detail`, `parquet`, `clean_names`, `search_normalizer`, `clean_rows_fn`,
#'   `count_*` overrides, `query`, `count_query`, or a custom `ajax_factory`.
#' @param loading_label Human-friendly label used in progress/processing messages
#' @param page_length Page length override (defaults to TABLE_PAGE_LENGTH)
#' @param options Additional DataTables option overrides merged onto the remote
#'   defaults (`serverSide`, `lengthChange`, etc.)
#' @param datatable_args Optional list merged into the arguments passed to
#'   `DT::datatable` (for common overrides across tables)
#' @returns A list suitable for passing as `table_config`
#' @noRd
build_remote_table_config <- function(column_defs,
                                      initial_data,
                                      data_source_spec,
                                      loading_label = NULL,
                                      page_length = NULL,
                                      options = list(),
                                      datatable_args = list(),
                                      search_placeholder = NULL) {
  if (is.null(data_source_spec) || !is.list(data_source_spec)) {
    stop("data_source_spec must be a list")
  }
  if (is.null(data_source_spec$table_id)) {
    stop("data_source_spec$table_id is required")
  }
  if (is.null(data_source_spec$resource)) {
    stop("data_source_spec$resource is required")
  }

  loading_label <- loading_label %||% data_source_spec$label %||% "data"
  effective_page_length <- page_length %||% data_source_spec$page_length %||% TABLE_PAGE_LENGTH
  data_source_spec$page_length <- effective_page_length

  processing_label <- if (!is.null(loading_label)) {
    paste0("Loading ", loading_label, "...")
  } else {
    "Loading data..."
  }

  lang <- list(processing = processing_label)
  if (!is.null(search_placeholder)) {
    lang$searchPlaceholder <- search_placeholder
  }

  remote_defaults <- list(
    serverSide = TRUE,
    lengthChange = FALSE,
    ordering = TRUE,
    searching = TRUE,
    language = lang
  )

  ajax_factory <- data_source_spec$ajax_factory %||% function(session) {
    build_remote_ajax_config(
      session = session,
      table_id = data_source_spec$table_id,
      initial_data = initial_data,
      data_source_spec = data_source_spec
    )
  }

  list(
    column_defs = column_defs,
    page_length = effective_page_length,
    use_progress = FALSE,
    progress_message = paste0("Processing ", loading_label, " table data"),
    initial_data = initial_data %||% data.frame(),
    options = utils::modifyList(remote_defaults, options),
    datatable_args = datatable_args,
    ajax = ajax_factory
  )
}

#' Build a zero-row data frame with the requested columns
#'
#' Creates a typed, zero-row data frame that can be used as a schema template
#' for remote tables and empty data sources.
#'
#' @param column_names Character vector of column names to include
#' @param column_types Optional named list of prototype vectors (e.g., list(a = numeric())).
#'   Columns without an explicit prototype default to character() columns.
#' @returns A zero-row data frame suitable for use as an empty template
#' @noRd
build_zero_row_df <- function(column_names, column_types = NULL, check_names = TRUE) {
  if (is.null(column_names) || !length(column_names)) {
    return(data.frame()[FALSE, , drop = FALSE])
  }

  column_types <- column_types %||% list()
  template <- stats::setNames(vector("list", length(column_names)), column_names)

  for (name in column_names) {
    prototype <- column_types[[name]] %||% character()
    template[[name]] <- prototype
  }

  as.data.frame(template, stringsAsFactors = FALSE, check.names = check_names)[FALSE, , drop = FALSE]
}

#' Build a typed zero-row schema template for table sources
#'
#' @param column_names Character vector of column names
#' @param numeric_columns Columns coerced to numeric()
#' @param integer_columns Columns coerced to integer()
#' @param logical_columns Columns coerced to logical()
#' @param list_columns Columns coerced to list()
#' @param column_types Optional named list of prototype vectors overriding defaults
#' @returns Zero-row data frame with requested structure
#' @noRd
build_schema_template <- function(column_names,
                                  numeric_columns = NULL,
                                  integer_columns = NULL,
                                  logical_columns = NULL,
                                  list_columns = NULL,
                                  column_types = NULL) {
  column_types <- column_types %||% list()
  resolver <- function(name) {
    if (!is.null(column_types[[name]])) {
      return(column_types[[name]])
    }
    if (!is.null(numeric_columns) && name %in% numeric_columns) {
      return(numeric())
    }
    if (!is.null(integer_columns) && name %in% integer_columns) {
      return(integer())
    }
    if (!is.null(logical_columns) && name %in% logical_columns) {
      return(logical())
    }
    character()
  }

  resolved <- lapply(column_names, resolver)
  names(resolved) <- column_names

  df <- build_zero_row_df(column_names, resolved)
  if (!is.null(list_columns) && length(list_columns)) {
    for (name in intersect(list_columns, column_names)) {
      df[[name]] <- vector("list", 0)
    }
  }

  df
}

#' Build a zero-row display template preserving column names
#'
#' @param column_names Character vector of display column names
#' @param column_types Optional named list overriding default character() prototype
#' @returns Zero-row data frame preserving exact column names
#' @noRd
build_display_template <- function(column_names, column_types = NULL) {
  build_zero_row_df(column_names, column_types, check_names = FALSE)
}

#' Build a remote table configuration from a metadata specification
#'
#' @param spec List describing the table (id, resource, schema, handlers, etc.)
#' @returns A table_config list suitable for `create_table()`
#' @noRd
build_table_config_from_spec <- function(spec) {
  required_fields <- c(
    "table_id", "resource", "loading_label", "column_defs",
    "schema_fields", "coerce_fn", "normalize_fn", "display_fn"
  )
  missing <- required_fields[vapply(required_fields, function(field) is.null(spec[[field]]), logical(1))]
  if (length(missing)) {
    stop("Table spec missing required field(s): ", paste(missing, collapse = ", "))
  }

  schema_template <- spec$schema_template %||% build_zero_row_df(spec$schema_fields)
  initial_display <- spec$initial_display
  if (is.null(initial_display)) {
    initial_display <- spec$display_fn(schema_template)
  }

  data_source_args <- utils::modifyList(
    list(
      table_id = spec$table_id,
      resource = spec$resource,
      coerce_fn = spec$coerce_fn,
      normalize_fn = spec$normalize_fn,
      display_fn = spec$display_fn,
      label = spec$loading_label,
      schema_fields = spec$schema_fields,
      empty_factory = function() schema_template
    ),
    spec$data_source %||% list()
  )

  data_source_spec <- do.call(build_data_source_spec, data_source_args)

  table_config <- build_remote_table_config(
    column_defs = spec$column_defs,
    initial_data = initial_display,
    data_source_spec = data_source_spec,
    loading_label = spec$loading_label,
    page_length = spec$page_length,
    options = spec$options %||% list(),
    datatable_args = spec$datatable_args,
    search_placeholder = spec$search_placeholder
  )

  if (!is.null(spec$ajax_factory)) {
    table_config$ajax <- spec$ajax_factory
  }

  table_config
}

#' Build and render a table directly from a metadata spec
#'
#' @param spec Table metadata list accepted by `build_table_config_from_spec()`
#' @returns DT datatable widget
#' @noRd
build_table_from_spec <- function(spec) {
  create_table(table_config = build_table_config_from_spec(spec))
}

#' Normalize and validate search term from DataTables request
#'
#' Decodes, trims, and validates the search term from a DataTables AJAX request.
#' Returns NULL for empty or whitespace-only search strings.
#'
#' @param value The raw search value from DataTables params
#' @returns A trimmed search string, or NULL if empty
#'
#' @importFrom httpuv decodeURIComponent
#' @noRd
normalize_search_term <- function(value) {
  term <- value %||% ""
  term <- httpuv::decodeURIComponent(term)
  term <- trimws(term)
  if (!nzchar(term)) {
    return(NULL)
  }
  term
}

#' Parse character or mixed vector into logical values
#'
#' Converts various string representations of boolean values (true/false, t/f,
#' 1/0) into R logical values. Case-insensitive. Unrecognized values become NA.
#'
#' @param x A vector to parse into logical values
#' @returns A logical vector of the same length as x
#' @noRd
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


#' Coerce API response to a data frame with standard unwrapping
#'
#' Handles common vegbankr response structures by recursively unwrapping nested
#' lists and data keys. Provides consistent error handling across all table types.
#'
#' @param parsed Raw API response (data frame, list, or nested structure)
#' @param schema_template Zero-row data frame with correct column types to return on coercion failure
#' @return Data frame, or empty_template if coercion fails
#' @importFrom dplyr bind_rows
#' @noRd
coerce_api_response <- function(parsed, schema_template) {
  if (is.null(parsed)) {
    return(schema_template)
  }
  if (is.data.frame(parsed)) {
    return(parsed)
  }
  if (is.list(parsed)) {
    # Unwrap common vegbankr envelope structures
    if (!is.null(parsed$data)) {
      return(coerce_api_response(parsed$data, schema_template))
    }
    # Handle single-element list wrappers (both named and unnamed)
    if (length(parsed) == 1) {
      return(coerce_api_response(parsed[[1]], schema_template))
    }
    # Try converting list of records to data frame
    tryCatch(
      {
        df <- dplyr::bind_rows(parsed)
        return(df)
      },
      error = function(e) {
        # Fall back to as.data.frame
        tryCatch(
          as.data.frame(parsed, stringsAsFactors = FALSE),
          error = function(e2) schema_template
        )
      }
    )
  }

  tryCatch(
    as.data.frame(parsed, stringsAsFactors = FALSE),
    error = function(e) schema_template
  )
}

#' Normalize API data to match schema template
#'
#' Ensures all expected fields exist and appear in the correct order. Coerces field
#' types to match the schema template when they differ (e.g., character to numeric).
#'
#' @param df Raw data frame from API (after coercion)
#' @param schema_template Zero-row data frame with correct column types
#' @return Normalized data frame with all expected columns, or schema_template if empty
#' @noRd
normalize_table_data <- function(df, schema_template) {
  if (is.null(df)) {
    return(schema_template)
  }

  if (!is.data.frame(df)) {
    df <- tryCatch(
      as.data.frame(df, stringsAsFactors = FALSE),
      error = function(e) schema_template
    )
  }

  if (!nrow(df)) {
    return(schema_template)
  }

  # Add any missing fields using prototypes from schema template
  missing_fields <- setdiff(names(schema_template), names(df))
  for (field in missing_fields) {
    # Use the schema template's column as a prototype
    prototype <- schema_template[[field]]
    if (is.list(prototype)) {
      df[[field]] <- vector("list", nrow(df))
    } else {
      df[[field]] <- rep(prototype[NA_integer_], nrow(df))
    }
  }

  # Reorder columns to match schema
  df <- df[, names(schema_template), drop = FALSE]

  # Coerce types to match schema template where they differ
  for (col_name in names(schema_template)) {
    template_type <- class(schema_template[[col_name]])[1]
    data_type <- class(df[[col_name]])[1]

    # Only coerce if types don't match and template isn't just character
    if (data_type != template_type && template_type != "character") {
      if (template_type == "numeric") {
        df[[col_name]] <- suppressWarnings(as.numeric(df[[col_name]]))
      } else if (template_type == "integer") {
        df[[col_name]] <- suppressWarnings(as.integer(df[[col_name]]))
      } else if (template_type == "logical") {
        df[[col_name]] <- as.logical(df[[col_name]])
      }
      # List columns already handled above, no coercion needed
    }
  }

  rownames(df) <- NULL
  df
}

#' Create a coercion function for table specs
#'
#' Factory that produces a coercion function bound to a specific schema template.
#' Eliminates the need for per-table coercion wrapper functions.
#'
#' @param schema_template Zero-row data frame with correct column types
#' @return Function that coerces API responses to data frames
#' @noRd
create_coercer <- function(schema_template) {
  function(parsed) {
    coerce_api_response(parsed, schema_template)
  }
}

#' Create a normalization function for table specs
#'
#' Factory that produces a normalization function with optional post-processing.
#' Handles common patterns like NA-to-zero conversion and custom transformations.
#'
#' @param schema_template Zero-row data frame with correct column types
#' @param na_to_zero_fields Character vector of fields where NA should become 0
#' @param custom_transforms List of functions to apply after normalization
#' @return Function that normalizes data frames to match schema
#' @noRd
create_normalizer <- function(schema_template, na_to_zero_fields = NULL, custom_transforms = NULL) {
  function(df) {
    normalized <- normalize_table_data(df, schema_template)

    # Handle NA → 0 for specified integer/numeric fields
    if (!is.null(na_to_zero_fields)) {
      for (field in na_to_zero_fields) {
        if (field %in% names(normalized)) {
          col_type <- class(normalized[[field]])[1]
          if (col_type == "integer") {
            normalized[[field]][is.na(normalized[[field]])] <- 0L
          } else if (col_type == "numeric") {
            normalized[[field]][is.na(normalized[[field]])] <- 0
          }
        }
      }
    }

    # Apply custom transformations if provided
    if (!is.null(custom_transforms)) {
      for (transform_fn in custom_transforms) {
        normalized <- transform_fn(normalized)
      }
    }

    normalized
  }
}

#' Sanitize data frame rows for DataTables JSON responses
#'
#' Mirrors the core behavior of DT's internal cleanDataFrame helper so we can
#' keep our dependency on exported APIs only. Removes names and dimensions from
#' columns to ensure clean JSON serialization.
#'
#' @param data Data frame (or other object) destined for DataTables AJAX
#' @return Object with unnamed columns whose values have been stripped of dims
#' @noRd
sanitize_dt_rows <- function(data) {
  if (is.null(data)) {
    return(data)
  }

  if (!is.data.frame(data)) {
    return(unname(data))
  }

  cleaned <- unname(data)
  for (j in seq_len(ncol(cleaned))) {
    column <- cleaned[[j]]
    column <- unname(column)
    dim(column) <- NULL
    if (is.table(column)) {
      column <- c(column)
    }
    cleaned[[j]] <- column
  }
  unname(cleaned)
}
