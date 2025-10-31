#' Shiny Server for Vegbank Web Application
#'
#' Initializes server-side functionality for data visualization and interactivity.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return Called for its side effects.
#'
#' @importFrom ggplot2 .data
#' @importFrom shiny reactiveVal observeEvent observe req renderUI renderText showNotification
#'             updateNavbarPage invalidateLater reactiveValuesToList onBookmark onBookmarked
#'             onRestore
#' @importFrom DT renderDataTable datatable dataTableProxy
#' @importFrom htmltools tags
#' @importFrom leaflet renderLeaflet leafletProxy
#' @importFrom utils head
#' @import vegbankr
#'
#' @noRd
# ================= MAIN SERVER FUNCTION ===========================================================
server <- function(input, output, session) {
  # CONSTANTS --------------------------------------------------------------------------------------

  # Get default map settings from constants module
  map_defaults <- get_map_defaults()
  DEFAULT_MAP_LAT <- map_defaults$lat
  DEFAULT_MAP_LNG <- map_defaults$lng
  DEFAULT_MAP_ZOOM <- map_defaults$zoom

  # STATE INITIALIZATION ---------------------------------------------------------------------------
  # Browser history and URL synchronization state management:
  # - updating_from_query: prevents circular updates when restoring state from URL
  # - history_initialized: ensures first navigation uses replace mode instead of push
  # - state$current_tab: tracks active navigation tab
  # - state$details_open: tracks whether detail overlay is visible
  # - state$detail_type/selected_code: identify which entity detail is shown
  # - state$map_center_lat/lng/zoom: current map view state
  # - state$map_has_custom_state: flag indicating map has moved from defaults
  # - state$table_states: reactiveValues storing pagination/sort/filter for each table
  # - state$map_request: temporary holder for "show on map" requests during tab transitions

  updating_from_query <- shiny::reactiveVal(FALSE)
  history_initialized <- shiny::reactiveVal(FALSE)

  # Initialize map state with defaults
  map_state <- initialize_map_state()

  state <- list(
    current_tab = shiny::reactiveVal("Overview"),
    details_open = shiny::reactiveVal(FALSE),
    detail_type = shiny::reactiveVal(NULL),
    selected_code = shiny::reactiveVal(NULL),
    map_center_lat = map_state$map_center_lat,
    map_center_lng = map_state$map_center_lng,
    map_zoom = map_state$map_zoom,
    map_has_custom_state = shiny::reactiveVal(FALSE),
    map_request = shiny::reactiveVal(NULL),
    table_states = shiny::reactiveValues()
  )

  # Table registry: maps tab names to table IDs and default page lengths
  # Used to encode/decode table state in the URL query string
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L),
    plants = list(tab = "Plants", table_id = "plant_table", default_length = 100L),
    communities = list(tab = "Communities", table_id = "comm_table", default_length = 100L),
    people = list(tab = "People", table_id = "party_table", default_length = 100L),
    projects = list(tab = "Projects", table_id = "proj_table", default_length = 100L)
  )

  # Reverse lookup: tab name -> table key
  tab_to_key <- setNames(names(table_registry), vapply(table_registry, `[[`, character(1), "tab"))

  #' Get table key from tab name safely
  #'
  #' @param tab_name The name of the navigation tab
  #' @return The table registry key, or NULL if tab has no table
  #' @noRd
  get_table_key <- function(tab_name) {
    if (is.null(tab_name) || !nzchar(tab_name)) {
      return(NULL)
    }

    if (!tab_name %in% names(tab_to_key)) {
      return(NULL)
    }

    tab_to_key[[tab_name]]
  }

  # HELPER FUNCTIONS: Query Parameter Parsing & Formatting ----------------------------------------

  #' Extract first element from query parameter array
  #' @noRd
  first_param <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return(NULL)
    }
    value[[1]]
  }

  #' Check if query parameter is valid (non-null, non-empty)
  #' @noRd
  is_valid_param <- function(value) {
    candidate <- first_param(value)
    !is.null(candidate) && !is.na(candidate) && nzchar(candidate)
  }

  #' URL-encode query parameter value
  #' @noRd
  encode_query_value <- function(value) {
    utils::URLencode(as.character(value), reserved = TRUE)
  }

  #' Parse numeric query parameter safely
  #' @param value Query parameter value (may be NULL, NA, or empty)
  #' @return Numeric value or NULL if invalid
  #' @noRd
  parse_numeric_param <- function(value) {
    raw_value <- first_param(value)
    if (is.null(raw_value) || isTRUE(is.na(raw_value)) || identical(raw_value, "")) {
      return(NULL)
    }

    candidate <- suppressWarnings(as.numeric(raw_value))
    if (length(candidate) == 0 || isTRUE(is.na(candidate))) NULL else candidate
  }

  #' Format coordinate for URL query string (5 decimal places)
  #' @noRd
  format_coord <- function(value) {
    sprintf("%.5f", as.numeric(value))
  }

  #' Format zoom level for URL query string
  #' @noRd
  format_zoom <- function(value) {
    formatted <- format(signif(as.numeric(value), digits = 6), trim = TRUE, scientific = FALSE)
    sub("\\.0+$", "", formatted)
  }

  # HELPER FUNCTIONS: Map State Management --------------------------------------------------------

  #' Compare numeric values with tolerance for floating point precision
  #' @noRd
  nearly_equal <- function(a, b, tol = 1e-6) {
    if (is.null(a) || is.null(b)) {
      return(FALSE)
    }
    diff <- suppressWarnings(abs(as.numeric(a) - as.numeric(b)))
    !is.na(diff) && diff <= tol
  }

  #' Check if map state matches default values
  #' @noRd
  map_is_default <- function(lat, lng, zoom) {
    nearly_equal(lat, DEFAULT_MAP_LAT) &&
      nearly_equal(lng, DEFAULT_MAP_LNG) &&
      nearly_equal(zoom, DEFAULT_MAP_ZOOM)
  }

  #' Update flag indicating whether map has custom state
  #' @noRd
  update_map_custom_flag <- function(lat = state$map_center_lat(), lng = state$map_center_lng(), zoom = state$map_zoom()) {
    state$map_has_custom_state(!map_is_default(lat, lng, zoom))
  }

  # HELPER FUNCTIONS: Table State Management ------------------------------------------------------

  #' Parse DataTables order entries from various formats
  #' @param entries Order data from DataTables state (list, data.frame, or matrix)
  #' @return List of order entries with column and direction, or NULL
  #' @noRd
  parse_order_entries <- function(entries) {
    if (is.null(entries)) {
      return(NULL)
    }

    if (is.data.frame(entries) || is.matrix(entries)) {
      entries <- split(entries, seq_len(nrow(entries)))
    }

    if (!is.list(entries)) {
      return(NULL)
    }

    parsed <- lapply(entries, function(item) {
      if (is.null(item)) {
        return(NULL)
      }

      column <- suppressWarnings(as.integer(item[[1]]))
      direction <- as.character(item[[2]] %||% "")

      if (is.na(column) || !nzchar(direction)) {
        return(NULL)
      }

      list(column = column, dir = direction)
    })

    parsed[vapply(parsed, is.null, logical(1))] <- NULL
    if (length(parsed) == 0) NULL else parsed
  }

  #' Normalize and validate DataTables state object
  #' @param raw_state Raw state object from DataTables
  #' @return Sanitized state list with start, length, order, search
  #' @noRd
  sanitize_table_state <- function(raw_state) {
    if (is.null(raw_state)) {
      return(NULL)
    }

    start <- suppressWarnings(as.integer(raw_state$start %||% raw_state[["start"]]))
    length <- suppressWarnings(as.integer(raw_state$length %||% raw_state[["length"]]))
    search <- raw_state$search %||% raw_state[["search"]] %||% ""

    if (!is.null(search) && is.list(search)) {
      search <- search$search %||% search[["search"]] %||% ""
    }

    order_entries <- raw_state$order %||% raw_state[["order"]]
    order <- parse_order_entries(order_entries)

    if (is.na(start)) start <- 0L
    if (is.na(length) || length <= 0) length <- 100L
    if (is.null(search)) search <- ""

    list(
      start = start,
      length = length,
      order = order,
      search = as.character(search)
    )
  }

  #' Serialize table sort order for URL query string
  #' @param order List of order entries from table state
  #' @return Comma-separated string like "0:asc,1:desc"
  #' @noRd
  serialize_order_for_query <- function(order) {
    if (is.null(order) || length(order) == 0) {
      return(NULL)
    }

    parts <- vapply(order, function(item) {
      paste(item$column, item$dir, sep = ":")
    }, character(1))

    paste(parts, collapse = ",")
  }

  #' Deserialize table sort order from URL query string
  #' @param order_string Query parameter value like "0:asc,1:desc"
  #' @return List of order entries with column and direction
  #' @noRd
  deserialize_order_from_query <- function(order_string) {
    order_value <- first_param(order_string)
    if (is.null(order_value) || !nzchar(order_value)) {
      return(NULL)
    }

    order_tokens <- strsplit(order_value, ",", fixed = TRUE)[[1]]
    if (length(order_tokens) == 0 || identical(order_tokens, "")) {
      return(NULL)
    }

    parsed <- lapply(order_tokens, function(token) {
      parts <- strsplit(token, ":", fixed = TRUE)[[1]]
      if (length(parts) != 2) {
        return(NULL)
      }
      column <- suppressWarnings(as.integer(parts[1]))
      direction <- parts[2]
      if (is.na(column) || !nzchar(direction)) {
        return(NULL)
      }
      list(column = column, dir = direction)
    })

    parsed[vapply(parsed, is.null, logical(1))] <- NULL
    if (length(parsed) == 0) NULL else parsed
  }

  #' Extract table state from URL query parameters
  #' @param key Table registry key (e.g., "plots")
  #' @param params Parsed query string parameters
  #' @return Table state list or NULL if no state present
  #' @noRd
  extract_table_state_from_query <- function(key, params) {
    prefix <- key
    start <- parse_numeric_param(params[[paste0(prefix, "_start")]])
    length <- parse_numeric_param(params[[paste0(prefix, "_length")]])
    order <- deserialize_order_from_query(params[[paste0(prefix, "_order")]])
    search <- first_param(params[[paste0(prefix, "_search")]]) %||% ""

    if (is.null(start) && is.null(length) && is.null(order) && !nzchar(search)) {
      return(NULL)
    }

    default_length <- table_registry[[key]]$default_length %||% 100

    if (is.null(start)) start <- 0
    if (is.null(length) || length <= 0) length <- default_length

    list(
      start = as.integer(start),
      length = as.integer(length),
      order = order,
      search = as.character(search)
    )
  }

  #' Build custom message for applying table state on client side
  #' @param key Table registry key
  #' @param state Sanitized table state
  #' @return Message list for custom JS handler
  #' @noRd
  table_state_message <- function(key, state) {
    table_id <- table_registry[[key]]$table_id
    if (is.null(table_id) || is.null(state)) {
      return(NULL)
    }

    page <- 0L
    if (!is.null(state$length) && state$length > 0) {
      page <- as.integer(floor(state$start / state$length))
    }

    list(
      tableId = table_id,
      state = list(
        start = state$start,
        length = state$length,
        page = page,
        search = state$search %||% "",
        order = lapply(state$order %||% list(), function(item) {
          list(column = item$column, dir = item$dir)
        })
      )
    )
  }

  #' Check if table state matches default values (no pagination/sort/filter)
  #' @noRd
  is_default_table_state <- function(key, state) {
    if (is.null(state)) {
      return(TRUE)
    }

    default_length <- table_registry[[key]]$default_length %||% 100L
    start_default <- is.null(state$start) || state$start <= 0
    length_default <- is.null(state$length) || identical(as.integer(state$length), as.integer(default_length))
    search_default <- is.null(state$search) || identical(state$search, "")
    order_default <- is.null(state$order) || length(state$order) == 0

    start_default && length_default && search_default && order_default
  }

  #' Send table state to client-side JavaScript to restore view
  #' @noRd
  send_table_state_to_client <- function(key, state) {
    message <- table_state_message(key, state)
    if (!is.null(message)) {
      session$sendCustomMessage("applyTableState", message)
    }
  }

  #' Store table state and optionally update URL query string
  #' @param key Table registry key
  #' @param raw_state Raw state from DataTables
  #' @noRd
  store_table_state <- function(key, raw_state) {
    sanitized <- sanitize_table_state(raw_state)
    if (is.null(sanitized)) {
      return()
    }

    tab_for_key <- table_registry[[key]]$tab

    if (is_default_table_state(key, sanitized)) {
      if (!is.null(state$table_states[[key]])) {
        state$table_states[[key]] <- NULL

        if (!isTRUE(updating_from_query()) && identical(tab_for_key, state$current_tab())) {
          update_app_query(mode = "replace")
        }
      }
      return()
    }

    current <- state$table_states[[key]]
    if (!is.null(current) && identical(current, sanitized)) {
      return()
    }

    state$table_states[[key]] <- sanitized

    if (!isTRUE(updating_from_query()) && identical(tab_for_key, state$current_tab())) {
      update_app_query(mode = "replace")
    }
  }

  # CORE URL SYNCHRONIZATION FUNCTIONS ------------------------------------------------------------

  #' Get current browser query string safely
  #' @return Query string (e.g., "?tab=Map&map_lat=40.5") or empty string
  #' @noRd
  current_query_string <- function() {
    query <- session$clientData$url_search
    if (is.null(query) || length(query) == 0 || isTRUE(is.na(query)) || identical(query, "")) {
      ""
    } else {
      query
    }
  }

  #' Update the browser history to reflect current app state
  #'
  #' Encodes tab, detail view, map position, and table state into URL query string.
  #' Uses "push" mode to add to history stack or "replace" to update current entry.
  #'
  #' @param mode "push" (add history entry) or "replace" (update current entry)
  #' @param tab Tab name; if NULL, uses state$current_tab()
  #' @param detail_type Entity type for detail overlay (e.g., "plot-observation")
  #' @param detail_code Entity code for detail overlay
  #' @noRd
  update_app_query <- function(mode = c("push", "replace"),
                               tab = NULL, detail_type = NULL, detail_code = NULL) {
    mode <- match.arg(mode)

    if (is.null(tab)) {
      tab <- state$current_tab()
    }

    if (is.null(tab) || !nzchar(tab)) {
      tab <- "Overview"
    }

    if (is.null(detail_type) || is.null(detail_code)) {
      if (isTRUE(state$details_open())) {
        detail_type <- state$detail_type()
        detail_code <- state$selected_code()
      } else {
        detail_type <- NULL
        detail_code <- NULL
      }
    }

    params <- list()
    if (!is.null(tab) && nzchar(tab)) {
      params$tab <- tab
    }

    if (!is.null(detail_type) && !is.null(detail_code) && nzchar(detail_type) && nzchar(detail_code)) {
      params$detail <- detail_type
      params$code <- detail_code
    }

    include_map_params <- identical(tab, "Map") || isTRUE(state$map_has_custom_state())

    map_lat <- state$map_center_lat()
    map_lng <- state$map_center_lng()
    map_zoom <- state$map_zoom()

    if (include_map_params && !anyNA(c(map_lat, map_lng, map_zoom))) {
      params$map_lat <- format_coord(map_lat)
      params$map_lng <- format_coord(map_lng)
      params$map_zoom <- format_zoom(map_zoom)
    }

    table_key <- get_table_key(tab)
    if (!is.null(table_key)) {
      table_state <- state$table_states[[table_key]]
      if (!is.null(table_state)) {
        params[[paste0(table_key, "_start")]] <- as.character(table_state$start)
        params[[paste0(table_key, "_length")]] <- as.character(table_state$length)

        order_string <- serialize_order_for_query(table_state$order)
        if (!is.null(order_string)) {
          params[[paste0(table_key, "_order")]] <- order_string
        }

        if (!is.null(table_state$search) && nzchar(table_state$search)) {
          params[[paste0(table_key, "_search")]] <- table_state$search
        }
      }
    }

    target_query <- if (length(params) > 0) {
      paste0(
        "?",
        paste(
          names(params),
          vapply(params, encode_query_value, character(1)),
          sep = "=",
          collapse = "&"
        )
      )
    } else {
      ""
    }

    if (identical(target_query, current_query_string())) {
      return(invisible(FALSE))
    }

    shiny::updateQueryString(target_query, mode = mode, session = session)
    invisible(TRUE)
  }

  #' Open entity detail overlay with optional history tracking
  #'
  #' @param detail_type Entity type (e.g., "plot-observation", "plant-concept")
  #' @param vb_code VegBank entity code
  #' @param push_history Whether to update URL and add history entry
  #' @param history_mode "push" or "replace" for history API
  #' @noRd
  open_detail <- function(detail_type, vb_code, push_history = TRUE, history_mode = "push") {
    # Ensure we have an up-to-date record of the current tab
    state$current_tab(input$page %||% state$current_tab())

    args <- list(
      state = state,
      session = session,
      output = output,
      vb_code = vb_code,
      detail_type = detail_type
    )

    if (detail_type %in% c("community-classification", "taxon-observation")) {
      args$comm_class_data <- comm_class_data
      args$taxa_data <- taxa_data
      args$plot_data <- plot_data
    }

    success <- do.call(open_code_details, args)

    if (isTRUE(success) && push_history) {
      update_app_query(
        mode = history_mode,
        tab = state$current_tab(),
        detail_type = detail_type,
        detail_code = vb_code
      )
    }

    invisible(success)
  }

  #' Close entity detail overlay with optional history tracking
  #'
  #' @param push_history Whether to update URL and add history entry
  #' @param history_mode "push" or "replace" for history API
  #' @param hide_overlay Whether to hide the overlay UI element
  #' @noRd
  close_detail <- function(push_history = TRUE, history_mode = "push", hide_overlay = TRUE) {
    if (!isTRUE(state$details_open())) {
      state$detail_type(NULL)
      state$selected_code(NULL)
      return(invisible(FALSE))
    }

    state$details_open(FALSE)
    state$detail_type(NULL)
    state$selected_code(NULL)

    if (hide_overlay) {
      session$sendCustomMessage("closeOverlay", list())
    }

    session$sendCustomMessage("clearAllTableSelections", list())

    if (push_history) {
      update_app_query(mode = history_mode, tab = state$current_tab(), detail_type = NULL, detail_code = NULL)
    }

    invisible(TRUE)
  }

  #' Reactive accessor for current URL query parameters
  #'
  #' Parses the browser query string into a named list. Used by observers
  #' to detect navigation events and restore state from URL.
  #'
  #' @return Named list of query parameters
  #' @noRd
  current_query <- shiny::reactive({
    query_string <- current_query_string()
    if (identical(query_string, "")) {
      list()
    } else {
      shiny::parseQueryString(query_string)
    }
  })


  # DATA LOADING -----------------------------------------------------------------------------------

  vegbankr::vb_debug()
  vegbankr::set_vb_base_url("https://api-dev.vegbank.org")

  shiny::withProgress(message = "Fetching data:", value = 0, {
    plot_data <- load_data_type(
      "plot observations",
      "inst/cached_data/ob_20251015.RDS",
      vegbankr::get_all_plot_observations,
      list(detail = "minimal")
    )

    comm_class_data <- load_data_type(
      "community classifications",
      "inst/cached_data/cl_20251015.RDS",
      vegbankr::get_all_community_classifications,
      list(detail = "minimal")
    )

    comm_concept_data <- load_data_type(
      "community concepts",
      "inst/cached_data/cc_20251015.RDS",
      vegbankr::get_all_community_concepts
    )

    taxa_data <- load_data_type(
      "taxon observations",
      "inst/cached_data/to_20251015.RDS",
      vegbankr::get_all_taxon_observations
    )

    plant_data <- load_data_type(
      "plant concepts",
      "inst/cached_data/pc_20251017.RDS",
      vegbankr::get_all_plant_concepts
    )

    project_data <- load_data_type(
      "projects",
      "inst/cached_data/pj_20251014.rds",
      vegbankr::get_all_projects
    )

    party_data <- load_data_type(
      "parties",
      "inst/cached_data/py_20251014.rds",
      vegbankr::get_all_parties
    )
  })

  # URL/HISTORY SYNCHRONIZATION OBSERVER -----------------------------------------------------------
  # This observer runs whenever the browser URL changes (back/forward, refresh, direct link).
  # It parses query parameters and restores tab, detail overlay, map view, and table state.
  # The updating_from_query flag prevents circular updates when we programmatically change the URL.

  shiny::observeEvent(current_query(),
    {
      params <- current_query()

      updating_from_query(TRUE)

      # Parse and apply requested tab
      requested_tab <- first_param(params$tab)
      if (!is_valid_param(requested_tab)) {
        requested_tab <- "Overview"
      }

      if (!identical(state$current_tab(), requested_tab)) {
        state$current_tab(requested_tab)
        shiny::updateNavbarPage(session, "page", selected = requested_tab)
      } else {
        state$current_tab(requested_tab)
      }

      # Parse and apply detail overlay state
      target_type <- NULL
      target_code <- NULL

      detail_valid <- is_valid_param(params$detail) && is_valid_param(params$code)

      if (detail_valid) {
        target_type <- first_param(params$detail)
        target_code <- first_param(params$code)

        needs_open <- !identical(state$detail_type(), target_type) ||
          !identical(state$selected_code(), target_code) ||
          !isTRUE(state$details_open())

        if (needs_open) {
          open_detail(target_type, target_code, push_history = FALSE)
        }
      } else if (isTRUE(state$details_open())) {
        close_detail(push_history = FALSE, hide_overlay = TRUE)
      } else if (!is.null(params$detail) || !is.null(params$code)) {
        update_app_query(
          mode = "replace",
          tab = requested_tab,
          detail_type = NULL,
          detail_code = NULL
        )
      }

      # Parse and apply map view state
      map_lat_param <- params$map_lat
      map_lng_param <- params$map_lng
      map_zoom_param <- params$map_zoom

      map_lat <- parse_numeric_param(map_lat_param)
      map_lng <- parse_numeric_param(map_lng_param)
      map_zoom <- parse_numeric_param(map_zoom_param)

      map_params_present <- !is.null(map_lat_param) || !is.null(map_lng_param) || !is.null(map_zoom_param)
      if (!is.null(map_lat) && !is.null(map_lng)) {
        if (!nearly_equal(state$map_center_lat(), map_lat, tol = 1e-5)) {
          state$map_center_lat(map_lat)
        }
        if (!nearly_equal(state$map_center_lng(), map_lng, tol = 1e-5)) {
          state$map_center_lng(map_lng)
        }

        if (!is.null(map_zoom) && !nearly_equal(state$map_zoom(), map_zoom, tol = 1e-4)) {
          state$map_zoom(map_zoom)
        }

        target_zoom <- if (!is.null(map_zoom)) map_zoom else state$map_zoom()
        update_map_custom_flag(map_lat, map_lng, target_zoom)

        if (identical(requested_tab, "Map")) {
          leaflet::leafletProxy("map", session) |>
            leaflet::setView(lng = map_lng, lat = map_lat, zoom = target_zoom)
        }
      } else if (map_params_present) {
        state$map_center_lat(DEFAULT_MAP_LAT)
        state$map_center_lng(DEFAULT_MAP_LNG)
        state$map_zoom(DEFAULT_MAP_ZOOM)
        update_map_custom_flag(DEFAULT_MAP_LAT, DEFAULT_MAP_LNG, DEFAULT_MAP_ZOOM)

        update_app_query(
          mode = "replace",
          tab = requested_tab,
          detail_type = if (detail_valid) target_type else NULL,
          detail_code = if (detail_valid) target_code else NULL
        )
      } else if (identical(requested_tab, "Map")) {
        if (!map_params_present && !map_is_default(state$map_center_lat(), state$map_center_lng(), state$map_zoom())) {
          state$map_center_lat(DEFAULT_MAP_LAT)
          state$map_center_lng(DEFAULT_MAP_LNG)
          state$map_zoom(DEFAULT_MAP_ZOOM)
        }

        update_map_custom_flag()

        leaflet::leafletProxy("map", session) |>
          leaflet::setView(
            lng = state$map_center_lng(),
            lat = state$map_center_lat(),
            zoom = state$map_zoom()
          )
      }

      # Parse and apply table state (pagination, sorting, filtering)
      table_key <- get_table_key(requested_tab)
      if (!is.null(table_key)) {
        table_params_present <- any(vapply(
          c("_start", "_length", "_order", "_search"),
          function(suffix) {
            !is.null(params[[paste0(table_key, suffix)]])
          },
          logical(1)
        ))

        table_state_from_query <- extract_table_state_from_query(table_key, params)

        if (is.null(table_state_from_query) || is_default_table_state(table_key, table_state_from_query)) {
          state$table_states[[table_key]] <- NULL
          if (table_params_present) {
            update_app_query(
              mode = "replace",
              tab = requested_tab,
              detail_type = if (detail_valid) target_type else NULL,
              detail_code = if (detail_valid) target_code else NULL
            )
          }
        } else {
          state$table_states[[table_key]] <- table_state_from_query
          send_table_state_to_client(table_key, table_state_from_query)
        }
      }

      # Ensure tab parameter is present in URL
      if (!is_valid_param(params$tab)) {
        update_app_query(
          mode = "replace",
          tab = requested_tab,
          detail_type = if (detail_valid) target_type else NULL,
          detail_code = if (detail_valid) target_code else NULL
        )
      }

      updating_from_query(FALSE)
      history_initialized(TRUE)
    },
    ignoreNULL = FALSE
  )

  # RENDER UI ELEMENTS --------------------------------------------------------------------------------

  output$dataSummary <- shiny::renderUI({
    htmltools::tags$p(
      "Vegbank is a database of vegetation plot data. Navigate to the Plots tab ",
      "to browse the available plot data. Each row in the table represents a plot observation.",
      "You can also view the plot locations on a map by navigating to the 'Map' tab. ",
      "Clicking on the see details button in a row in the table or a link in the pin label on the ",
      "map will display detailed information about that plot observation including information ",
      "about the plot location, species observed, and other details. Clicking on a taxon or community",
      " link will open a detailed view of that plant observation or community concept.",
    )
  })

  output$plot_table <- DT::renderDataTable({
    build_plot_table(plot_data, taxa_data, comm_class_data)
  })

  output$comm_table <- DT::renderDataTable({
    build_community_table(comm_concept_data)
  })

  output$proj_table <- DT::renderDataTable({
    build_project_table(project_data)
  })

  output$party_table <- DT::renderDataTable({
    build_party_table(party_data)
  })

  output$plant_table <- DT::renderDataTable({
    build_plant_table(plant_data)
  })

  output$map <- leaflet::renderLeaflet({
    process_map_data(
      map_data = plot_data,
      center_lng = DEFAULT_MAP_LNG,
      center_lat = DEFAULT_MAP_LAT,
      zoom = DEFAULT_MAP_ZOOM
    )
  })


  # EVENT OBSERVERS --------------------------------------------------------------------------------

  # Track map state changes without triggering redraws
  shiny::observeEvent(input$map_zoom,
    {
      zoom <- suppressWarnings(as.numeric(input$map_zoom))
      if (is.null(zoom) || is.na(zoom)) {
        return()
      }

      previous_zoom <- state$map_zoom()
      changed <- !nearly_equal(previous_zoom, zoom, tol = 1e-4)

      state$map_zoom(zoom)
      update_map_custom_flag()

      if (changed && !isTRUE(updating_from_query())) {
        update_app_query(mode = "replace")
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$map_center,
    {
      map_center <- input$map_center
      if (is.null(map_center)) {
        return()
      }

      lat <- suppressWarnings(as.numeric(map_center$lat))
      lng <- suppressWarnings(as.numeric(map_center$lng))

      if (any(is.na(c(lat, lng)))) {
        return()
      }

      previous_lat <- state$map_center_lat()
      previous_lng <- state$map_center_lng()
      changed <- !nearly_equal(previous_lat, lat, tol = 1e-5) || !nearly_equal(previous_lng, lng, tol = 1e-5)

      state$map_center_lat(lat)
      state$map_center_lng(lng)
      update_map_custom_flag()

      if (changed && !isTRUE(updating_from_query())) {
        update_app_query(mode = "replace")
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$page,
    {
      if (is.null(input$page) || !nzchar(input$page)) {
        return()
      }

      state$current_tab(input$page)

      if (isTRUE(updating_from_query())) {
        return()
      }

      mode <- if (isTRUE(history_initialized())) "push" else "replace"

      update_app_query(
        mode = mode,
        tab = input$page,
        detail_type = if (isTRUE(state$details_open())) state$detail_type() else NULL,
        detail_code = if (isTRUE(state$details_open())) state$selected_code() else NULL
      )

      history_initialized(TRUE)
    },
    ignoreNULL = FALSE
  )

  for (key in names(table_registry)) {
    local({
      table_key <- key
      input_id <- paste0(table_registry[[table_key]]$table_id, "_state")

      shiny::observeEvent(input[[input_id]],
        {
          store_table_state(table_key, input[[input_id]])
        },
        ignoreNULL = TRUE
      )
    })
  }

  shiny::observeEvent(input$see_obs_details, {
    vb_code <- input$see_obs_details
    open_detail("plot-observation", vb_code)
  })

  shiny::observeEvent(input$show_on_map,
    {
      map_data <- input$show_on_map
      lat <- as.numeric(map_data$lat)
      lng <- as.numeric(map_data$lng)
      code <- map_data$code

      # Check for valid coordinates
      if (is.na(lat) || is.na(lng) || !is.numeric(lat) || !is.numeric(lng)) {
        shiny::showNotification("Cannot show on map: Missing or invalid coordinates for plot: " + code,
          type = "warning"
        )
        return()
      }

      target_zoom <- map_defaults$detail_zoom %||% DEFAULT_MAP_ZOOM

      state$map_center_lat(lat)
      state$map_center_lng(lng)
      state$map_zoom(target_zoom)
      update_map_custom_flag(lat, lng, target_zoom)

      state$map_request(list(lat = lat, lng = lng, code = code, zoom = target_zoom))

      state$current_tab("Map")

      mode <- if (isTRUE(history_initialized())) "push" else "replace"
      update_app_query(
        mode = mode,
        tab = "Map",
        detail_type = if (isTRUE(state$details_open())) state$detail_type() else NULL,
        detail_code = if (isTRUE(state$details_open())) state$selected_code() else NULL
      )
      shiny::updateNavbarPage(session, "page", selected = "Map")

      map_update_observer <- shiny::observe({
        shiny::req(input$page == "Map")

        map_req <- state$map_request()
        if (!is.null(map_req)) {
          session$sendCustomMessage("invalidateMapSize", list())
          move_map_to_obs(
            session,
            map_req$lat,
            map_req$lng,
            paste("Plot", map_req$code, "is here!")
          )
          state$map_request(NULL)

          map_update_observer$destroy()
        }
      })
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$close_details, {
    close_detail(push_history = TRUE)
  })

  shiny::observeEvent(input$label_link_click, {
    vb_code <- input$label_link_click
    if (!is.null(vb_code) && nchar(vb_code) > 0) {
      open_detail("plot-observation", vb_code)
    }
  })

  shiny::observeEvent(input$comm_class_link_click, {
    vb_code <- input$comm_class_link_click
    open_detail("community-classification", vb_code)
  })

  shiny::observeEvent(input$comm_link_click, {
    vb_code <- input$comm_link_click
    open_detail("community-concept", vb_code)
  })

  shiny::observeEvent(input$taxa_link_click, {
    vb_code <- input$taxa_link_click
    open_detail("taxon-observation", vb_code)
  })

  shiny::observeEvent(input$proj_link_click, {
    vb_code <- input$proj_link_click
    open_detail("project", vb_code)
  })

  shiny::observeEvent(input$party_link_click, {
    vb_code <- input$party_link_click
    open_detail("party", vb_code)
  })

  shiny::observeEvent(input$plant_link_click, {
    vb_code <- input$plant_link_click
    open_detail("plant-concept", vb_code)
  })

  shiny::observeEvent(input$ref_link_click, {
    vb_code <- input$ref_link_click
    open_detail("reference", vb_code)
  })
}


# ================= OTHER FUNCTIONS ===========================================================

#' Handles opening entity detail views with consistent state management
#'
#' @param state The state object containing reactive values
#' @param session Shiny session object
#' @param output Shiny output object
#' @param vb_code The VegBank code for the entity
#' @param detail_type The type of detail view to open
#' @param error_message Custom error message for invalid VegBank codes
#' @return TRUE if successful, FALSE if validation failed
#' @noRd
open_code_details <- function(
    state, session, output, vb_code, detail_type, error_message = NULL,
    comm_class_data = NULL, taxa_data = NULL, plot_data = NULL) {
  if (!is_valid_vb_code(vb_code)) {
    error_msg <- error_message %||% paste0("No VegBank code found for that ", gsub("-", " ", detail_type))
    shiny::showNotification(error_msg, type = "error")
    return(FALSE)
  }

  state$detail_type(detail_type)
  state$selected_code(vb_code)
  state$details_open(TRUE)

  # Use a helper function to find the correct table selection code in case of indirect links
  # Community classifications and taxon observations require looking up the related
  # plot observation to select the correct row.
  if (!is.null(comm_class_data) && !is.null(taxa_data) && !is.null(plot_data)) {
    row_code <- find_row_selection_code(detail_type, vb_code, comm_class_data, taxa_data, plot_data)
    if (!is.null(row_code)) {
      select_table_row_by_code(session, row_code)
    }
  } else {
    # Fallback to direct selection for simple cases
    select_table_row_by_code(session, vb_code)
  }

  show_detail_view(detail_type, vb_code, output, session)

  return(TRUE)
}

#' Validates a VegBank code for use in the application
#'
#' @param vb_code The VegBank code to validate
#' @return TRUE if valid, FALSE if invalid
#' @noRd
is_valid_vb_code <- function(vb_code) {
  !is.null(vb_code) && !is.na(vb_code) &&
    nchar(as.character(vb_code)) > 0 && vb_code != "NA"
}

#' Move the map to the specified latitude and longitude with a popup message
#'
#' @param session Shiny session object
#' @param lat Latitude of the observation
#' @param lng Longitude of the observation
#' @param message Message to display in the popup
#' @noRd
move_map_to_obs <- function(session, lat, lng, message) {
  leaflet::leafletProxy("map", session) |>
    update_map_view(lng = lng, lat = lat, label = message)
}

#' Select a table row by VegBank code using a custom Shiny message
#'
#' @param session The Shiny session object
#' @param vb_code The VegBank code to select
#' @noRd
select_table_row_by_code <- function(session, vb_code) {
  # Skip if invalid VegBank code
  if (!is_valid_vb_code(vb_code)) {
    return()
  }

  # Send selection message to JavaScript
  session$sendCustomMessage("selectTableRowByCode", list(
    vbCode = vb_code
  ))
}

#' Find the correct VegBank code for table row selection based on detail type
#' Community classifications and taxon observations require looking up the related
#' plot observation to select the correct row.
#'
#' @param detail_type The type of detail view
#' @param vb_code The VegBank code from the detail view
#' @param comm_class_data Community classification data frame
#' @param taxa_data Taxon observation data frame
#' @param plot_data Plot observation data frame
#' @return The VegBank code to use for table row selection, or NULL if not found
#' @noRd
find_row_selection_code <- function(detail_type, vb_code, comm_class_data, taxa_data, plot_data) {
  switch(detail_type,
    "plot-observation" = vb_code,
    "community-concept" = vb_code,
    "project" = vb_code,
    "party" = vb_code,
    "plant-concept" = vb_code,
    "community-classification" = {
      # Find the plot row that contains this community classification
      comm_class_row <- which(comm_class_data$cl_code == vb_code)
      # TODO: Make sure an ob_code exists in comm_class_data
      if (length(comm_class_row) > 0) comm_class_data$ob_code[comm_class_row[1]] else NULL
    },
    "taxon-observation" = {
      # Find the plot row that contains this taxon observation
      taxa_row <- which(taxa_data$to_code == vb_code)
      if (length(taxa_row) > 0) {
        ob_code <- taxa_data$ob_code[taxa_row[1]]
        plot_row_index <- which(plot_data$ob_code == ob_code)
        # TODO: Make sure an ob_code exists in taxa_data and is linking entities appropriately here
        if (length(plot_row_index) > 0) plot_data$ob_code[plot_row_index[1]] else NULL
      } else {
        NULL
      }
    },
    NULL # Unknown detail type
  )
}

#' Helper function to load data types with API fallback
#' This function will try to load data from the API first, and if it fails, it
#' will fall back to reading from a cached RDS file.
#' If the API is not used, it will only read from the cached file.
# Returns a data frame with the loaded data or an empty data frame if loading fails.
#'
#' @param data_type Name of the data type being loaded (for progress messages)
#' @param file_path Path to the cached RDS file
#' @param api_function The vegbankr API function to call
#' @param api_params Additional parameters to pass to the API function
#' @return A data frame with the loaded data or an empty data frame if loading fails.
#'
#' @importFrom utils modifyList
#' @noRd
load_data_type <- function(data_type, file_path, api_function, api_params = list(), use_api = FALSE) {
  shiny::incProgress(0.2, detail = paste0("Loading ", data_type, "..."))
  # Special case for taxon observations (count was too slow so cannot fetch all and have to read from cache)
  if (use_api && data_type == "taxon observations") {
    shiny::showNotification(
      "Taxa API requests don't return a count - using cached data instead",
      type = "warning", duration = NULL
    )
    return(read_from_cache(data_type, file_path))
  }

  # For other data types, use API if requested
  if (use_api) {
    tryCatch(
      {
        # Call the API function with parameters
        params <- list(limit = 0)
        params <- modifyList(params, api_params)

        # Get total count first
        count_call <- do.call(api_function, params)
        num_items <- vegbankr::get_page_details(count_call)[["count_reported"]]

        # Now get all data
        params$limit <- num_items
        do.call(api_function, params)
      },
      error = function(e) {
        shiny::showNotification(
          paste0("Error fetching ", data_type, " from API: ", e$message),
          type = "error", duration = NULL
        )
        read_from_cache(data_type, file_path)
      }
    )
  } else {
    read_from_cache(data_type, file_path)
  }
}

#' Checks if the specified file exists and reads it as an RDS file.
#' If the file does not exist, it shows an error notification and returns an empty data frame.
#'
#' @param data_type Name of the data type being loaded (for error messages)
#' @param file_path Path to the cached RDS file
#' @return A data frame with the loaded data or an empty data frame if loading fails.
#' @noRd
read_from_cache <- function(data_type, file_path) {
  if (file.exists(file_path)) {
    readRDS(file_path)
  } else {
    shiny::showNotification(
      paste0(data_type, " cache not found: ", file_path),
      type = "error", duration = NULL
    )
    data.frame()
  }
}
