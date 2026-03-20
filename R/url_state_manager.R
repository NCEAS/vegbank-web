#' URL State Manager for Browser History Navigation
#'
#' R6 class that encapsulates URL query string management for Shiny applications.
#' Provides methods for encoding/decoding state, synchronizing with browser history,
#' and preventing circular updates.
#'
#' @importFrom R6 R6Class
#' @importFrom shiny reactiveVal
#' @noRd

URLStateManager <- R6::R6Class(
  "URLStateManager",
  public = list(
    #' @description
    #' Initialize URL State Manager
    #' @param session Shiny session object
    #' @param defaults Named list of default values (e.g., map_lat, map_lng, etc.)
    #' @param table_registry Named list mapping table keys to configuration
    initialize = function(session, defaults = list(), table_registry = list()) {
      private$session <- session
      private$defaults <- defaults
      private$table_registry <- table_registry
      private$updating <- FALSE
      private$history_initialized <- FALSE

      # Build reverse lookup: tab name -> table key
      if (length(table_registry) > 0) {
        private$tab_to_key <- setNames(
          names(table_registry),
          vapply(table_registry, `[[`, character(1), "tab")
        )
      }
    },

    #' @description
    #' Check if currently updating from URL (prevents circular updates)
    is_updating = function() {
      private$updating
    },

    #' @description
    #' Set updating flag
    #' @param value TRUE or FALSE
    set_updating = function(value) {
      private$updating <- value
    },

    #' @description
    #' Check if history has been initialized
    is_history_initialized = function() {
      private$history_initialized
    },

    #' @description
    #' Mark history as initialized
    set_history_initialized = function(value = TRUE) {
      private$history_initialized <- value
    },

    #' @description
    #' Parse query string into named list
    #' @return Named list of query parameters
    parse_query_string = function() {
      query_string <- private$get_query_string()
      if (identical(query_string, "")) {
        list()
      } else {
        shiny::parseQueryString(query_string)
      }
    },

    #' @description
    #' Extract first element from query parameter array
    #' @param value Query parameter value
    #' @return First element or NULL
    first_param = function(value) {
      if (is.null(value) || length(value) == 0) {
        return(NULL)
      }
      value[[1]]
    },

    #' @description
    #' Check if query parameter is valid (non-null, non-empty)
    #' @param value Query parameter value
    #' @return TRUE if valid
    is_valid_param = function(value) {
      candidate <- self$first_param(value)
      !is.null(candidate) && !is.na(candidate) && nzchar(candidate)
    },

    #' @description
    #' Parse numeric query parameter safely
    #' @param value Query parameter value (may be NULL, NA, or empty)
    #' @return Numeric value or NULL if invalid
    parse_numeric_param = function(value) {
      raw_value <- self$first_param(value)
      if (is.null(raw_value) || isTRUE(is.na(raw_value)) || identical(raw_value, "")) {
        return(NULL)
      }

      candidate <- suppressWarnings(as.numeric(raw_value))
      if (length(candidate) == 0 || isTRUE(is.na(candidate))) NULL else candidate
    },

    #' @description
    #' Format coordinate for URL (5 decimal places)
    #' @param value Numeric coordinate value
    #' @return Formatted string
    format_coord = function(value) {
      sprintf("%.5f", as.numeric(value))
    },

    #' @description
    #' Format zoom level for URL
    #' @param value Numeric zoom value
    #' @return Formatted string
    format_zoom = function(value) {
      formatted <- format(signif(as.numeric(value), digits = 6), trim = TRUE, scientific = FALSE)
      sub("\\.0+$", "", formatted)
    },

    #' @description
    #' Compare numeric values with tolerance
    #' @param a First value
    #' @param b Second value
    #' @param tol Tolerance for comparison
    #' @return TRUE if nearly equal
    nearly_equal = function(a, b, tol = 1e-6) {
      if (is.null(a) || is.null(b)) {
        return(FALSE)
      }
      diff <- suppressWarnings(abs(as.numeric(a) - as.numeric(b)))
      !is.na(diff) && diff <= tol
    },

    #' @description
    #' Check if map coordinates match defaults
    #' @param lat Latitude
    #' @param lng Longitude
    #' @param zoom Zoom level
    #' @return TRUE if at default position
    is_map_default = function(lat, lng, zoom) {
      self$nearly_equal(lat, private$defaults$map_lat) &&
        self$nearly_equal(lng, private$defaults$map_lng) &&
        self$nearly_equal(zoom, private$defaults$map_zoom)
    },

    #' @description
    #' Get table key from tab name safely
    #' @param tab_name The name of the navigation tab
    #' @return The table registry key, or NULL if tab has no table
    get_table_key = function(tab_name) {
      if (is.null(tab_name) || !nzchar(tab_name)) {
        return(NULL)
      }

      if (!tab_name %in% names(private$tab_to_key)) {
        return(NULL)
      }

      private$tab_to_key[[tab_name]]
    },

    #' @description
    #' Parse DataTables order entries from various formats
    #' @param entries Order data from DataTables state
    #' @return List of order entries with column and direction, or NULL
    parse_order_entries = function(entries) {
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
    },

    #' @description
    #' Serialize table sort order for URL query string
    #' @param order List of order entries from table state
    #' @return Comma-separated string like "0:asc,1:desc"
    serialize_order_for_query = function(order) {
      if (is.null(order) || length(order) == 0) {
        return(NULL)
      }

      parts <- vapply(order, function(item) {
        paste(item$column, item$dir, sep = ":")
      }, character(1))

      paste(parts, collapse = ",")
    },

    #' @description
    #' Deserialize table sort order from URL query string
    #' @param order_string Query parameter value like "0:asc,1:desc"
    #' @return List of order entries with column and direction
    deserialize_order_from_query = function(order_string) {
      order_value <- self$first_param(order_string)
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
    },

    #' @description
    #' Normalize and validate DataTables state object
    #' @param raw_state Raw state object from DataTables
    #' @return Sanitized state list with start, length, order, search
    sanitize_table_state = function(raw_state) {
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
      order <- self$parse_order_entries(order_entries)

      if (length(start) == 0 || is.na(start)) start <- 0L
      if (length(length) == 0 || is.na(length) || length <= 0) length <- 100L
      if (is.null(search)) search <- ""

      list(
        start = start,
        length = length,
        order = order,
        search = as.character(search)
      )
    },

    #' @description
    #' Extract table state from URL query parameters
    #' @param key Table registry key (e.g., "plots")
    #' @param params Parsed query string parameters
    #' @return Table state list or NULL if no state present
    extract_table_state_from_query = function(key, params) {
      prefix <- key
      start <- self$parse_numeric_param(params[[paste0(prefix, "_start")]])
      length <- self$parse_numeric_param(params[[paste0(prefix, "_length")]])
      order <- self$deserialize_order_from_query(params[[paste0(prefix, "_order")]])
      search <- self$first_param(params[[paste0(prefix, "_search")]]) %||% ""

      if (is.null(start) && is.null(length) && is.null(order) && !nzchar(search)) {
        return(NULL)
      }

      default_length <- private$table_registry[[key]]$default_length %||% 100

      if (is.null(start)) start <- 0
      if (is.null(length) || length <= 0) length <- default_length

      list(
        start = as.integer(start),
        length = as.integer(length),
        order = order,
        search = as.character(search)
      )
    },

    #' @description
    #' Check if table state matches default values
    #' @param key Table registry key
    #' @param state Table state object
    #' @return TRUE if state is default
    is_default_table_state = function(key, state) {
      if (is.null(state)) {
        return(TRUE)
      }

      default_length <- private$table_registry[[key]]$default_length %||% 100L
      start_default <- is.null(state$start) || state$start <= 0
      length_default <- is.null(state$length) || identical(as.integer(state$length), as.integer(default_length))
      search_default <- is.null(state$search) || identical(state$search, "")
      order_default <- is.null(state$order) || length(state$order) == 0

      start_default && length_default && search_default && order_default
    },

    #' @description
    #' Build query string from state components
    #' @param tab Tab name
    #' @param detail_type Detail overlay type
    #' @param detail_code Detail overlay code
    #' @param map_lat Map latitude
    #' @param map_lng Map longitude
    #' @param map_zoom Map zoom level
    #' @param map_has_custom_state Whether map has custom state
    #' @param table_states List of table states
    #' @param highlight_table Table ID where selection occurred
    #' @param highlight_row Row index of selection
    #' @param plot_filter Plot table filter (list with type, code, label) or NULL
    #' @param community_filter Community table filter (list with type, code, label) or NULL
    #' @return Query string (e.g., "?tab=Plots&detail=plot&code=123")
    build_query_string = function(tab = NULL, detail_type = NULL, detail_code = NULL,
                                  map_lat = NULL, map_lng = NULL, map_zoom = NULL,
                                  map_has_custom_state = FALSE, table_states = list(),
                                  highlight_table = NULL, highlight_row = NULL,
                                  plot_filter = NULL, community_filter = NULL,
                                  community_status = NULL, plant_status = NULL) {
      if (is.null(tab) || !nzchar(tab)) {
        tab <- "Overview"
      }

      params <- list()
      params$tab <- tab

      if (!is.null(detail_type) && !is.null(detail_code) &&
            nzchar(detail_type) && nzchar(detail_code)) {
        params$detail <- detail_type
        params$code <- detail_code

        # Include highlight state when detail overlay is open
        if (!is.null(highlight_table) && nzchar(highlight_table)) {
          params$hl_table <- highlight_table
        }
        if (!is.null(highlight_row) && !is.na(highlight_row)) {
          params$hl_row <- as.character(as.integer(highlight_row))
        }
      }

      # Include plot filter parameters for cross-resource queries
      if (!is.null(plot_filter) && is.list(plot_filter)) {
        if (!is.null(plot_filter$code) && nzchar(plot_filter$code)) {
          params$plot_filter_code <- plot_filter$code
        }
        if (!is.null(plot_filter$type) && nzchar(plot_filter$type)) {
          params$plot_filter_type <- plot_filter$type
        }
        if (!is.null(plot_filter$label) && nzchar(plot_filter$label)) {
          params$plot_filter_label <- plot_filter$label
        }
      }

      # Include community filter parameters
      if (!is.null(community_filter) && is.list(community_filter)) {
        if (!is.null(community_filter$code) && nzchar(community_filter$code)) {
          params$comm_filter_code <- community_filter$code
        }
        if (!is.null(community_filter$type) && nzchar(community_filter$type)) {
          params$comm_filter_type <- community_filter$type
        }
        if (!is.null(community_filter$label) && nzchar(community_filter$label)) {
          params$comm_filter_label <- community_filter$label
        }
      }

      # Include community status filter (omit when it's the default)
      if (!is.null(community_status) && nzchar(community_status) &&
          !identical(community_status, "current_accepted")) {
        params$communities_status <- community_status
      }

      # Include plant status filter (omit when it's the default)
      if (!is.null(plant_status) && nzchar(plant_status) &&
          !identical(plant_status, "current_accepted")) {
        params$plants_status <- plant_status
      }

      include_map_params <- identical(tab, "Map") || isTRUE(map_has_custom_state)

      if (include_map_params && !anyNA(c(map_lat, map_lng, map_zoom))) {
        params$map_lat <- self$format_coord(map_lat)
        params$map_lng <- self$format_coord(map_lng)
        params$map_zoom <- self$format_zoom(map_zoom)
      }

      # Include table state for ALL tables with non-default state, not just current tab
      # This ensures highlights on other tabs are restored to the correct page
      for (table_key in names(table_states)) {
        table_state <- table_states[[table_key]]
        if (is.null(table_state)) next

        params[[paste0(table_key, "_start")]] <- as.character(table_state$start)
        params[[paste0(table_key, "_length")]] <- as.character(table_state$length)

        order_string <- self$serialize_order_for_query(table_state$order)
        if (!is.null(order_string)) {
          params[[paste0(table_key, "_order")]] <- order_string
        }

        if (!is.null(table_state$search) && nzchar(table_state$search)) {
          params[[paste0(table_key, "_search")]] <- table_state$search
        }
      }

      if (length(params) > 0) {
        paste0(
          "?",
          paste(
            names(params),
            vapply(params, private$encode_query_value, character(1)),
            sep = "=",
            collapse = "&"
          )
        )
      } else {
        ""
      }
    },

    #' @description
    #' Update browser URL query string
    #' @param query_string New query string
    #' @param mode "push" or "replace"
    #' @return TRUE if URL was updated, FALSE if no change
    update_query_string = function(query_string, mode = c("push", "replace")) {
      mode <- match.arg(mode)

      current_query <- private$get_query_string()
      if (identical(query_string, current_query)) {
        return(invisible(FALSE))
      }

      shiny::updateQueryString(query_string, mode = mode, session = private$session)
      invisible(TRUE)
    }
  ),
  private = list(
    session = NULL,
    defaults = NULL,
    table_registry = NULL,
    tab_to_key = NULL,
    updating = NULL,
    history_initialized = NULL,

    #' Get current browser query string
    #' @return Query string or empty string
    get_query_string = function() {
      query <- private$session$clientData$url_search
      if (is.null(query) || length(query) == 0 || isTRUE(is.na(query)) || identical(query, "")) {
        ""
      } else {
        query
      }
    },

    #' URL-encode query parameter value
    #' @param value Value to encode
    #' @return Encoded string
    encode_query_value = function(value) {
      utils::URLencode(as.character(value), reserved = TRUE)
    }
  )
)
