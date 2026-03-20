#' Shiny Server for VegBank Web Application
#'
#' Initializes server-side functionality for data visualization and interactivity.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return Called for its side effects.
#'
#' @importFrom rlang .data
#' @importFrom shiny reactiveVal observeEvent observe req renderUI renderText showNotification
#'             updateNavbarPage invalidateLater reactiveValuesToList onBookmark onBookmarked
#'             onRestore isolate
#' @importFrom DT renderDataTable datatable dataTableProxy
#' @importFrom htmltools tags
#' @importFrom leaflet renderLeaflet leafletProxy
#' @importFrom utils head write.csv
#' @import vegbankr
#'
#' @noRd
# ================= MAIN SERVER FUNCTION ===========================================================
server <- function(input, output, session) {
  cfg <- config::get()
  if (isTRUE(cfg$vb_debug)) vegbankr::vb_debug()
  vegbankr::vb_set_base_url(cfg$vb_base_url)

  # MAP DEFAULTS ------------------------------------------------------------------------------------
  # Get default map settings from constants module
  map_defaults <- get_map_defaults()
  DEFAULT_MAP_LAT <- map_defaults$lat
  DEFAULT_MAP_LNG <- map_defaults$lng
  DEFAULT_MAP_ZOOM <- map_defaults$zoom

  # STATE INITIALIZATION ---------------------------------------------------------------------------
  # Browser history and URL synchronization state management

  # Initialize map state with defaults
  map_state <- initialize_map_state()

  state <- list(
    current_tab = shiny::reactiveVal("Home"),
    details_open = shiny::reactiveVal(FALSE),
    detail_type = shiny::reactiveVal(NULL),
    detail_code = shiny::reactiveVal(NULL),
    highlighted_table = shiny::reactiveVal(NULL),
    highlighted_row = shiny::reactiveVal(NULL),
    map_center_lat = map_state$map_center_lat,
    map_center_lng = map_state$map_center_lng,
    map_zoom = map_state$map_zoom,
    map_has_custom_state = shiny::reactiveVal(FALSE),
    map_request = shiny::reactiveVal(NULL),
    plot_filter = shiny::reactiveVal(NULL), # Cross-resource / citation filter: list(type, code, label)
    community_filter = shiny::reactiveVal(NULL), # Citation filter for cc: list(type, code, label)
    table_states = shiny::reactiveValues(),
    table_sync_pending = shiny::reactiveValues(),
    table_sync_completed_at = shiny::reactiveValues()
  )

  # Table registry: maps tab names to table IDs and default page lengths
  # Used to encode/decode table state in the URL query string
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L),
    plants = list(tab = "Plants", table_id = "plant_table", default_length = 100L),
    communities = list(tab = "Communities", table_id = "comm_table", default_length = 100L),
    parties = list(tab = "Parties", table_id = "party_table", default_length = 100L),
    projects = list(tab = "Projects", table_id = "proj_table", default_length = 100L)
  )

  for (key in names(table_registry)) {
    state$table_sync_pending[[key]] <- FALSE
  }

  # Map data state: observations for the leaflet map and fetch status flag
  map_observations <- shiny::reactiveVal(NULL)
  map_fetch_in_progress <- shiny::reactiveVal(FALSE)
  map_initialized <- shiny::reactiveVal(FALSE)
  map_filter_notice_shown <- shiny::reactiveVal(FALSE)

  # Overview data state so we can lazy load that page once a session
  overview_initialized <- shiny::reactiveVal(FALSE)

  # Initialize URL State Manager with defaults and table registry
  url_manager <- URLStateManager$new(
    session = session,
    defaults = list(
      map_lat = DEFAULT_MAP_LAT,
      map_lng = DEFAULT_MAP_LNG,
      map_zoom = DEFAULT_MAP_ZOOM
    ),
    table_registry = table_registry
  )

  # Initialize current_tab from URL before any render logic runs.
  # Citation URLs arrive as ?cite=IDENTIFIER after an HTTP 302 redirect from /cite/ paths.
  initial_query <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search))
  has_cite_param <- !is.null(initial_query$cite) && nzchar(initial_query$cite)

  # For citation URLs, don't pre-select a tab — the citation resolver will navigate
  # to the correct tab after resolving the identifier. For all other URLs, parse the
  # tab parameter or default to Home.
  if (has_cite_param) {
    # Leave state$current_tab at its default ("Home") but don't initialize any
    # tab-specific data. The citation resolver will set the real tab.
    initial_tab <- NULL
  } else {
    initial_tab <- if (!is.null(initial_query$tab) && nzchar(initial_query$tab)) {
      initial_query$tab
    } else {
      "Home"
    }
    state$current_tab(initial_tab)
  }

  session$onFlushed(function() {
    if (!url_manager$is_history_initialized()) {
      session$sendCustomMessage("setNavInteractivity", list(disabled = TRUE))
    }
  }, once = TRUE)

  # Reactive accessor for current URL query parameters
  current_query <- shiny::reactive({
    url_manager$parse_query_string()
  })

  # INTERNAL HELPER FUNCTIONS ----------------------------------------------------------------------
  # These functions are nested within server() to access closure variables (state, url_manager, etc.)

  # --- Map State Helpers -----

  # Update the flag that tracks whether the map is at default position or has been customized
  # This flag determines whether map coordinates should be included in the URL query string
  update_map_custom_flag <- function(lat = state$map_center_lat(), lng = state$map_center_lng(), zoom = state$map_zoom()) {
    state$map_has_custom_state(!url_manager$is_map_default(lat, lng, zoom))
  }

  can_mutate_history <- function() {
    url_manager$is_history_initialized() && !url_manager$is_updating()
  }


  # --- DataTable State Management -----

  # Build a message object for applying table state on the client side via custom JavaScript handler
  # Converts server-side table state into format expected by DataTables JavaScript API
  # Parameters:
  #   key - Table registry key (e.g., "plots", "plants")
  #   state - Sanitized table state with start, length, order, search
  # Returns: Message list for custom JS handler, or NULL if invalid inputs
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

  # Send table state to client-side JavaScript to restore DataTable view.
  # During a deep-link/URL restoration this function is a no-op until the
  # browser history is fully initialized; afterwards it is used whenever the
  # server needs to push a previously saved state back down (e.g., browser
  # back/forward). The payload matches the structure expected by the
  # client-side `applyTableState` handler in `R/ui.R`.
  send_table_state_to_client <- function(key, state) {
    if (!url_manager$is_history_initialized()) {
      return()
    }

    message <- table_state_message(key, state)
    if (!is.null(message)) {
      session$sendCustomMessage("applyTableState", message)
    }
  }

  # Store table state received from the JS `stateSaveCallback` and decide whether
  # to update the encoded URL. Each DataTable widget sends its state through a
  # Shiny input (`<table_id>_state`); we sanitize the payload, de-dupe against the
  # current snapshot, and only mutate browser history when the active tab matches
  # the table emitting the event. This keeps the URL and Shiny’s authoritative
  # state in sync without double-applying DT settings.
  # Parameters:
  #   key - Table registry key
  #   raw_state - Raw state object from DataTables (needs sanitization)
  store_table_state <- function(key, raw_state) {
    sanitized <- url_manager$sanitize_table_state(raw_state)
    if (is.null(sanitized)) {
      return()
    }

    tab_for_key <- table_registry[[key]]$tab
    pending_sync <- shiny::isolate(isTRUE(state$table_sync_pending[[key]]))
    current <- shiny::isolate(state$table_states[[key]])
    sync_completed_at <- shiny::isolate(state$table_sync_completed_at[[key]])

    # When a sync is pending (we're restoring state from URL), ignore incoming
    # state reports until the table settles at the expected state. This prevents
    # race conditions where the table reports its initial state (start=0) before
    # the URL-specified pagination is applied.
    if (pending_sync) {
      if (!is.null(current) && identical(current, sanitized)) {
        # Table has reached the expected state - sync complete
        state$table_sync_pending[[key]] <- FALSE
        # Record when sync completed to ignore stale default state callbacks
        state$table_sync_completed_at[[key]] <- Sys.time()
      }
      # Either way, don't update URL while sync is pending
      return()
    }

    # Grace period: ignore default state callbacks within 10 seconds of sync completion
    # This handles race conditions where stale default state callbacks arrive after sync
    # The delay can be significant due to server-side AJAX processing and table re-rendering
    if (!is.null(sync_completed_at)) {
      grace_period_active <- difftime(Sys.time(), sync_completed_at, units = "secs") < 10
      if (grace_period_active && url_manager$is_default_table_state(key, sanitized)) {
        return()
      }
      # Clear the completed timestamp once grace period is over or we have non-default state
      state$table_sync_completed_at[[key]] <- NULL
    }

    if (url_manager$is_default_table_state(key, sanitized)) {
      if (!is.null(current)) {
        state$table_states[[key]] <- NULL

        if (can_mutate_history() && identical(tab_for_key, state$current_tab())) {
          update_app_query(mode = "replace")
        }
      }
      return()
    }

    if (!is.null(current) && identical(current, sanitized)) {
      return()
    }

    state$table_states[[key]] <- sanitized

    if (can_mutate_history() && identical(tab_for_key, state$current_tab())) {
      update_app_query(mode = "replace")
    }
  }

  # --- URL and Browser History Management ---

  # Update the browser history to reflect current application state
  # This is the core function that encodes all app state (tab, detail view, map, tables)
  # into a URL query string and pushes it to browser history via History API
  #
  # The URL encodes:
  #   - Current tab (tab=Plots)
  #   - Detail overlay state (detail=plot-observation&code=VB.123)
  #   - Map position if non-default (map_lat, map_lng, map_zoom)
  #   - Table state for current tab (plots_start, plots_length, plots_order, plots_search)
  #
  # Parameters:
  #   mode - "push" to create new history entry, "replace" to update current entry
  #   tab - Tab name; if NULL, uses current tab from state
  #   detail_type - Entity type for detail overlay (e.g., "plot-observation")
  #   detail_code - Entity code for detail overlay (e.g., "VB.123")
  update_app_query <- function(mode = c("push", "replace"),
                               tab = NULL, detail_type = NULL, detail_code = NULL) {
    mode <- match.arg(mode)

    if (is.null(tab)) {
      tab <- state$current_tab()
    }

    # If detail params not provided, get from current state if overlay is open
    if (is.null(detail_type) || is.null(detail_code)) {
      if (isTRUE(state$details_open())) {
        detail_type <- state$detail_type()
        detail_code <- state$detail_code()
      } else {
        detail_type <- NULL
        detail_code <- NULL
      }
    }

    # Get table states, being careful about sync pending state
    table_states_list <- shiny::reactiveValuesToList(state$table_states)

    # Build query string using URL manager
    target_query <- url_manager$build_query_string(
      tab = tab,
      detail_type = detail_type,
      detail_code = detail_code,
      map_lat = state$map_center_lat(),
      map_lng = state$map_center_lng(),
      map_zoom = state$map_zoom(),
      map_has_custom_state = state$map_has_custom_state(),
      table_states = table_states_list,
      highlight_table = state$highlighted_table(),
      highlight_row = state$highlighted_row(),
      plot_filter = state$plot_filter(),
      community_filter = state$community_filter()
    )

    url_manager$update_query_string(target_query, mode = mode)
  }

  # --- Detail Overlay Navigation -----

  # Open entity detail overlay (sidebar) with optional browser history tracking
  # Displays detailed information about a specific entity (plot, taxon, community, etc.)
  # Optionally updates browser URL and history to make detail view shareable/bookmarkable
  #
  # Parameters:
  #   detail_type - Entity type: "plot-observation", "plant-concept", "community-concept",
  #                 "community-classification", "project", "party", "reference"
  #   vb_code - VegBank entity code (e.g., "VB.123")
  #   push_history - If TRUE, updates URL and adds browser history entry
  #   history_mode - "push" creates new history entry, "replace" updates current entry
  #   skip_highlight - If TRUE, skip server-side row highlight (client handles it from URL)
  open_detail <- function(detail_type, vb_code, push_history = TRUE, history_mode = "push",
                          skip_highlight = FALSE) {
    success <- open_code_details(
      state = state,
      session = session,
      output = output,
      vb_code = vb_code,
      detail_type = detail_type,
      skip_highlight = skip_highlight
    )

    # Update URL and browser history if successful and requested
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

  # Close entity detail overlay with optional browser history tracking
  # Hides the detail sidebar and clears detail state
  # Optionally updates browser URL to remove detail parameters
  #
  # Parameters:
  #   push_history - If TRUE, updates URL and adds browser history entry
  #   history_mode - "push" creates new history entry, "replace" updates current entry
  #   hide_overlay - If TRUE, sends message to client to close overlay UI
  close_detail <- function(push_history = TRUE, history_mode = "push", hide_overlay = TRUE) {
    # If already closed, just ensure state is cleared
    if (!isTRUE(state$details_open())) {
      state$detail_type(NULL)
      state$detail_code(NULL)
      return(invisible(FALSE))
    }

    # Clear detail state
    state$details_open(FALSE)
    state$detail_type(NULL)
    state$detail_code(NULL)

    # Clear highlighted table and row state
    state$highlighted_table(NULL)
    state$highlighted_row(NULL)

    # Hide overlay UI element via custom message
    if (hide_overlay) {
      session$sendCustomMessage("closeOverlay", list())
    }

    # Clear any table row highlights
    session$sendCustomMessage("clearAllTableHighlights", list())

    # Update URL to remove detail parameters
    if (push_history) {
      update_app_query(mode = history_mode, tab = state$current_tab(), detail_type = NULL, detail_code = NULL)
    }

    invisible(TRUE)
  }

  # --- Citation Resolution -----

  # Resolve a ?cite=IDENTIFIER URL and navigate to the appropriate view.
  # Called from the URL observer when a ?cite= query parameter is detected.
  #
  # Uses resolve_citation() for API lookup, then directly applies navigation
  # using existing server-side functions (open_detail, state mutations, tab
  # switching) rather than relying on a URL round-trip.
  #
  # Supported citation types:
  #   - Single entities (Plots/Communities): filter + detail overlay + highlight
  #   - Datasets: cross-resource filter on Plots tab
  #   - Unsupported types: error notification, stay on current tab
  #
  # The citation loading overlay (already visible from page load) is hidden
  # when resolution completes or fails.
  resolve_and_redirect_citation <- function(identifier) {
    citation <- resolve_citation(identifier)

    if (is.null(citation)) {
      session$sendCustomMessage("hideLoadingOverlay", list(type = "citation"))

      shiny::showNotification(
        paste0("Could not resolve citation: ", identifier),
        type = "error",
        duration = NULL
      )

      # Fall back to Home tab
      state$current_tab("Home")
      shiny::updateNavbarPage(session, "page", selected = "Home")
      update_app_query(mode = "replace", tab = "Home")
      session$sendCustomMessage("setNavInteractivity", list(disabled = FALSE))
      return()
    }

    # Only datasets, plot observations (Plots tab), and community concepts
    # (Communities tab) support citation filtering.
    supported_citation_tabs <- c("Plots", "Communities")
    is_dataset <- citation$resource_info$api_type == "user-datasets"

    if (!is_dataset && !isTRUE(citation$tab %in% supported_citation_tabs)) {
      session$sendCustomMessage("hideLoadingOverlay", list(type = "citation"))

      shiny::showNotification(
        paste0("Citations are not supported for ",
               citation$resource_info$plural %||% citation$detail_type, "."),
        type = "warning",
        duration = NULL
      )

      # Fall back to Home tab
      state$current_tab("Home")
      shiny::updateNavbarPage(session, "page", selected = "Home")
      update_app_query(mode = "replace", tab = "Home")
      session$sendCustomMessage("setNavInteractivity", list(disabled = FALSE))
      return()
    }

    filter_type <- if (is_dataset) "collection-citation" else "single-entity-citation"

    filter_info <- list(
      type = filter_type,
      code = citation$vb_code,
      label = paste("Citation identifier:", identifier),
      resource_type = citation$detail_type,
      resource_info = citation$resource_info
    )

    if (is_dataset) {
      state$plot_filter(filter_info)
      state$current_tab("Plots")
      shiny::updateNavbarPage(session, "page", selected = "Plots")
      update_app_query(mode = "replace", tab = "Plots")
    } else {
      if (citation$tab == "Plots") {
        state$plot_filter(filter_info)
        state$highlighted_table("plot_table")
      } else if (citation$tab == "Communities") {
        state$community_filter(filter_info)
        state$highlighted_table("comm_table")
      }

      state$highlighted_row(0L)
      state$current_tab(citation$tab)
      shiny::updateNavbarPage(session, "page", selected = citation$tab)

      open_detail(
        detail_type = citation$detail_type,
        vb_code = citation$vb_code,
        push_history = FALSE,
        skip_highlight = FALSE
      )

      update_app_query(
        mode = "replace",
        tab = citation$tab,
        detail_type = citation$detail_type,
        detail_code = citation$vb_code
      )
    }

    session$sendCustomMessage("hideLoadingOverlay", list(type = "citation"))
    session$sendCustomMessage("setNavInteractivity", list(disabled = FALSE))
  }

  # Helper function to create filter alert UI
  create_filter_alert <- function(filter, resource_singular, resource_plural, clear_input_id) {
    if (is.null(filter)) {
      return(NULL)
    }

    # Customize message based on filter type
    message <- if (filter$type == "single-entity-citation") {
      # e.g. "Showing the cited plot observation: ob.2948 (Citation identifier: VB.Ob.2948.ACAD143)"
      sprintf("Showing the cited %s: %s (%s)", resource_singular, filter$code, filter$label)
    } else if (filter$type == "collection-citation") {
      # e.g. "Showing plot observations in dataset: ds.200278 (Citation identifier: ds.200278)"
      sprintf("Showing plot observations in dataset: %s (%s)", filter$code, filter$label)
    } else {
      sprintf(
        "Showing %s related to %s: %s (%s)",
        resource_plural, filter$type, filter$code, filter$label
      )
    }

    htmltools::tags$div(
      class = "alert alert-info alert-dismissible show d-flex align-items-center justify-content-between",
      role = "alert",
      htmltools::tags$strong(message),
      htmltools::tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-info ms-3",
        onclick = sprintf("Shiny.setInputValue('%s', Math.random());", clear_input_id),
        "Clear filter"
      )
    )
  }

  MAP_FILTER_NOTICE_MSG <- paste0(
    "Filters applied to the Plots table don't affect the Map. ",
    "You can only view all of the plots in VegBank simultaneously."
  )

  # Show the map-filter notification once per session, if the Plots table
  # currently has an active search or cross-resource filter.
  # No-ops when already shown or when no filter is active.
  maybe_show_map_filter_notice <- function() {
    if (isTRUE(map_filter_notice_shown())) return()
    plots_search <- shiny::isolate(state$table_states[["plots"]]$search)
    has_table_search <- !is.null(plots_search) && nzchar(plots_search)
    has_cross_filter <- !is.null(shiny::isolate(state$plot_filter()))
    if (has_table_search || has_cross_filter) {
      map_filter_notice_shown(TRUE)
      shiny::showNotification(MAP_FILTER_NOTICE_MSG, type = "message", duration = 10)
    }
  }

  # RENDER UI ELEMENTS --------------------------------------------------------------------------------


  # Initialize overview immediately if that's the initial tab (not a citation URL)
  if (identical(initial_tab, "Overview")) {
    init_overview(output, state, session)
    overview_initialized(TRUE)
  }

  # Initialize map data fetch when navigating directly to the Map tab.
  # The map data observeEvent below uses ignoreInit = TRUE so it only handles
  # subsequent tab changes. This block handles the initial page load case.
  if (identical(initial_tab, "Map")) {
    # Defer the fetch to after Shiny is fully initialized (map output registered)
    session$onFlushed(function() {
      if (is.null(isolate(map_observations())) && !isTRUE(isolate(map_fetch_in_progress()))) {
        session$sendCustomMessage("showLoadingOverlay", list(type = "map"))
        session$sendCustomMessage("setNavInteractivity", list(disabled = TRUE))
        map_fetch_in_progress(TRUE)
        observations <- do_map_fetch_with_feedback()
        map_fetch_in_progress(FALSE)
        if (!is.null(observations)) {
          map_observations(observations)
        } else {
          session$sendCustomMessage("setNavInteractivity", list(disabled = FALSE))
          session$sendCustomMessage("hideLoadingOverlay", list(type = "map"))
        }
      }
    }, once = TRUE)
  }

  output$plot_filter_alert <- shiny::renderUI({
    filter <- state$plot_filter()
    if (is.null(filter)) {
      return(NULL)
    }

    # Get display names from resource_info if available, otherwise use defaults
    resource_singular <- filter$resource_info$singular %||% "plot observation"
    resource_plural <- filter$resource_info$plural %||% "plot observations"

    create_filter_alert(
      filter,
      resource_singular = resource_singular,
      resource_plural = resource_plural,
      clear_input_id = "clear_plot_filter"
    )
  })

  output$comm_filter_alert <- shiny::renderUI({
    filter <- state$community_filter()
    if (is.null(filter)) {
      return(NULL)
    }

    # Get display names from resource_info if available, otherwise use defaults
    resource_singular <- filter$resource_info$singular %||% "community concept"
    resource_plural <- filter$resource_info$plural %||% "community concepts"

    create_filter_alert(
      filter,
      resource_singular = resource_singular,
      resource_plural = resource_plural,
      clear_input_id = "clear_comm_filter"
    )
  })

  output$plot_table <- DT::renderDataTable({
    # Rebuild table when filter changes to pass vb_code for cross-resource queries
    filter <- state$plot_filter()
    vb_code <- if (!is.null(filter)) filter$code else NULL
    filter_type <- if (!is.null(filter)) filter$type else NULL
    build_plot_table_with_filter(vb_code, filter_type)
  })

  # Download handler for plot table
  output$download_plot_table <- create_table_download_handler("plot_table", input, state, session)

  output$comm_table <- DT::renderDataTable({
    # Rebuild table when filter changes for citation filtering
    filter <- state$community_filter()
    vb_code <- if (!is.null(filter)) filter$code else NULL
    filter_type <- if (!is.null(filter)) filter$type else NULL
    build_concept_table_with_filter(concept_type = "community", vb_code = vb_code, filter_type = filter_type)
  })

  output$proj_table <- DT::renderDataTable({
    build_project_table()
  })

  output$party_table <- DT::renderDataTable({
    build_party_table()
  })

  output$plant_table <- DT::renderDataTable({
    build_plant_table()
  })

  output$map <- leaflet::renderLeaflet({
    # Wait for map data to be available (fetched when Map tab is visited)
    shiny::req(map_observations())

    # Use the current map state (already restored from URL by the URL observer) so that
    # direct navigation with map_lat/map_lng/map_zoom params renders the correct initial view.
    # isolate() prevents creating a reactive dependency that would re-render the whole map.
    center_lat <- shiny::isolate(state$map_center_lat())
    center_lng <- shiny::isolate(state$map_center_lng())
    zoom <- shiny::isolate(state$map_zoom())

    map <- process_map_data(
      map_data = map_observations(),
      center_lng = center_lng,
      center_lat = center_lat,
      zoom = zoom
    )

    # Add callback to hide loading screen once map is fully rendered
    # Use isolate() to prevent creating a reactive dependency on map_initialized()
    if (!isTRUE(shiny::isolate(map_initialized()))) {
      map <- htmlwidgets::onRender(map, "
        function(el, x) {
          var map = this;
          var signaled = false;

          function signalMapReady() {
            if (!signaled) {
              signaled = true;
              Shiny.setInputValue('map_ready', true, {priority: 'event'});
            }
          }

          // Check if all visible tile images have full opacity
          function allTilesFullOpacity() {
            var tiles = el.querySelectorAll('.leaflet-tile-loaded');
            if (tiles.length === 0) return false;

            for (var i = 0; i < tiles.length; i++) {
              var style = window.getComputedStyle(tiles[i]);
              var opacity = parseFloat(style.opacity);
              if (opacity < 0.99) {
                return false;
              }
            }
            return true;
          }

          // Check if map container is NOT recalculating (Shiny output state)
          function isNotRecalculating() {
            return !el.classList.contains('recalculating');
          }

          // Check if clusters are rendered
          function hasClusters() {
            var clusters = el.querySelectorAll('.marker-cluster');
            var markers = el.querySelectorAll('.leaflet-marker-icon');
            return clusters.length > 0 || markers.length > 0;
          }

          // Main readiness check - polls until all conditions are met
          function checkFullyReady() {
            if (signaled) return;

            var notRecalculating = isNotRecalculating();
            var tilesReady = allTilesFullOpacity();
            var clustersPresent = hasClusters();

            if (notRecalculating && tilesReady && clustersPresent) {
              signalMapReady();
            } else {
              // Keep checking every 50ms
              setTimeout(checkFullyReady, 50);
            }
          }

          // Start checking after initial render
          map.whenReady(function() {
            // Give Leaflet a moment to start tile loading
            setTimeout(checkFullyReady, 100);
          });

          // Fallback timeout
          setTimeout(function() {
            if (!signaled) {
              signalMapReady();
            }
          }, 15000);
        }
      ")
    }

    map
  })


  # EVENT OBSERVERS --------------------------------------------------------------------------------

  # Handle tab-specific initialization on navigation:
  #   - Overview: lazy-initialize overview data on first visit
  #   - Map: fetch plot observation data on first visit
  # Uses per-tab flags to prevent duplicate work across repeated navigations.
  shiny::observeEvent(state$current_tab(),
    {
      tab <- state$current_tab()

      # Lazy-initialize Overview content on first visit
      if (identical(tab, "Overview") && !overview_initialized()) {
        session$sendCustomMessage("showLoadingOverlay", list(type = "overview"))
        init_overview(output, state, session)
        overview_initialized(TRUE)
      }

      # Only fetch map data when switching to Map tab
      if (!identical(tab, "Map")) {
        return()
      }

      # Already have data? Skip fetch.
      if (!is.null(map_observations())) {
        return()
      }

      # Already fetching? Skip.
      if (isTRUE(map_fetch_in_progress())) {
        return()
      }

      # Check if map has been initialized before
      if (!isTRUE(map_initialized())) {
        # First time loading - show loading screen and lock nav
        session$sendCustomMessage("showLoadingOverlay", list(type = "map"))
        session$sendCustomMessage("setNavInteractivity", list(disabled = TRUE))
      }

      # Fetch data
      map_fetch_in_progress(TRUE)
      observations <- do_map_fetch_with_feedback()
      map_fetch_in_progress(FALSE)

      if (!is.null(observations)) {
        map_observations(observations)
        # Note: map_initialized flag and loading screen will be hidden
        # by the onRender callback in output$map after map fully renders
      } else {
        # Loading screen and nav interactivity are reset regardless of error type;
        # do_map_fetch_with_feedback() already surfaced the right notification.
        session$sendCustomMessage("setNavInteractivity", list(disabled = FALSE))
        session$sendCustomMessage("hideLoadingOverlay", list(type = "map"))
      }
    },
    ignoreInit = TRUE
  )

  # Enable/disable DT download button based on filtered record count
  # Button is only enabled when records <= 20,000 (DOWNLOAD_MAX_RECORDS)
  shiny::observe({
    # React to table ready signal (fires after table init/re-render)
    input$plot_table_ready

    # Get the filtered record count from the DataTable AJAX response
    filtered_count <- input$plot_table_filtered_count

    # Enable button only if we have a count and it's within the limit
    # Initial state (NULL count) keeps button disabled
    can_download <- !is.null(filtered_count) &&
      is.numeric(filtered_count) &&
      filtered_count > 0 &&
      filtered_count <= DOWNLOAD_MAX_RECORDS

    # Enable/disable via custom message handler
    session$sendCustomMessage("setDownloadButtonState", list(enabled = can_download))
  })

  # When DT button is clicked, trigger the download
  shiny::observeEvent(input$plot_download_trigger, {
    # Client-side validation will show loading overlay after checks pass
    session$sendCustomMessage("triggerDownload", list())
  })

  # Hide loading screen when map is fully rendered and ready
  shiny::observeEvent(input$map_ready,
    {
      if (!isTRUE(map_initialized())) {
        map_initialized(TRUE)
        session$sendCustomMessage("hideLoadingOverlay", list(type = "map"))
        session$sendCustomMessage("setNavInteractivity", list(disabled = FALSE))

        # Show filter notice if the user had a filter active when they first
        # navigated to the Map tab (hidden behind the loading overlay until now)
        maybe_show_map_filter_notice()
      }
    },
    ignoreInit = TRUE
  )

  # When returning to an already-loaded map with an active Plots filter, show
  # the notice once so the user isn't confused by the unfiltered marker set.
  shiny::observeEvent(state$current_tab(),
    {
      if (!identical(state$current_tab(), "Map")) return()
      if (!isTRUE(map_initialized())) return()
      maybe_show_map_filter_notice()
    },
    ignoreInit = TRUE
  )

  # Handle search-by-code from the map search control
  shiny::observeEvent(input$map_search_query,
    {
      query <- trimws(input$map_search_query$query)
      if (is.null(query) || !nzchar(query)) return()

      obs <- map_observations()
      if (is.null(obs) || nrow(obs) == 0) {
        session$sendCustomMessage("map_search_results", list(status = "no_data"))
        return()
      }

      # Case-insensitive match on ob_code (vb_code) or author_obs_code
      query_lower <- tolower(query)
      matches <- obs[
        tolower(obs$ob_code) == query_lower |
        tolower(obs$author_obs_code) == query_lower, ,
        drop = FALSE
      ]
      matches <- matches[!is.na(matches$latitude) & !is.na(matches$longitude), , drop = FALSE]

      if (nrow(matches) == 0) {
        session$sendCustomMessage("map_search_results", list(status = "none"))
        return()
      }

      if (nrow(matches) == 1) {
        session$sendCustomMessage("map_search_results", list(
          status = "single",
          lat = matches$latitude[1],
          lng = matches$longitude[1],
          popup_label = build_plot_popup_label(matches$author_obs_code[1], matches$ob_code[1])
        ))
        return()
      }

      # Multiple matches — send disambiguation list (cap at 50 for sanity)
      matches <- utils::head(matches, 50)
      match_list <- lapply(seq_len(nrow(matches)), function(i) {
        list(
          lat = matches$latitude[i],
          lng = matches$longitude[i],
          author_obs_code = matches$author_obs_code[i],
          ob_code = matches$ob_code[i],
          popup_label = build_plot_popup_label(matches$author_obs_code[i], matches$ob_code[i])
        )
      })
      session$sendCustomMessage("map_search_results", list(
        status = "multiple",
        matches = match_list
      ))
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # Track map state changes without triggering redraws
  shiny::observeEvent(input$map_zoom,
    {
      zoom <- suppressWarnings(as.numeric(input$map_zoom))
      if (is.null(zoom) || is.na(zoom)) {
        return()
      }

      previous_zoom <- state$map_zoom()
      changed <- !url_manager$nearly_equal(previous_zoom, zoom, tol = 1e-4)

      state$map_zoom(zoom)
      update_map_custom_flag()

      if (changed && can_mutate_history()) {
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
      changed <- !url_manager$nearly_equal(previous_lat, lat, tol = 1e-5) || !url_manager$nearly_equal(previous_lng, lng, tol = 1e-5)

      state$map_center_lat(lat)
      state$map_center_lng(lng)
      update_map_custom_flag()

      if (changed && can_mutate_history()) {
        update_app_query(mode = "replace")
      }
    },
    ignoreInit = TRUE
  )

  # Handle tab navigation: update state, restore table state, invalidate map, update URL
  # This observer handles user-initiated tab changes (navbar clicks).
  # It does NOT call updateNavbarPage() - the URL observer handles that to prevent feedback loops.
  # ignoreInit = TRUE prevents firing on the initial navbar selection ("Overview"),
  # which would race with citation resolution and URL-driven initialization.
  shiny::observeEvent(input$page,
    {
      if (is.null(input$page) || !nzchar(input$page)) {
        return()
      }

      # Early exit if we're in the middle of URL restoration to prevent feedback loops
      # The URL observer will handle navbar updates during restoration
      if (url_manager$is_updating()) {
        return()
      }

      # Skip if the tab is already current (e.g., after citation resolution already
      # set the tab via state$current_tab() and updateNavbarPage()). This prevents
      # duplicate URL pushes and redundant state updates.
      if (identical(state$current_tab(), input$page)) {
        return()
      }

      state$current_tab(input$page)

      # Invalidate map size when navigating to Map tab to ensure proper rendering
      # Map forgets its size when hidden and needs recalculation
      # See https://github.com/NCEAS/vegbank-web/issues/28
      if (identical(input$page, "Map")) {
        session$sendCustomMessage("invalidateMapSize", list())
      }

      # When switching tabs, apply any stored table state for the new tab's table
      new_table_key <- url_manager$get_table_key(input$page)
      if (!is.null(new_table_key)) {
        stored_state <- shiny::isolate(state$table_states[[new_table_key]])
        if (!is.null(stored_state)) {
          state$table_sync_pending[[new_table_key]] <- TRUE
          send_table_state_to_client(new_table_key, stored_state)
        }
      }

      # Defer history mutations until the initial URL restoration has completed
      if (!can_mutate_history()) {
        return()
      }

      update_app_query(
        mode = "push",
        tab = input$page,
        detail_type = if (isTRUE(state$details_open())) state$detail_type() else NULL,
        detail_code = if (isTRUE(state$details_open())) state$detail_code() else NULL
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$row_highlight,
    {
      payload <- input$row_highlight
      table_id <- payload$tableId %||% NULL
      row_index <- payload$rowIndex %||% NULL

      state$highlighted_table(table_id)

      if (is.null(row_index) || is.na(row_index)) {
        state$highlighted_row(NULL)
      } else {
        state$highlighted_row(as.integer(row_index))
      }
    },
    ignoreNULL = TRUE
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

  shiny::observeEvent(input$show_on_map,
    {
      map_data <- input$show_on_map
      lat <- as.numeric(map_data$lat)
      lng <- as.numeric(map_data$lng)
      code <- map_data$code
      ob_code <- map_data$ob_code

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

      state$map_request(list(lat = lat, lng = lng, code = code, ob_code = ob_code, zoom = target_zoom))

      state$current_tab("Map")

      mode <- if (url_manager$is_history_initialized()) "push" else "replace"
      update_app_query(
        mode = mode,
        tab = "Map",
        detail_type = if (isTRUE(state$details_open())) state$detail_type() else NULL,
        detail_code = if (isTRUE(state$details_open())) state$detail_code() else NULL
      )
      shiny::updateNavbarPage(session, "page", selected = "Map")

      map_update_observer <- shiny::observe({
        shiny::req(input$page == "Map")

        map_req <- state$map_request()
        if (!is.null(map_req)) {
          # Invalidating the map size is necessary when navigating to Map tab because the
          # map fogrets its size when hidden and needs to be recalculated to render properly.
          # See https://github.com/NCEAS/vegbank-web/issues/28
          session$sendCustomMessage("invalidateMapSize", list())
          label <- build_plot_popup_label(map_req$code, map_req$ob_code)
          move_map_to_obs(
            session,
            map_req$lat,
            map_req$lng,
            label
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

  shiny::observeEvent(input$plot_link_click, {
    vb_code <- input$plot_link_click
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

  shiny::observeEvent(input$cover_method_link_click, {
    vb_code <- input$cover_method_link_click
    open_detail("cover-method", vb_code)
  })

  shiny::observeEvent(input$stratum_method_link_click, {
    vb_code <- input$stratum_method_link_click
    open_detail("stratum-method", vb_code)
  })

  shiny::observeEvent(input$to_link_click, {
    vb_code <- input$to_link_click
    if (!is.null(vb_code) && nchar(vb_code) > 0) {
      open_detail("taxon-observation", vb_code)
    }
  })

  # URL STATE SYNCHRONIZATION OBSERVER ----------------------------------------------------------------

  # This observer runs whenever the browser URL changes (back/forward, refresh, direct link).
  # It parses query parameters and restores tab, detail overlay, map view, and table state.
  # The URL manager's updating flag prevents circular updates when we programmatically change the URL.
  shiny::observeEvent(current_query(),
    {
      params <- current_query()

      url_manager$set_updating(TRUE)
      on.exit(
        {
          url_manager$set_updating(FALSE)
          if (!url_manager$is_history_initialized()) {
            session$sendCustomMessage("setNavInteractivity", list(disabled = FALSE))
            url_manager$set_history_initialized(TRUE)
          }
        },
        add = TRUE
      )

      # Handle citation resolution: ?cite=IDENTIFIER
      # /cite/ paths are HTTP-302-redirected to /?cite= by the UI function.
      # Here we resolve the identifier via the vegbankr API, apply the appropriate
      # navigation state, and replace the URL with resolved parameters.
      # The return() ensures no other URL params from the ?cite= query are processed.
      cite_param <- url_manager$first_param(params$cite)
      if (url_manager$is_valid_param(cite_param)) {
        # Strip surrounding quotes that may appear from URL encoding (%22)
        cite_param <- gsub('^["\']|["\']$', "", cite_param)
        if (nzchar(cite_param)) {
          resolve_and_redirect_citation(cite_param)
          return()
        }
      }

      # Parse and apply requested tab
      requested_tab <- url_manager$first_param(params$tab)
      if (!url_manager$is_valid_param(requested_tab)) {
        requested_tab <- "Home"
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

      detail_valid <- url_manager$is_valid_param(params$detail) && url_manager$is_valid_param(params$code)

      if (detail_valid) {
        target_type <- url_manager$first_param(params$detail)
        target_code <- url_manager$first_param(params$code)

        # Restore highlight state from URL
        hl_table <- url_manager$first_param(params$hl_table)
        hl_row <- url_manager$parse_numeric_param(params$hl_row)
        if (!is.null(hl_table) && nzchar(hl_table)) {
          state$highlighted_table(hl_table)
        }
        if (!is.null(hl_row) && !is.na(hl_row)) {
          state$highlighted_row(as.integer(hl_row))
        }

        needs_open <- !identical(state$detail_type(), target_type) ||
          !identical(state$detail_code(), target_code) ||
          !isTRUE(state$details_open())

        if (needs_open) {
          # Skip server-side highlight; client handles it from URL params after table loads
          open_detail(target_type, target_code,
            push_history = FALSE,
            skip_highlight = TRUE
          )
        }
      } else if (isTRUE(state$details_open())) {
        close_detail(push_history = FALSE, hide_overlay = TRUE)
      } else if (!is.null(params$detail) || !is.null(params$code)) {
        if (url_manager$is_history_initialized()) {
          update_app_query(
            mode = "replace",
            tab = requested_tab,
            detail_type = NULL,
            detail_code = NULL
          )
        }
      }

      # Parse and apply plot filter state
      plot_filter_code <- url_manager$first_param(params$plot_filter_code)
      plot_filter_type <- url_manager$first_param(params$plot_filter_type)
      plot_filter_label <- url_manager$first_param(params$plot_filter_label)

      if (url_manager$is_valid_param(plot_filter_code) && url_manager$is_valid_param(plot_filter_type)) {
        # Restore filter from URL
        current_filter <- state$plot_filter()
        new_filter <- list(
          type = plot_filter_type,
          code = plot_filter_code,
          label = if (url_manager$is_valid_param(plot_filter_label)) plot_filter_label else plot_filter_code
        )

        # Only update if different from current state
        if (is.null(current_filter) ||
          !identical(current_filter$code, new_filter$code) ||
          !identical(current_filter$type, new_filter$type)) {
          state$plot_filter(new_filter)
        }
      } else if (!is.null(params$plot_filter_code) || !is.null(params$plot_filter_type)) {
        # Invalid filter params in URL, clear them
        state$plot_filter(NULL)
        if (url_manager$is_history_initialized()) {
          update_app_query(mode = "replace", tab = requested_tab)
        }
      } else {
        # No filter params in URL, clear filter state
        if (!is.null(state$plot_filter())) {
          state$plot_filter(NULL)
        }
      }

      # Parse and apply community filter state
      comm_filter_code <- url_manager$first_param(params$comm_filter_code)
      comm_filter_type <- url_manager$first_param(params$comm_filter_type)
      comm_filter_label <- url_manager$first_param(params$comm_filter_label)

      if (url_manager$is_valid_param(comm_filter_code) && url_manager$is_valid_param(comm_filter_type)) {
        # Restore community filter from URL
        current_comm_filter <- state$community_filter()
        new_comm_filter <- list(
          type = comm_filter_type,
          code = comm_filter_code,
          label = if (url_manager$is_valid_param(comm_filter_label)) comm_filter_label else comm_filter_code
        )

        # Only update if different from current state
        if (is.null(current_comm_filter) ||
          !identical(current_comm_filter$code, new_comm_filter$code) ||
          !identical(current_comm_filter$type, new_comm_filter$type)) {
          state$community_filter(new_comm_filter)
        }
      } else if (!is.null(params$comm_filter_code) || !is.null(params$comm_filter_type)) {
        # Invalid community filter params in URL, clear them
        state$community_filter(NULL)
        if (url_manager$is_history_initialized()) {
          update_app_query(mode = "replace", tab = requested_tab)
        }
      } else {
        # No community filter params in URL, clear filter state
        if (!is.null(state$community_filter())) {
          state$community_filter(NULL)
        }
      }

      # Parse and apply map view state
      map_lat_param <- params$map_lat
      map_lng_param <- params$map_lng
      map_zoom_param <- params$map_zoom

      map_lat <- url_manager$parse_numeric_param(map_lat_param)
      map_lng <- url_manager$parse_numeric_param(map_lng_param)
      map_zoom <- url_manager$parse_numeric_param(map_zoom_param)

      map_params_present <- !is.null(map_lat_param) || !is.null(map_lng_param) || !is.null(map_zoom_param)
      if (!is.null(map_lat) && !is.null(map_lng)) {
        if (!url_manager$nearly_equal(state$map_center_lat(), map_lat, tol = 1e-5)) {
          state$map_center_lat(map_lat)
        }
        if (!url_manager$nearly_equal(state$map_center_lng(), map_lng, tol = 1e-5)) {
          state$map_center_lng(map_lng)
        }

        if (!is.null(map_zoom) && !url_manager$nearly_equal(state$map_zoom(), map_zoom, tol = 1e-4)) {
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

        if (url_manager$is_history_initialized()) {
          update_app_query(
            mode = "replace",
            tab = requested_tab,
            detail_type = if (detail_valid) target_type else NULL,
            detail_code = if (detail_valid) target_code else NULL
          )
        }
      } else if (identical(requested_tab, "Map")) {
        if (!map_params_present && !url_manager$is_map_default(state$map_center_lat(), state$map_center_lng(), state$map_zoom())) {
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

      # Parse and apply table state for ALL tables (not just current tab)
      # This ensures that when user navigates to another tab, that table's
      # pagination state is already restored from URL
      for (table_key in names(table_registry)) {
        table_state_from_query <- url_manager$extract_table_state_from_query(table_key, params)

        if (is.null(table_state_from_query) || url_manager$is_default_table_state(table_key, table_state_from_query)) {
          state$table_states[[table_key]] <- NULL
          state$table_sync_pending[[table_key]] <- FALSE
        } else {
          state$table_states[[table_key]] <- table_state_from_query
          # Only set sync pending and send to client for the CURRENT tab's table
          # Other tables will be synced when user navigates to them
          current_tab_key <- url_manager$get_table_key(requested_tab)
          if (!is.null(current_tab_key) && identical(table_key, current_tab_key)) {
            state$table_sync_pending[[table_key]] <- TRUE
            send_table_state_to_client(table_key, table_state_from_query)
          }
        }
      }

      # Clean up invalid table params from URL (for current tab only)
      current_table_key <- url_manager$get_table_key(requested_tab)
      if (!is.null(current_table_key)) {
        current_table_params_present <- any(vapply(
          c("_start", "_length", "_order", "_search"),
          function(suffix) {
            !is.null(params[[paste0(current_table_key, suffix)]])
          },
          logical(1)
        ))
        current_table_state <- state$table_states[[current_table_key]]

        if (current_table_params_present && is.null(current_table_state)) {
          # Current tab had invalid table params, clean them up
          if (url_manager$is_history_initialized()) {
            update_app_query(
              mode = "replace",
              tab = requested_tab,
              detail_type = if (detail_valid) target_type else NULL,
              detail_code = if (detail_valid) target_code else NULL
            )
          }
        }
      }

      # Ensure tab parameter is present in URL
      if (!url_manager$is_valid_param(params$tab)) {
        if (url_manager$is_history_initialized()) {
          update_app_query(
            mode = "replace",
            tab = requested_tab,
            detail_type = if (detail_valid) target_type else NULL,
            detail_code = if (detail_valid) target_code else NULL
          )
        }
      }
    },
    ignoreNULL = FALSE
  )

  # CROSS-RESOURCE FILTER OBSERVER ----------------------------------------------------------------

  # Generic observer for all obs_count clicks - extracts entity type from vb_code prefix
  shiny::observeEvent(input$obs_count_click, {
    event_data <- input$obs_count_click
    if (is.null(event_data)) {
      return()
    }

    # Extract code and label from the click event
    # The JS handler sends a list with $code and $label for obs_count links
    if (is.list(event_data)) {
      vb_code <- event_data$code
      label <- event_data$label
    } else {
      # Fallback for simple string value
      vb_code <- event_data
      label <- event_data
    }

    if (is.null(vb_code) || is.na(vb_code) || !nzchar(vb_code)) {
      return()
    }

    # Extract entity type from vb_code prefix
    # VegBank codes follow pattern: prefix.id (e.g., pj.340, py.123, pc.456)
    entity_type <- convert_code_to_singular(vb_code)
    if (is.null(entity_type)) {
      warning("Could not determine entity type from vb_code: ", vb_code)
      return()
    }

    # Set filter state
    state$plot_filter(list(
      type = entity_type,
      code = vb_code,
      label = label %||% vb_code
    ))

    # Clear plots table state (including search) when applying cross-resource filter
    # This prevents old search terms from being carried over to the filtered view
    state$table_states[["plots"]] <- NULL

    # Navigate to Plots tab and update URL
    state$current_tab("Plots")
    shiny::updateNavbarPage(session, "page", selected = "Plots")
    update_app_query(mode = "push", tab = "Plots")
  })

  shiny::observeEvent(input$clear_plot_filter, {
    state$plot_filter(NULL)
    # Clear plots table state (including search) when clearing filter
    # This ensures we start fresh without any stale search terms
    state$table_states[["plots"]] <- NULL
    # Update URL to remove filter parameters
    update_app_query(mode = "push", tab = state$current_tab())
  })

  shiny::observeEvent(input$clear_comm_filter, {
    state$community_filter(NULL)
    # Clear communities table state when clearing filter
    state$table_states[["communities"]] <- NULL
    # Update URL to remove filter parameters
    update_app_query(mode = "push", tab = state$current_tab())
  })
}

# ================= CITATION RESOLUTION ============================================================
# Note: RESOURCE_REGISTRY and helper functions are defined in R/resource_registry.R

#' Resolve citation identifier to app navigation parameters
#'
#' Calls vegbankr::vb_resolve() to look up the resource type and VegBank code
#' for a legacy identifier, then maps the result to app navigation targets
#' using RESOURCE_REGISTRY.
#'
#' @param identifier Character string, the accession code (or vb_code or DOI) to resolve
#'   (e.g., "VB.Ob.2948.ACAD143")
#' @return A list with components:
#'   \describe{
#'     \item{vb_code}{The resolved VegBank code (e.g., "ob.2948")}
#'     \item{tab}{The app tab to navigate to (e.g., "Plots")}
#'     \item{detail_type}{The detail view type (e.g., "plot-observation")}
#'     \item{resource_info}{Full resource metadata from RESOURCE_REGISTRY}
#'     \item{identifier}{The original identifier (for labeling)}
#'   }
#'   Returns NULL if resolution fails or the resource type is unsupported.
#' @noRd
resolve_citation <- function(identifier) {
  result <- tryCatch(
    vegbankr::vb_resolve(identifier),
    error = function(e) {
      warning("Citation resolution failed for '", identifier, "': ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result) || is.null(result$vb_resource_type) || is.null(result$vb_code)) {
    return(NULL)
  }

  resource_info <- get_resource_by_api_type(result$vb_resource_type)
  if (is.null(resource_info)) {
    return(NULL)
  }

  list(
    vb_code = result$vb_code,
    tab = resource_info$tab,
    detail_type = resource_info$detail_type,
    resource_info = resource_info, # Include full resource info for display names
    identifier = identifier
  )
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
#' @param skip_highlight If TRUE, skip sending row highlight message (client handles it)
#' @return TRUE if successful, FALSE if validation failed
#' @noRd
open_code_details <- function(
    state, session, output, vb_code, detail_type, error_message = NULL,
    skip_highlight = FALSE) {
  if (!is_valid_vb_code(vb_code)) {
    error_msg <- error_message %||% paste0("No VegBank code found for that ", gsub("-", " ", detail_type))
    shiny::showNotification(error_msg, type = "error", duration = NULL)
    return(FALSE)
  }

  state$detail_type(detail_type)
  state$detail_code(vb_code)
  state$details_open(TRUE)

  # Skip row highlight if client will handle it (e.g., during URL restore)
  if (!isTRUE(skip_highlight)) {
    highlight_table_row(session, state$highlighted_table(), state$highlighted_row())
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

#' Fetch map data and notify the user on failure
#'
#' Wraps `fetch_plot_map_data()` in error handling so that both the Shiny
#' notification and the loading-screen teardown always happen in the same
#' call site that manages the loading overlay. Returns the data frame on
#' success, or NULL on any failure (after showing the appropriate notification).
#'
#' @return Data frame of plot observations, or NULL
#' @noRd
do_map_fetch_with_feedback <- function() {
  tryCatch(
    {
      obs <- fetch_plot_map_data()
      if (is.null(obs)) {
        shiny::showNotification(
          "Map data is currently unavailable. Please try again later.",
          type = "warning",
          duration = NULL
        )
      }
      obs
    },
    error = function(err) {
      shiny::showNotification(
        paste("Failed to load map data:", conditionMessage(err)),
        type = "error",
        duration = NULL
      )
      NULL
    }
  )
}

#' Build the popup label for a plot observation
#'
#' Produces the human-readable string shown in the leaflet popup when the map
#' flies to a plot. The JavaScript mirror of this function is `buildPopupNode()`
#' in `vegbank_app.js`.
#'
#' @param code Author observation code (author_obs_code)
#' @param ob_code VegBank observation code (ob_code), or NULL / empty string
#' @return A character string
#' @noRd
build_plot_popup_label <- function(code, ob_code = NULL) {
  if (!is.null(ob_code) && nzchar(ob_code %||% "")) {
    paste0("Plot ", code, " (", ob_code, ") is here!")
  } else {
    paste("Plot", code, "is here!")
  }
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

#' Highlight a table row using a custom Shiny message
#'
#' @param session The Shiny session object
#' @param table_id The table ID to target for highlighting
#' @param row_index Zero-based row index inside the targeted table
#' @noRd
highlight_table_row <- function(session, table_id, row_index) {
  # Skip if no table or row specified
  if (is.null(table_id) || is.null(row_index)) {
    return()
  }

  message_payload <- list(
    tableId = table_id,
    rowIndex = row_index
  )

  # Defer sending until after the current flush cycle to ensure client is ready
  session$onFlushed(function() {
    session$sendCustomMessage("highlightTableRow", message_payload)
  }, once = TRUE)
}
