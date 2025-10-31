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

  # Initialize map state with defaults
  map_state <- initialize_map_state()

  state <- list(
    map_request = shiny::reactiveVal(NULL),
    detail_type = shiny::reactiveVal(NULL),
    selected_code = shiny::reactiveVal(NULL),
    details_open = shiny::reactiveVal(FALSE),
    map_center_lat = map_state$map_center_lat,
    map_center_lng = map_state$map_center_lng,
    map_zoom = map_state$map_zoom
  )

  state$current_tab <- shiny::reactiveVal("Overview")

  updating_from_query <- shiny::reactiveVal(FALSE)
  history_initialized <- shiny::reactiveVal(FALSE)

  first_param <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return(NULL)
    }
    value[[1]]
  }

  is_valid_param <- function(value) {
    candidate <- first_param(value)
    !is.null(candidate) && !is.na(candidate) && nzchar(candidate)
  }

  encode_query_value <- function(value) {
    utils::URLencode(as.character(value), reserved = TRUE)
  }

  # Read the browser query string and normalize null/NA cases to empty string
  current_query_string <- function() {
    query <- session$clientData$url_search
    if (is.null(query) || length(query) == 0 || isTRUE(is.na(query)) || identical(query, "")) {
      ""
    } else {
      query
    }
  }

  # Update the browser history to reflect the active tab and detail view
  update_app_query <- function(mode = c("push", "replace"), tab = NULL, detail_type = NULL, detail_code = NULL) {
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

  # Open the requested detail overlay and optionally capture the change in history
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

  # Close the detail overlay and clear browser URL parameters when needed
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

  # Reactive accessor for the current query parameters so observers can react
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

  # Keep the app state in sync when the user navigates via browser history
  shiny::observeEvent(current_query(), {
    params <- current_query()

    updating_from_query(TRUE)

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
  }, ignoreNULL = FALSE)

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
      state$map_zoom(input$map_zoom)
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$map_center,
    {
      if (!is.null(input$map_center)) {
        state$map_center_lat(input$map_center$lat)
        state$map_center_lng(input$map_center$lng)
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

      # Store the map data for the observer
      state$map_request(list(lat = lat, lng = lng, code = code))
      shiny::updateNavbarPage(session, "page", selected = "Map")

      # Create a self-destroying observer (necessary to avoid updating
      # the map before we're on the map page)
      map_update_observer <- shiny::observe({
        # Only proceed if we're on the map page
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
