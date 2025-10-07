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
    selected_accession = shiny::reactiveVal(NULL),
    details_open = shiny::reactiveVal(FALSE),
    map_center_lat = map_state$map_center_lat,
    map_center_lng = map_state$map_center_lng,
    map_zoom = map_state$map_zoom
  )


  # DATA LOADING -----------------------------------------------------------------------------------

  vegbankr::vb_debug()
  vegbankr::set_vb_base_url("https://api-dev.vegbank.org")

  shiny::withProgress(message = "Fetching data:", value = 0, {
    plot_data <- load_data_type(
      "plot observations",
      "inst/cached_data/plot_obs_minimal_all.RDS",
      vegbankr::get_all_plot_observations,
      list(detail = "minimal")
    )

    comm_class_data <- load_data_type(
      "community classifications",
      "inst/cached_data/comm_class_minimal_all.RDS",
      vegbankr::get_all_community_classifications
    )

    comm_concept_data <- load_data_type(
      "community concepts",
      "inst/cached_data/comm_concept_full_all.RDS",
      vegbankr::get_all_community_concepts
    )

    taxa_data <- load_data_type(
      "taxon observations",
      "inst/cached_data/taxon_obs_top_5.RDS",
      vegbankr::get_all_taxon_observations
    )

    plant_data <- load_data_type(
      "plant concepts",
      "inst/cached_data/plant_concepts_all_20250924.RDS",
      vegbankr::get_all_plant_concepts
    )

    project_data <- load_data_type(
      "projects",
      "inst/cached_data/projects_all.RDS",
      vegbankr::get_all_projects
    )

    party_data <- load_data_type(
      "parties",
      "inst/cached_data/parties_all.RDS",
      vegbankr::get_all_parties
    )
  })

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

  shiny::observeEvent(input$see_obs_details, {
    accession_code <- input$see_obs_details
    open_accession_details(
      state,
      session,
      output,
      accession_code,
      "plot-observation"
    )
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
    state$details_open(FALSE)
    session$sendCustomMessage("clearAllTableSelections", list())
    session$doBookmark()
  })

  shiny::observeEvent(input$label_link_click, {
    accession_code <- input$label_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      open_accession_details(state, session, output, accession_code, "plot-observation")
    }
  })

  shiny::observeEvent(input$comm_class_link_click, {
    accession_code <- input$comm_class_link_click
    open_accession_details(
      state, session, output,
      accession_code, "community-classification",
      NULL, comm_class_data, taxa_data, plot_data
    )
  })

  shiny::observeEvent(input$comm_link_click, {
    accession_code <- input$comm_link_click
    open_accession_details(state, session, output, accession_code, "community-concept")
  })

  shiny::observeEvent(input$taxa_link_click, {
    accession_code <- input$taxa_link_click
    open_accession_details(
      state, session, output,
      accession_code, "taxon-observation",
      NULL, comm_class_data, taxa_data, plot_data
    )
  })

  shiny::observeEvent(input$proj_link_click, {
    accession_code <- input$proj_link_click
    open_accession_details(state, session, output, accession_code, "project")
  })

  shiny::observeEvent(input$party_link_click, {
    accession_code <- input$party_link_click
    open_accession_details(state, session, output, accession_code, "party")
  })

  shiny::observeEvent(input$plant_link_click, {
    accession_code <- input$plant_link_click
    open_accession_details(state, session, output, accession_code, "plant-concept")
  })

  shiny::observeEvent(input$stratum_method_link_click, {
    accession_code <- input$stratum_method_link_click
    open_accession_details(state, session, output, accession_code, "stratum-method")
  })


  # STATE PERSISTENCE ----------------------------------------------------------------------------------
  shiny::onBookmark(function(state_obj) {
    state_obj$values$current_tab <- input$page
    state_obj$values$detail_type <- state$detail_type()
    state_obj$values$selected_accession <- state$selected_accession()
    state_obj$values$details_open <- state$details_open()
    state_obj$values$map_center_lat <- state$map_center_lat()
    state_obj$values$map_center_lng <- state$map_center_lng()
    state_obj$values$map_zoom <- state$map_zoom()

    state_obj
  })

  shiny::onBookmarked(function(url) {
    shiny::updateQueryString(url)
  })

  shiny::onRestore(function(context) {
    # Restore the tab
    if (!is.null(context$values$current_tab)) {
      shiny::updateNavbarPage(session, "page", selected = context$values$current_tab)
    }

    # Restore map state
    if (!is.null(context$values$map_center_lat)) {
      state$map_center_lat(context$values$map_center_lat)
    }
    if (!is.null(context$values$map_center_lng)) {
      state$map_center_lng(context$values$map_center_lng)
    }
    if (!is.null(context$values$map_zoom)) {
      state$map_zoom(context$values$map_zoom)
    }

    # Use a self-destroying observer to handle map initialization from URL
    # This observer is in the onRestore function to ensure it runs after state
    # restoration and is only created when necessary instead of on every session
    map_init_observer <- shiny::observeEvent(session$clientData$url_search,
      {
        # Only update map if we have restored state values
        if (state$map_center_lat() != DEFAULT_MAP_LAT ||
          state$map_center_lng() != DEFAULT_MAP_LNG ||
          state$map_zoom() != DEFAULT_MAP_ZOOM) {
          leaflet::leafletProxy("map", session) |>
            leaflet::setView(
              lng = state$map_center_lng(),
              lat = state$map_center_lat(),
              zoom = state$map_zoom()
            )
        }

        # Destroy this observer after first run
        map_init_observer$destroy()
      },
      once = TRUE
    )

    # Safely reopen the detail overlay
    if (isTRUE(context$values$details_open)) {
      detail_open_observer <- shiny::observe({
        state$detail_type(context$values$detail_type)
        state$selected_accession(context$values$selected_accession)
        state$details_open(TRUE)

        # Show the detail view first
        show_detail_view(
          state$detail_type(),
          state$selected_accession(),
          output,
          session
        )

        # Then select the row - the JavaScript will handle timing via DataTable events
        accession_code <- state$selected_accession()
        detail_type <- state$detail_type()

        message("DEBUG onRestore: Restoring detail view - ", detail_type, ": ", accession_code)

        # Look up the correct row code in case of indirect links (comm class and taxon obs)
        if (!is.null(accession_code) && !is.null(detail_type)) {
          row_code <- find_row_selection_code(
            detail_type, accession_code, comm_class_data, taxa_data, plot_data
          )
          if (!is.null(row_code)) {
            select_table_row_by_accession(session, row_code)
          }
        }

        detail_open_observer$destroy()
      })
    }

    invisible(NULL)
  })

  shiny::observe({
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })

  shiny::setBookmarkExclude(generate_bookmark_exclusions())
}


# ================= OTHER FUNCTIONS ===========================================================

#' Handles opening accession detail views with consistent state management
#'
#' @param state The state object containing reactive values
#' @param session Shiny session object
#' @param output Shiny output object
#' @param accession_code The accession code for the entity
#' @param detail_type The type of detail view to open
#' @param error_message Custom error message for invalid accession codes
#' @return TRUE if successful, FALSE if validation failed
#' @noRd
open_accession_details <- function(
    state, session, output, accession_code, detail_type, error_message = NULL,
    comm_class_data = NULL, taxa_data = NULL, plot_data = NULL) {
  if (!is_valid_accession_code(accession_code)) {
    error_msg <- error_message %||% paste0("No accession code found for that ", gsub("-", " ", detail_type))
    shiny::showNotification(error_msg, type = "error")
    return(FALSE)
  }

  state$detail_type(detail_type)
  state$selected_accession(accession_code)
  state$details_open(TRUE)

  # Use a helper function to find the correct table selection code in case of indirect links
  # Community classifications and taxon observations require looking up the related
  # plot observation to select the correct row.
  if (!is.null(comm_class_data) && !is.null(taxa_data) && !is.null(plot_data)) {
    row_code <- find_row_selection_code(detail_type, accession_code, comm_class_data, taxa_data, plot_data)
    if (!is.null(row_code)) {
      select_table_row_by_accession(session, row_code)
    }
  } else {
    # Fallback to direct selection for simple cases
    select_table_row_by_accession(session, accession_code)
  }

  show_detail_view(detail_type, accession_code, output, session)

  return(TRUE)
}

#' Validates an accession code for use in the application
#'
#' @param accession_code The accession code to validate
#' @return TRUE if valid, FALSE if invalid
#' @noRd
is_valid_accession_code <- function(accession_code) {
  !is.null(accession_code) && !is.na(accession_code) &&
    nchar(as.character(accession_code)) > 0 && accession_code != "NA"
}

#' Generates a character vector of bookmark exclusions for all tables defined
#' in dt_output_ids and the map so the bookmark URL doesn't become too long.
#'
#' @noRd
generate_bookmark_exclusions <- function() {
  dt_output_ids <- c("plot_table", "comm_table", "proj_table", "party_table", "plant_table")

  # Base exclusions to apply to all DataTables
  table_exclusions <- c(
    "_cells_selected", "_cell_clicked", "_columns_selected", "_row_last_clicked",
    "_rows_all", "_rows_current", "_rows_selected", "_search", "_state"
  )

  # Generate exclusions for each table
  all_table_exclusions <- unlist(lapply(dt_output_ids, function(table_id) {
    paste0(table_id, table_exclusions)
  }))

  # Map-specific exclusions
  map_exclusions <- c(
    "map_bounds", "map_center", "map_click", "map_marker_click",
    "map_marker_mouseout", "map_marker_mouseover", "map_zoom"
  )

  # Combine and return all exclusions
  c(all_table_exclusions, map_exclusions)
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

#' Select a table row by accession code using a custom Shiny message
#'
#' @param session The Shiny session object
#' @param accession_code The accession code to select
#' @noRd
select_table_row_by_accession <- function(session, accession_code) {
  # Skip if invalid accession code
  if (!is_valid_accession_code(accession_code)) {
    return()
  }

  # Send selection message to JavaScript
  session$sendCustomMessage("selectTableRowByAccession", list(
    accessionCode = accession_code
  ))
}

#' Find the correct accession code for table row selection based on detail type
#' Community classifications and taxon observations require looking up the related
#' plot observation to select the correct row.
#'
#' @param detail_type The type of detail view
#' @param accession_code The accession code from the detail view
#' @param comm_class_data Community classification data frame
#' @param taxa_data Taxon observation data frame
#' @param plot_data Plot observation data frame
#' @return The accession code to use for table row selection, or NULL if not found
#' @noRd
find_row_selection_code <- function(detail_type, accession_code, comm_class_data, taxa_data, plot_data) {
  switch(detail_type,
    "plot-observation" = accession_code,
    "community-concept" = accession_code,
    "project" = accession_code,
    "party" = accession_code,
    "plant-concept" = accession_code,
    # TODO: Should there be a stratum/cover method table or should we highlight the plot observation?
    "stratum-method" = accession_code,
    "community-classification" = {
      # Find the plot row that contains this community classification
      comm_class_row <- which(comm_class_data$comm_class_accession_code == accession_code)
      if (length(comm_class_row) > 0) comm_class_data$obs_accession_code[comm_class_row[1]] else NULL
    },
    "taxon-observation" = {
      # Find the plot row that contains this taxon observation
      taxa_row <- which(taxa_data$taxon_observation_accession_code == accession_code)
      if (length(taxa_row) > 0) {
        observation_id <- taxa_data$observation_id[taxa_row[1]]
        plot_row_index <- which(plot_data$observation_id == observation_id)
        if (length(plot_row_index) > 0) plot_data$obs_accession_code[plot_row_index[1]] else NULL
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
