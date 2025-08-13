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
#' @importFrom DT renderDataTable datatable dataTableProxy selectRows
#' @importFrom htmltools tags
#' @importFrom leaflet renderLeaflet leafletProxy
#' @importFrom utils head
#' @import vegbankr
#'
#' @noRd

# ================= MAIN SERVER FUNCTION =================

server <- function(input, output, session) {
  # STATE MANAGEMENT ______________________________________________________________________________
  state <- list(
    map_request = shiny::reactiveVal(NULL),
    detail_type = shiny::reactiveVal(NULL),
    selected_accession = shiny::reactiveVal(NULL),
    details_open = shiny::reactiveVal(FALSE),
    map_center_lat = shiny::reactiveVal(39.8283), # Default latitude
    map_center_lng = shiny::reactiveVal(-98.5795), # Default longitude
    map_zoom = shiny::reactiveVal(2) # Default zoom
  )

  # Load data from vegbankr
  vegbankr::vb_debug()
  vegbankr::set_vb_base_url("https://api-dev.vegbank.org")

  shiny::withProgress(message = "Fetching data...", value = 0, {
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

  move_map_to_obs <- function(idx) {
    data <- plot_data
    leaflet::leafletProxy("map", session) |>
      update_map_view(
        data$longitude[idx],
        data$latitude[idx],
        paste("Plot", data$author_obs_code[idx], "is here!")
      )
  }

  # RENDER UI ELEMENTS __________________________________________________________________
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

  output$map <- leaflet::renderLeaflet({
    # Always initially render with default values
    process_map_data(plot_data)
  })

  # Use a self-destroying observer to handle map initialization from URL
  map_init_observer <- shiny::observeEvent(session$clientData$url_search,
    {
      # Only update map if we have restored state values
      if (state$map_center_lat() != 39.8283 ||
        state$map_center_lng() != -98.5795 ||
        state$map_zoom() != 2) {
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

  # EVENT HANDLERS _________________________________________________________________________________
  shiny::observeEvent(input$see_details,
    {
      i <- as.numeric(input$see_details)
      selected_row_accession <- plot_data[i, "obs_accession_code"]

      # Check for valid accession code
      if (is.null(selected_row_accession) ||
        is.na(selected_row_accession) ||
        selected_row_accession == "") {
        shiny::showNotification(paste0("No accession code found for row: ", i), type = "error")
        return()
      }

      # TODO: Select the row in the datatable (with error handling)

      state$detail_type("plot-observation")
      state$selected_accession(selected_row_accession)
      state$details_open(TRUE)
      # Open the details view
      show_detail_view("plot-observation", selected_row_accession, output, session)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$show_on_map,
    {
      idx <- as.numeric(input$show_on_map)

      # Check for valid index before proceeding
      if (is.na(idx) || idx < 1 || idx > nrow(plot_data)) {
        shiny::showNotification("Cannot show on map: Missing or invalid index for this row",
          type = "warning"
        )
        return()
      }

      # Check for valid latitude and longitude
      lat <- plot_data$latitude[idx]
      lon <- plot_data$longitude[idx]

      if (is.na(lat) || is.na(lon) || !is.numeric(lat) || !is.numeric(lon)) {
        shiny::showNotification("Cannot show on map: Missing or invalid coordinates for this plot",
          type = "warning"
        )
        return()
      }

      state$map_request(idx)
      shiny::updateNavbarPage(session, "page", selected = "Map")

      # Create a self-destroying observer
      map_update_observer <- shiny::observe({
        # Only proceed if we're on the map page
        shiny::req(input$page == "Map")

        idx <- state$map_request()
        if (!is.null(idx) && length(idx) > 0) {
          session$sendCustomMessage("invalidateMapSize", list())
          move_map_to_obs(idx)
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
    session$doBookmark()
  })

  shiny::observeEvent(input$label_link_click, {
    accession_code <- input$label_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      state$detail_type("plot-observation")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("plot-observation", accession_code, output, session)
    }
  })

  shiny::observeEvent(input$comm_class_link_click, {
    accession_code <- input$comm_class_link_click
    # Check for valid accession code
    if (is.null(accession_code) ||
      is.na(accession_code) ||
      accession_code == "") {
      shiny::showNotification(paste0("No accession code found for that community classification"), type = "error")
      return()
    }
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      state$detail_type("community-classification")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("community-classification", accession_code, output, session)
    }
  })

  shiny::observeEvent(input$comm_link_click, {
    accession_code <- input$comm_link_click
    # Check for valid accession code
    if (is.null(accession_code) ||
      is.na(accession_code) ||
      accession_code == "") {
      shiny::showNotification(paste0("No accession code found for that community concept"), type = "error")
      return()
    }
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      state$detail_type("community-concept")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("community-concept", accession_code, output, session)
    }
  })

  shiny::observeEvent(input$taxa_link_click, {
    accession_code <- input$taxa_link_click
    # Check for valid accession code
    if (is.null(accession_code) ||
      is.na(accession_code) ||
      accession_code == "") {
      shiny::showNotification(paste0("No accession code found for that taxon observation"), type = "error")
      return()
    }
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      state$detail_type("taxon-observation")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("taxon-observation", accession_code, output, session)
    }
  })

  shiny::observeEvent(input$proj_link_click, {
    accession_code <- input$proj_link_click
    # Check for valid accession code
    if (is.null(accession_code) ||
      is.na(accession_code) ||
      accession_code == "") {
      shiny::showNotification(paste0("No accession code found for that project"), type = "error")
      return()
    }
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      state$detail_type("project")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("project", accession_code, output, session)
    }
  })

  shiny::observeEvent(input$party_link_click, {
    accession_code <- input$party_link_click
    # Check for valid accession code
    if (is.null(accession_code) ||
      is.na(accession_code) ||
      accession_code == "") {
      shiny::showNotification(paste0("No accession code found for that party"), type = "error")
      return()
    }
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      state$detail_type("party")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("party", accession_code, output, session)
    }
  })

  # STATE PERSISTENCE ____________________________________________________________________________
  shiny::onBookmark(function(state_obj) {
    state_obj$values$current_tab <- input$page
    state_obj$values$detail_type <- state$detail_type()
    state_obj$values$selected_accession <- state$selected_accession()
    state_obj$values$details_open <- state$details_open()

    # Add map state
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

    # Safely reopen the detail overlay
    if (isTRUE(context$values$details_open)) {
      detail_open_observer <- shiny::observe({
        state$detail_type(context$values$detail_type)
        state$selected_accession(context$values$selected_accession)
        state$details_open(TRUE)
        show_detail_view(
          state$detail_type(),
          state$selected_accession(),
          output,
          session
        )
        detail_open_observer$destroy()
      })
    }

    invisible(NULL)
  })

  shiny::observe({
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })

  # Exclude DataTable inputs from bookmarks (avoids storing the large table state)
  shiny::setBookmarkExclude(c(
    "plot_table_rows_selected", "plot_table_rows_all", "plot_table_rows_current",
    "plot_table_search", "plot_table_state", "plot_table_row_last_clicked",
    "plot_table_cell_clicked", "proj_table_rows_selected", "proj_table_rows_all",
    "proj_table_rows_current", "proj_table_search", "proj_table_state",
    "proj_table_row_last_clicked", "proj_table_cell_clicked", "comm_table_rows_selected",
    "comm_table_rows_all", "comm_table_rows_current", "comm_table_search", "comm_table_state",
    "comm_table_row_last_clicked", "comm_table_cell_clicked", "party_table_rows_selected",
    "party_table_rows_all", "party_table_rows_current", "party_table_search", "party_table_state",
    "party_table_row_last_clicked", "party_table_cell_clicked",
    "map_bounds", "map_marker_mouseout", "map_marker_mouseover",
    "map_marker_click", "map_click"
  ))
}

# Helper function to load data types with API fallback
# This function will try to load data from the API first, and if it fails, it
# will fall back to reading from a cached RDS file.
# If the API is not used, it will only read from the cached file.
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

#' This function checks if the specified file exists and reads it as an RDS file.
#' If the file does not exist, it shows an error notification and returns an empty data frame.
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
