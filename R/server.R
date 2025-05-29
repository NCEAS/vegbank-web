#' Shiny Server for Vegbank Web Application
#'
#' Initializes server-side functionality for data visualization and interactivity.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return Called for its side effects.
#' @importFrom ggplot2 .data
#' @importFrom shiny reactiveVal observeEvent observe req renderUI renderText showNotification
#'             updateNavbarPage invalidateLater reactiveValuesToList onBookmark onBookmarked
#'             onRestore
#' @importFrom DT renderDataTable datatable dataTableProxy selectRows
#' @importFrom htmltools tags
#' @importFrom leaflet renderLeaflet leafletProxy
#' @importFrom utils head

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

  # Load data from local files
  plot_data <- readRDS("inst/shiny/www/plot_obs_minimal_all_data.RDS")
  taxa_data <- readRDS("inst/shiny/www/taxa_top5.RDS")
  comm_data <- readRDS("inst/shiny/www/comm_class_minimal_all.RDS")
  colnames(comm_data)[colnames(comm_data) == "accessioncode"] <- "obsaccessioncode"

  update_map_view <- function(idx) {
    data <- plot_data
    leaflet::leafletProxy("map", session) |>
      plot_map$update_map_view(
        data$longitude[idx],
        data$latitude[idx],
        paste("Plot", data$authorobscode[idx], "is here!")
      )
  }

  # RENDER UI ELEMENTS __________________________________________________________________
  output$dataSummary <- shiny::renderUI({
    htmltools::tags$p(
      "Vegbank is a database of vegetation plot data. Navigate to the 'Plots > Table' tab ",
      "to browse the available plot data. Each row in the table represents a plot observation.",
      "You can also view the plot locations on a map by navigating to the 'Plots > Map' tab. ",
      "Clicking on the see details button in a row in the table or a link in the pin label on the ",
      "map will display detailed information about that plot observation including information ",
      "about the plot location, species observed, and other details."
    )
  })

  output$dataTable <- DT::renderDataTable({
    plot_table$process_table_data(plot_data, taxa_data, comm_data)
  })

  output$map <- leaflet::renderLeaflet({
    # Always initially render with default values
    plot_map$process_map_data(plot_data)
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
      selected_row_accession <- plot_data[i, "obsaccessioncode"]

      # Check for valid accession code
      if (is.null(selected_row_accession) ||
        is.na(selected_row_accession) ||
        selected_row_accession == "") {
        progress_handler$show_notification(paste0("No accession code found for row: ", i), type = "error")
        return()
      }

      # Select the row in the datatable (with error handling)
      tryCatch(
        {
          dt_proxy <- DT::dataTableProxy("dataTable")
          DT::selectRows(dt_proxy, i)
        },
        error = function(e) {
          print(paste("Error selecting row:", e$message))
        }
      )

      state$detail_type("plot")
      state$selected_accession(selected_row_accession)
      state$details_open(TRUE)
      # Open the details view
      show_detail_view("plot", selected_row_accession, output, session)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$show_on_map,
    {
      idx <- as.numeric(input$show_on_map)

      # Check for valid index before proceeding
      if (is.na(idx) || idx < 1 || idx > nrow(plot_data)) {
        return()
      }

      # Check for valid latitude and longitude
      lat <- plot_data$latitude[idx]
      lon <- plot_data$longitude[idx]

      if (is.na(lat) || is.na(lon) || !is.numeric(lat) || !is.numeric(lon)) {
        progress_handler$show_notification("Cannot show on map: Missing or invalid coordinates for this plot",
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
          session$sendCustomMessage(type = "closeDropdown", message = list())
          session$sendCustomMessage("invalidateMapSize", list())
          update_map_view(idx)
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
      state$detail_type("plot")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("plot", accession_code, output, session)
    }
  })

  shiny::observeEvent(input$comm_link_click, {
    accession_code <- input$comm_link_click
    # Check for valid accession code
    if (is.null(accession_code) ||
      is.na(accession_code) ||
      accession_code == "") {
      progress_handler$show_notification(paste0("No accession code found for that community"), type = "error")
      return()
    }
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      state$detail_type("community")
      state$selected_accession(accession_code)
      state$details_open(TRUE)
      show_detail_view("community", accession_code, output, session)
    }
  })

  # TODO: Add observer for taxa link clicks
  # TODO: Add observer for search bar

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
    "dataTable_rows_selected", "dataTable_rows_all", "dataTable_rows_current",
    "dataTable_search", "dataTable_state", "dataTable_row_last_clicked",
    "dataTable_cell_clicked", "map_bounds", "map_marker_mouseout", "map_marker_mouseover",
    "map_marker_click", "map_click"
  ))
}
