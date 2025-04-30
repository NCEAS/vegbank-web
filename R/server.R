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
    selected_accession = shiny::reactiveVal(NULL),
    details_open = shiny::reactiveVal(FALSE),
    selected_community_accession = shiny::reactiveVal(NULL),
    detail_type = shiny::reactiveVal(NULL) # Tracks detail type - "plot" or "community"
  )

  # DATA OPERATIONS _______________________________________________________________________________
  fetch_map_data <- function() {
    show_progress("Loading map data...")(function(step, complete) {
      step(0.2, "Fetching pins")

      result <- veg_bank_api$get_map_points()

      if (!result$success) {
        step(0.3, "Error loading map data")
        plot_map$create_empty_map()
      } else {
        step(0.5, "Processing map data")
        map <- plot_map$process_map_data(result$data)

        complete("Map rendered")
        map
      }
    })
  }

  # Replace the separate detail view functions with our generic one
  update_and_open_details <- function(accession_code) {
    show_detail_view("plot", accession_code, state, output, session, veg_bank_api)
  }

  update_and_open_community_details <- function(accession_code) {
    show_detail_view("community", accession_code, state, output, session, veg_bank_api)
  }

  update_map_view <- function(latitude, longitude, popup_text) {
    leaflet::leafletProxy("map", session) |>
      plot_map$update_map_view(longitude, latitude, popup_text)
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

  # Create a callback function for the table data
  output$dataTable <- DT::renderDataTable(
    {
      # Define the columns for the datatable
      empty_data <- data.frame(
        Actions = character(0),
        `Author Plot Code` = character(0),
        Location = character(0),
        `Top Taxa` = character(0),
        Community = character(0)
      )

      # Define the options for server-side processing
      dt_options <- list(
        processing = TRUE,
        serverSide = TRUE,
        ajax = list(
          url = "http://127.0.0.1:28015/get_datatable_observations",
          contentType = "application/json"
        ),
        columnDefs = list(
          list(targets = 0, width = "10%", orderable = FALSE),
          list(targets = 1, width = "10%"),
          list(targets = 2, width = "10%"),
          list(targets = 3, width = "45%"),
          list(targets = 4, width = "25%")
        ),
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")),
        deferRender = TRUE,
        initComplete = DT::JS("
        function(settings, json) {
          // Setup event handlers once the table is initialized
          $(this).on('click', '.details-btn', function() {
            var plotId = $(this).data('row');
            Shiny.setInputValue('see_details', plotId, {priority: 'event'});
            return false;
          });

          $(this).on('click', '.map-btn', function() {
            var acc = $(this).data('acc');
            Shiny.setInputValue('show_on_map', acc, {priority: 'event'});
            return false;
          });

          $(this).on('click', '.community-link', function() {
            var acc = $(this).data('acc');
            Shiny.setInputValue('comm_link_click', acc, {priority: 'event'});
            return false;
          });
        }
      ")
      )

      # Create the datatable with proper options
      DT::datatable(
        empty_data,
        options = dt_options,
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        fillContainer = TRUE
      )
    },
    server = FALSE
  ) # Using server=FALSE to make browser connect directly to API


  # Event handler for the see_details button
  shiny::observeEvent(input$see_details, {
    plot_id <- as.numeric(input$see_details)
    message("See details clicked for plot ID: ", plot_id)

    # Fetch the plot details by ID from the API
    withProgress(message = "Loading plot details...", {
      result <- veg_bank_api$get_plot_details(plot_id)

      if (!result$success) {
        showNotification("Failed to load plot details", type = "error")
      } else {
        plot_data <- result$data
        # Set the plot accession code for the details view
        acc_code <- plot_data$obsaccessioncode
        update_and_open_details(acc_code)
      }
    })
  })

  # Event handler for the map button
  shiny::observeEvent(input$show_on_map, {
    accession_code <- input$show_on_map
    message("Show on map clicked for accession: ", accession_code)

    # Fetch location data for this plot from the API
    withProgress(message = "Finding plot location...", {
      result <- veg_bank_api$get_plot_location(accession_code)

      if (!result$success) {
        showNotification("Failed to locate plot on map", type = "error")
      } else {
        plot_data <- result$data

        # Update map view with this location
        shiny::updateNavbarPage(session, "page", selected = "Map")

        # Create a self-destroying observer
        map_update_observer <- shiny::observe({
          # Only proceed if we're on the map page
          shiny::req(input$page == "Map")

          session$sendCustomMessage(type = "closeDropdown", message = list())
          session$sendCustomMessage("invalidateMapSize", list())

          # Update map view with coordinates
          leaflet::leafletProxy("map", session) |>
            plot_map$update_map_view(
              plot_data$longitude,
              plot_data$latitude,
              paste("Plot", plot_data$authorobscode, "is here!")
            )

          map_update_observer$destroy()
        })
      }
    })
  })

  # Event handler for community links
  shiny::observeEvent(input$comm_link_click, {
    accession_code <- input$comm_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      update_and_open_community_details(accession_code)
    }
  })

  output$map <- leaflet::renderLeaflet({
    fetch_map_data()
  })

  # STATE PERSISTENCE ____________________________________________________________________________
  shiny::onBookmark(function(state_obj) {
    state_obj$values$selected_accession <- state$selected_accession()
    state_obj$values$details_open <- state$details_open()
    state_obj
  })

  shiny::onBookmarked(function(url) {
    shiny::updateQueryString(url)
  })

  shiny::onRestore(function(state_obj) {
    if (!is.null(state_obj$values$selected_accession)) {
      acc <- state_obj$values$selected_accession
      update_and_open_details(acc)
    }
    invisible(NULL)
  })

  shiny::observe({
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })
}
