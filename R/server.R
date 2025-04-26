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
    data = shiny::reactiveVal(NULL),
    data_loading = shiny::reactiveVal(FALSE), # Track if data is currently loading
    selected_accession = shiny::reactiveVal(NULL),
    details_open = shiny::reactiveVal(FALSE),
    map_request = shiny::reactiveVal(NULL),
    selected_community_accession = shiny::reactiveVal(NULL),
    detail_type = shiny::reactiveVal(NULL), # Tracks detail type - "plot" or "community"
    pagination = list(
      current_page_size = shiny::reactiveVal(100),
      last_plot_id = shiny::reactiveVal(NULL),
      has_more_data = shiny::reactiveVal(TRUE)
    )
  )

  # HELPER FUNCTIONS ______________________________________________________________________________
  update_pagination_state <- function(data, page_size) {
    if ("has_more" %in% names(data)) {
      state$pagination$has_more_data(data$has_more)
      data <- data$results
    } else {
      state$pagination$has_more_data(nrow(data) >= page_size)
    }

    if (nrow(data) > 0) {
      state$pagination$last_plot_id(data$plot_id[nrow(data)])
    }

    data
  }

  # DATA OPERATIONS _______________________________________________________________________________
  fetch_table_data <- function(page_size = NULL, prev_plot_id = NULL, force_refresh = FALSE) {
    # Don't fetch if already loading
    if (state$data_loading()) {
      return(NULL)
    }

    # Mark as loading
    state$data_loading(TRUE)

    page_size <- if (is.null(page_size)) state$pagination$current_page_size() else page_size

    withProgress(
      message = "Loading table data...",
      value = 0,
      {
        incProgress(0.2, "Fetching data")

        result <- veg_bank_api$get_table_data(page_size, prev_plot_id)

        if (!result$success) {
          incProgress(0.2, "Error loading data")
          shiny::showNotification("Failed to load data. Please try again.", type = "error")
        } else {
          incProgress(0.3, "Processing data")
          data <- update_pagination_state(result$data, page_size)
          state$data(data)
        }

        # Mark as done loading regardless of success/failure
        state$data_loading(FALSE)

        incProgress(0.5, "Done")
      }
    )
  }

  # Separate observer to handle data loading
  observeEvent(input$page,
    {
      if (input$page == "Table" && is.null(state$data()) && !state$data_loading()) {
        fetch_table_data()
      }
    },
    ignoreInit = FALSE
  )

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

  update_and_open_details <- function(accession_code) {
    show_progress("Loading details...")(function(step, complete) {
      step(0.2, "Fetching details")

      result <- veg_bank_api$get_observation_details(accession_code)

      if (!result$success) {
        step(0.3, "Error loading details")
        shiny::showNotification("Failed to load details. Please try again.", type = "error")
        FALSE
      } else {
        step(0.5, "Processing details")

        details <- build_details_view(result$data)
        output$plot_id_details <- details$plot_id_details
        output$locationDetails <- details$location_details
        output$layout_details <- details$layout_details
        output$environmental_details <- details$environmental_details
        output$methods_details <- details$methods_details
        output$plot_quality_details <- details$plot_quality_details
        output$taxaDetails <- details$taxa_details

        # Clear community details
        output$community_name <- shiny::renderUI(NULL)
        output$community_description <- shiny::renderUI(NULL)

        state$selected_accession(accession_code)
        state$details_open(TRUE)
        state$detail_type("plot") # Set to plot type

        complete("Details ready")
        session$sendCustomMessage("openOverlay", list())
        session$sendCustomMessage("updateDetailType", list(type = "plot"))

        TRUE
      }
    })
  }

  # New function to handle community details
  update_and_open_community_details <- function(accession_code) {
    show_progress("Loading community details...")(function(step, complete) {
      step(0.2, "Fetching community details")

      result <- veg_bank_api$get_community_details(accession_code)

      if (!result$success) {
        step(0.3, "Error loading community details")
        shiny::showNotification("Failed to load community details. Please try again.", type = "error")
        FALSE
      } else {
        step(0.5, "Processing community details")

        details <- build_community_details_view(result$data)
        output$plot_id_details <- shiny::renderUI(NULL)
        output$locationDetails <- shiny::renderUI(NULL)
        output$layout_details <- shiny::renderUI(NULL)
        output$environmental_details <- shiny::renderUI(NULL)
        output$methods_details <- shiny::renderUI(NULL)
        output$plot_quality_details <- shiny::renderUI(NULL)
        output$taxaDetails <- shiny::renderUI(NULL)

        # Add new outputs for community details
        output$community_name <- details$community_name
        output$community_description <- details$community_description

        state$selected_community_accession(accession_code)
        state$details_open(TRUE)
        state$detail_type("community") # Set to community type

        complete("Community details ready")
        session$sendCustomMessage("openOverlay", list())
        session$sendCustomMessage("updateDetailType", list(type = "community"))

        TRUE
      }
    })
  }

  find_page_with_accession <- function(accession_code, callback) {
    state$pagination$last_plot_id(NULL)

    search_for_accession <- function() {
      current_data <- state$data()

      if (!is.null(current_data)) {
        idx <- which(current_data$obsaccessioncode == accession_code)
        if (length(idx) > 0) {
          callback(idx)
          TRUE
        } else if (state$pagination$has_more_data()) {
          fetch_table_data(prev_plot_id = state$pagination$last_plot_id())
          FALSE
        } else {
          message("Accession code not found in any page: ", accession_code)
          TRUE
        }
      } else {
        if (state$pagination$has_more_data()) {
          fetch_table_data(prev_plot_id = state$pagination$last_plot_id())
          FALSE
        } else {
          message("Accession code not found in any page: ", accession_code)
          TRUE
        }
      }
    }

    found <- search_for_accession()

    if (!found) {
      search_timer <- shiny::reactiveTimer(500)
      shiny::observe({
        search_timer()
        found <- search_for_accession()
        if (found) {
          # Fix the observeEvent.priority issue
          # This should be handled differently - perhaps with priority in observeEvent
          shiny::getDefaultReactiveDomain()$flushQueue()
        }
      })
    }
  }

  update_map_view <- function(idx) {
    data <- state$data()
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

  # Simplified dataTable renderer - only handles rendering, not data loading
  output$dataTable <- DT::renderDataTable({
    # Use reactives to trigger re-render when data changes
    data <- state$data()
    # Use the plot_table module to process the data
    plot_table$process_table_data(data)
  })

  output$tablePagination <- shiny::renderUI({
    # Only show pagination UI if we have data
    if (!is.null(state$data())) {
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::div(
            class = "d-flex justify-content-between align-items-center mt-2",
            shiny::div(
              class = "pagination-info",
              shiny::textOutput("paginationStatus")
            ),
            shiny::div(
              class = "pagination-controls",
              shiny::actionButton("prevPage", "Previous", class = "btn btn-outline-secondary"),
              shiny::actionButton("nextPage", "Next", class = "btn btn-outline-primary")
            )
          )
        )
      )
    }
  })

  output$paginationStatus <- shiny::renderText({
    data <- state$data()
    if (is.null(data)) {
      "No data available"
    } else {
      paste0(
        "Showing ", nrow(data), " records",
        if (state$pagination$has_more_data()) " (more available)" else " (end of data)"
      )
    }
  })

  output$map <- leaflet::renderLeaflet({
    fetch_map_data()
  })

  # EVENT HANDLERS _________________________________________________________________________________
  shiny::observeEvent(input$prevPage, {
    if (!state$data_loading()) {
      state$pagination$last_plot_id(NULL)
      fetch_table_data()
    }
  })

  shiny::observeEvent(input$nextPage, {
    if (state$pagination$has_more_data() && !state$data_loading()) {
      # When moving to next page, we need to preserve the last_plot_id
      prev_id <- state$pagination$last_plot_id()
      fetch_table_data(prev_plot_id = prev_id)
    }
  })

  shiny::observeEvent(input$show_on_map, {
    idx <- as.numeric(input$show_on_map)
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
  })

  shiny::observeEvent(input$see_details, {
    i <- as.numeric(input$see_details)
    dt_proxy <- DT::dataTableProxy("dataTable")
    DT::selectRows(dt_proxy, i, ignore.selectable = TRUE)

    data_table <- state$data()
    selected_row_accession <- data_table[i, "obsaccessioncode"]
    update_and_open_details(selected_row_accession)
  })

  shiny::observeEvent(input$close_details, {
    data <- state$data()
    idx <- which(data$obsaccessioncode == state$selected_accession())

    if (length(idx) > 0) {
      dt_proxy <- DT::dataTableProxy("dataTable")
      DT::selectRows(dt_proxy, idx, ignore.selectable = FALSE)
    }

    state$details_open(FALSE)
    state$selected_accession(NULL)
    state$selected_community_accession(NULL) # Reset community accession as well
    state$detail_type(NULL) # Reset detail type
    session$doBookmark()
  })

  shiny::observeEvent(input$label_link_click, {
    accession_code <- input$label_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      update_and_open_details(accession_code)
    }
  })

  shiny::observeEvent(input$comm_link_click, {
    accession_code <- input$comm_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      update_and_open_community_details(accession_code)
    }
  })

  shiny::observeEvent(input$search_enter, {
    if (!state$data_loading()) {
      shiny::updateNavbarPage(session, "page", selected = "Table")
      state$pagination$last_plot_id(NULL)
      fetch_table_data()
    }
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
      shiny::observeEvent(state$data(),
        {
          update_and_open_details(acc)

          data <- state$data()
          idx <- match(acc, data$obsaccessioncode)
          if (!is.na(idx)) {
            dt_proxy <- DT::dataTableProxy("dataTable")
            DT::selectRows(dt_proxy, idx, ignore.selectable = TRUE)
          }
        },
        once = TRUE
      )
    }
    invisible(NULL)
  })

  shiny::observe({
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })
}
