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
    plot_data = shiny::reactiveVal(NULL),
    taxa_data = shiny::reactiveVal(NULL)
  )

  # Load data in a more efficient way - just once at startup
  full_data <- readRDS("inst/shiny/www/plot_obs_minimal_all.RDS")
  state$plot_data(full_data)
  state$taxa_data(readRDS("inst/shiny/www/taxa_top5.RDS"))

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

  # Simplify the dataTable implementation to ensure it compiles
  output$dataTable <- DT::renderDataTable({
    # Create a simplified paginated data approach
    data <- state$plot_data()

    # Return early if no data
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame("No Data Available" = "Please try again or check your connection"),
        options = list(dom = "t")
      ))
    }

    # Pre-generate the action buttons in R - more reliable than JS manipulation
    action_buttons <- vapply(seq_len(nrow(data)), function(i) {
      sprintf(
        '<div class="btn-group btn-group-sm">
          <button class="btn btn-sm btn-outline-primary" 
                  onclick="Shiny.setInputValue(\'see_details\', %d, {priority: \'event\'})">Details</button>
          <button class="btn btn-sm btn-outline-secondary"
                  onclick="Shiny.setInputValue(\'show_on_map\', %d, {priority: \'event\'})">Map</button>
         </div>',
        i, i
      )
    }, character(1))
    
    # Add the pre-generated buttons as a column
    data <- cbind(Actions = action_buttons, data)

    # Create the datatable with options for large datasets
    DT::datatable(
      data,
      rownames = FALSE,
      escape = FALSE,  # Important: Allow HTML in the Actions column
      selection = list(mode = "single", target = "row", selectable = FALSE),
      options = list(
        dom = "frtip",  # Simplified DOM structure
        pageLength = 100,
        scrollY = "calc(100vh - 300px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        deferRender = TRUE,
        processing = TRUE,
        columnDefs = list(
          # Make the actions column not sortable or searchable
          list(targets = 0, orderable = FALSE, searchable = FALSE, width = "120px")
        )
      )
    )
  })
  
  # output$map <- leaflet::renderLeaflet({
  #   fetch_map_data()
  # })

  # EVENT HANDLERS _________________________________________________________________________________
  shiny::observeEvent(input$see_details, {
    i <- as.numeric(input$see_details)
    
    # Debug print to see what value is coming in
    print(paste("Details button clicked for row:", i))
    
    # Get the data - need to use the reactive to ensure we have the latest data
    data_table <- state$plot_data()
    
    # More robust validation
    if (is.null(i) || is.na(i) || length(i) == 0 || i < 1 || i > nrow(data_table)) {
      shiny::showNotification("Invalid row selection", type = "error")
      return()
    }
    
    # Check if the column exists
    if (!"data.obsaccessioncode" %in% colnames(data_table)) {
      shiny::showNotification("Column 'data.obsaccessioncode' not found in data", type = "error")
      print(paste("Available columns:", paste(colnames(data_table), collapse=", ")))
      return()
    }
    
    # Get the accession code safely
    selected_row_accession <- data_table[i, "data.obsaccessioncode"]
    
    # Check for valid accession code
    if (is.null(selected_row_accession) || is.na(selected_row_accession) || selected_row_accession == "") {
      shiny::showNotification("No accession code found for row: " + i, type = "error")
      return()
    }
    
    # Set reactive value
    state$selected_accession(selected_row_accession)
    
    # Select the row in the datatable (with error handling)
    tryCatch({
      dt_proxy <- DT::dataTableProxy("dataTable")
      DT::selectRows(dt_proxy, i)
    }, error = function(e) {
      print(paste("Error selecting row:", e$message))
    })
    
    # Open the details view
    update_and_open_details(selected_row_accession)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  shiny::observeEvent(input$show_on_map, {
    idx <- as.numeric(input$show_on_map)
    
    # Check for valid index before proceeding
    if (is.na(idx) || idx < 1) {
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
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
    # session$doBookmark()
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

  # shiny::observeEvent(input$search_enter, {
  #   if (!state$data_loading()) {
  #     shiny::updateNavbarPage(session, "page", selected = "Table")
  #     state$pagination$last_plot_id(NULL)
  #     fetch_table_data()
  #   }
  # })

  # STATE PERSISTENCE ____________________________________________________________________________
  # shiny::onBookmark(function(state_obj) {
  #   state_obj$values$selected_accession <- state$selected_accession()
  #   state_obj$values$details_open <- state$details_open()
  #   state_obj
  # })

  # shiny::onBookmarked(function(url) {
  #   shiny::updateQueryString(url)
  # })

  # shiny::onRestore(function(state_obj) {
  #   if (!is.null(state_obj$values$selected_accession)) {
  #     acc <- state_obj$values$selected_accession
  #     shiny::observeEvent(state$data(),
  #       {
  #         update_and_open_details(acc)

  #         data <- state$data()
  #         idx <- match(acc, data$obsaccessioncode)
  #         if (!is.na(idx)) {
  #           dt_proxy <- DT::dataTableProxy("dataTable")
  #           DT::selectRows(dt_proxy, idx, ignore.selectable = TRUE)
  #         }
  #       },
  #       once = TRUE
  #     )
  #   }
  #   invisible(NULL)
  # })

  # shiny::observe({
  #   shiny::reactiveValuesToList(input)
  #   session$doBookmark()
  # })
}
