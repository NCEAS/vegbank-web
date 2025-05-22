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
    # TODO: Why doe these need to be reactive? They never respond to inputs right now.
    data = shiny::reactiveVal(NULL),
    data_loading = shiny::reactiveVal(FALSE), # Track if data is currently loading
    selected_accession = shiny::reactiveVal(NULL),
    details_open = shiny::reactiveVal(FALSE),
    map_request = shiny::reactiveVal(NULL),
    selected_community_accession = shiny::reactiveVal(NULL),
    detail_type = shiny::reactiveVal(NULL) # Tracks detail type - "plot" or "community"
  )

  # Load data from local files
  plot_data <- readRDS("inst/shiny/www/plot_obs_minimal_all_data.RDS")
  taxa_data <- readRDS("inst/shiny/www/taxa_top5.RDS")
  comm_data <- readRDS("inst/shiny/www/comm_class_minimal_all.RDS")
  colnames(comm_data)[colnames(comm_data) == "accessioncode"] <- "obsaccessioncode"

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

  output$dataTable <- DT::renderDataTable({
    plot_table$process_table_data(plot_data, taxa_data, comm_data)
  })

  output$map <- leaflet::renderLeaflet({
    plot_map$process_map_data(plot_data)
  })

  # EVENT HANDLERS _________________________________________________________________________________
  shiny::observeEvent(input$see_details,
    {
      i <- as.numeric(input$see_details)
      data_table <- plot_data

      # More robust validation
      if (is.null(i) || is.na(i) || length(i) == 0 || i < 1 || i > nrow(data_table)) {
        shiny::showNotification("Invalid row selection", type = "error")
        return()
      }

      # Check if the column exists
      if (!"obsaccessioncode" %in% colnames(data_table)) {
        shiny::showNotification("Column 'obsaccessioncode' not found in data", type = "error")
        print(paste("Available columns:", paste(colnames(data_table), collapse = ", ")))
        return()
      }

      # Get the accession code safely
      selected_row_accession <- data_table[i, "obsaccessioncode"]

      # Check for valid accession code
      if (is.null(selected_row_accession) ||
            is.na(selected_row_accession) ||
            selected_row_accession == "") {
        shiny::showNotification(paste0("No accession code found for row: ", i), type = "error")
        return()
      }

      # Set reactive value
      state$selected_accession(selected_row_accession)

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

      # Open the details view
      show_detail_view("plot", selected_row_accession, state, output, session, veg_bank_api)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$show_on_map,
    {
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
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

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
      show_detail_view("plot", accession_code, state, output, session, veg_bank_api)
    }
  })

  shiny::observeEvent(input$comm_link_click, {
    accession_code <- input$comm_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      show_detail_view("community", accession_code, state, output, session, veg_bank_api)
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
