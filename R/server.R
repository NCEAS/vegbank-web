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

  # Load data from local files
  state$plot_data(readRDS("inst/shiny/www/plot_obs_minimal_all_data.RDS"))
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
    withProgress(
      message = "Compiling table data...",
      value = 0,
      {
        # Join taxa data with plot data
        plot_data <- state$plot_data()
        taxa_data <- state$taxa_data()


        # Return early if no data
        if (is.null(plot_data) || nrow(plot_data) == 0 ||
          is.null(taxa_data) || nrow(taxa_data) == 0) {
          setProgress(1, "Missing data!")
          shiny::showNotification(
            "Missing required data. Please try again or check your connection.",
            type = "error"
          )
          return(DT::datatable(
            data.frame("Missing required data" = "Please try again or check your connection"),
            options = list(dom = "t")
          ))
        }

        setProgress(0.2, "Cleaning author observation codes")
        n_rows <- nrow(plot_data)
        
        author_codes <- if ("authorplotcode" %in% colnames(plot_data)) {
          # Replace NA or empty values with "Unknown"
          ifelse(is.na(plot_data$authorplotcode) | plot_data$authorplotcode == "", 
                 "Not Provided", 
                 plot_data$authorplotcode)
        } else {
          rep("Not Provided", n_rows)
        }
        
        setProgress(0.3, "Cleaning location data")
        locations <- if ("stateprovince" %in% colnames(plot_data)) {
          # Replace NA or empty values with "Unknown"
          ifelse(is.na(plot_data$stateprovince) | plot_data$stateprovince == "", 
                 "Not Provided", 
                 plot_data$stateprovince)
        } else {
          rep("Not Provided", n_rows)
        }

        # Process taxa data to create bulleted lists
        setProgress(0.4, "Creating taxa lists...")
        data <- dplyr::left_join(plot_data, taxa_data, by = "observation_id")

        taxa_lists <- data %>%
          dplyr::group_by(observation_id) %>%
          dplyr::summarize(
            top_taxa = paste0(
              '<ul class="taxa-list">',
              paste0(
                '<li><a href="#" onclick="Shiny.setInputValue(\'taxa_link_click\', \'',
                accessioncode, '\', {priority: \'event\'}); return false;">',
                int_currplantscinamenoauth, "</a> (", maxcover, "%)</li>",
                collapse = ""
              ),
              "</ul>"
            ),
            .groups = "drop"
          )

        # Join the taxa lists back to the plot data
        data <- dplyr::left_join(plot_data, taxa_lists, by = "observation_id")

        setProgress(0.6, "Creating action buttons...")
        # Create action buttons
        action_buttons <- vapply(seq_len(nrow(data)), function(i) {
          sprintf(
            '<div class="btn-group btn-group-sm">
          <button class="btn btn-sm btn-outline-primary"
            onclick="Shiny.setInputValue(\'see_details\', %d, {priority: \'event\'})">
              Details
          </button>
          <button class="btn btn-sm btn-outline-secondary"
            onclick="Shiny.setInputValue(\'show_on_map\', %d, {priority: \'event\'})">
              Map
          </button>
         </div>',
            i, i
          )
        }, character(1))

        # TODO: Create community links
        community_links <- vapply(seq_len(nrow(data)), function(i) {
          comm_name <- if (!is.null(data$commname) && !is.na(data$commname[i])) data$commname[i] else "Not Provided"
          comm_code <- if (!is.null(data$commconceptaccessioncode) && !is.na(data$commconceptaccessioncode[i])) {
            data$commconceptaccessioncode[i]
          } else {
            ""
          }
          if (comm_code != "") {
            sprintf(
              '<a href="#" onclick="Shiny.setInputValue(\'comm_link_click\', \'%s\', 
                {priority: \'event\'}); return false;">%s</a>',
              comm_code, comm_name
            )
          } else {
            comm_name
          }
        }, character(1))

        setProgress(0.8, "Building table...")

        # Create a display table with only the desired columns and names
        display_data <- data.frame(
          "Actions" = action_buttons,
          "Author Plot Code" = author_codes,
          "Location" = locations,
          "Top Taxa" = data$top_taxa,
          "Community" = community_links,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        # Create the datatable with options for large datasets
        DT::datatable(
          display_data,
          rownames = FALSE,
          escape = FALSE, # Important: Allow HTML in the Actions column
          selection = list(mode = "single", target = "row", selectable = FALSE),
          options = list(
            dom = "frtip", # Simplified DOM structure
            pageLength = 100,
            scrollY = "calc(100vh - 300px)",
            scrollX = TRUE,
            scrollCollapse = TRUE,
            deferRender = TRUE,
            processing = TRUE,
            columnDefs = list(
              list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
              list(targets = 1, width = "15%"),
              list(targets = 2, width = "10%"),
              list(targets = 3, orderable = FALSE, searchable = TRUE, width = "45%"),
              list(targets = 4, width = "20%")
            )
          )
        )
      }
    )
  })

  # output$map <- leaflet::renderLeaflet({
  #   fetch_map_data()
  # })

  # EVENT HANDLERS _________________________________________________________________________________
  shiny::observeEvent(input$see_details,
    {
      i <- as.numeric(input$see_details)
      data_table <- state$plot_data()

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
