#' Shiny Server for Vegbank Web Application
#'
#' Initializes server-side functionality for data visualization and interactivity.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return Called for its side effects.
#' @importFrom ggplot2 .data
server <- function(input, output, session) {
  # TODO: rework data flow to use server-side pagination, search, and details endpoint
  rv_data <- shiny::reactiveVal(NULL)
  selected_accession <- shiny::reactiveVal(NULL)
  details_open <- shiny::reactiveVal(FALSE)
  map_request <- shiny::reactiveVal(NULL)

  # Add pagination reactive values
  current_page_size <- shiny::reactiveVal(100)
  last_plot_id <- shiny::reactiveVal(NULL)
  has_more_data <- shiny::reactiveVal(TRUE)

  # Function to fetch paginated table data
  fetch_table_data <- function(page_size = NULL, prev_plot_id = NULL, force_refresh = FALSE) {
    shiny::withProgress(message = "Loading table data...", value = 0, {
      # Use provided values or defaults from reactive values
      page_size <- if (is.null(page_size)) current_page_size() else page_size

      # Construct API URL
      base_url <- "http://127.0.0.1:28015/get_observation_table"
      api_url <- if (is.null(prev_plot_id)) {
        paste0(base_url, "/", page_size)
      } else {
        paste0(base_url, "/", page_size, "/", prev_plot_id)
      }

      shiny::incProgress(0.2, detail = "Fetching data")
      message("Fetching paginated table data: ", api_url)

      # Timer for table data
      start_time <- Sys.time()
      response <- tryCatch(
        {
          httr::GET(api_url)
        },
        error = function(e) {
          message("Error connecting to API: ", e$message)
          NULL
        }
      )
      end_time <- Sys.time()
      message(paste(
        "Table data retrieval took",
        round(difftime(end_time, start_time, units = "secs"), 2), "seconds"
      ))

      if (!is.null(response) && httr::status_code(response) == 200) {
        shiny::incProgress(0.6, detail = "Processing data")
        raw_content <- httr::content(response, "text")

        if (nchar(raw_content) > 0 && jsonlite::validate(raw_content)) {
          data <- jsonlite::fromJSON(raw_content)

          # Check if we have a has_more field in response to determine pagination status
          if ("has_more" %in% names(data)) {
            has_more_data(data$has_more)
            data <- data$results # Assuming actual data is in a results field
          } else {
            # If no has_more field, check if we got less data than requested
            has_more_data(nrow(data) >= page_size)
          }

          # Update last plot ID if we have data
          if (nrow(data) > 0) {
            last_plot_id(data$plot_id[nrow(data)])
          }

          # Update the reactive data
          rv_data(data)
          shiny::incProgress(1, detail = "Done")
          return(TRUE)
        } else {
          message("Invalid JSON response from API")
          shiny::showNotification("Error loading data: Invalid format", type = "error")
          shiny::incProgress(1, detail = "Error")
          return(FALSE)
        }
      } else {
        status <- if (!is.null(response)) httr::status_code(response) else "connection failed"
        message("API Error: Status ", status)
        shiny::showNotification("Failed to load data. Please try again.", type = "error")
        shiny::incProgress(1, detail = "Error")
        return(FALSE)
      }
    })
  }

  # Load initial data when app starts
  shiny::observe({
    # Load first page of data when app initializes
    fetch_table_data()
  })

  # Find page with specific accession code
  find_page_with_accession <- function(accession_code, callback) {
    # Reset pagination to start from beginning
    last_plot_id(NULL)

    search_for_accession <- function() {
      current_data <- rv_data()

      # Check if accession code is in current page
      if (!is.null(current_data)) {
        idx <- which(current_data$obsaccessioncode == accession_code)
        if (length(idx) > 0) {
          # Found it - execute callback
          callback(idx)
          return(TRUE)
        }
      }

      # Not in current page, try next page if available
      if (has_more_data()) {
        fetch_table_data(prev_plot_id = last_plot_id())
        # Continue searching in next iteration
        return(FALSE)
      } else {
        # Exhausted all pages and didn't find it
        message("Accession code not found in any page: ", accession_code)
        return(TRUE)
      }
    }

    # Initial search
    found <- search_for_accession()

    # If not found, continue checking with a timer
    if (!found) {
      search_timer <- shiny::reactiveTimer(500)

      shiny::observe({
        search_timer()
        found <- search_for_accession()
        if (found) {
          # Stop this observer once found
          shiny::observeEvent.priority <- -1
        }
      })
    }
  }

  # Render UI Outputs ____________________________________________________________________________
  output$dataSummary <- shiny::renderUI({
    htmltools::tags$p(paste0("Vegbank is a database of vegetation plot data. The data displayed in
            this app is a subset of the full dataset, containing ", nrow(rv_data()), " randomly
            selectedplots. Each row in the table and link in a map label represents a plot.
            Clicking on arow in the table or a link in the map will display detailed information
            about the plot, including information about the plot location, species observed, and
            other details."))
  })

  output$topPlaces <- shiny::renderPlot({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    build_top10_barchart(data, "plotstateprovince", "Place", "#4F8773")
  })

  output$topSpecies <- shiny::renderPlot({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    build_top10_barchart(data, "toptaxon1name", "Species", "#6AA26E")
  })

  output$authorPie <- plotly::renderPlotly({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    build_pie_chart(data, "interp_current_partyname")
  })

  output$mostRecentUploads <- shiny::renderUI({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    build_most_recent_date_list(data)
  })

  output$plotHeatmap <- shiny::renderPlot({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    build_plot_heatmap(data)
  })

  output$dataTable <- DT::renderDataTable({
    data <- rv_data()
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame("No Data Available" = "Please try again or check your connection"),
        options = list(dom = "t")
      ))
    }

    n_rows <- nrow(data)

    taxa_lists <- tryCatch(
      {
        apply(data, 1, build_taxa_list)
      },
      error = function(e) {
        message("Error generating taxa lists: ", e$message)
        rep("Error loading taxa", n_rows)
      }
    )

    # Provide fallback vectors if columns are missing
    author_codes <- if (!is.null(data$authorobscode)) data$authorobscode else rep("Unknown", n_rows)
    locations <- if (!is.null(data$stateprovince)) data$stateprovince else rep("Unknown", n_rows)
    communities <- if (!is.null(data$commname)) data$commname else rep("Unknown", n_rows)

    # Ensure 'actions' matches the row count
    actions <- mapply(build_action_buttons, seq_len(n_rows))

    display_data <- data.frame(
      "Actions" = actions,
      "Author Plot Code" = author_codes,
      "Location" = locations,
      "Top Taxa" = taxa_lists,
      "Community" = communities,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    DT::datatable(display_data,
      rownames = FALSE,
      escape = FALSE,
      selection = list(mode = "single", target = "row", selectable = FALSE),
      options = list(
        dom = "ft",
        pageLength = 100,
        scrollY = "calc(100vh - 300px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = c(0), width = "10%"), # Actions
          list(targets = c(1), width = "10%"), # Author Plot Code
          list(targets = c(2), width = "10%"), # Location
          list(targets = c(3), width = "45%"), # Top Taxa
          list(targets = c(4), width = "20%") # Community
        )
      )
    )
  })

  # Create pagination UI
  output$tablePagination <- shiny::renderUI({
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
  })

  # Display pagination status
  output$paginationStatus <- shiny::renderText({
    data <- rv_data()
    if (is.null(data)) {
      return("No data available")
    }

    paste0(
      "Showing ", nrow(data), " records",
      if (has_more_data()) " (more available)" else " (end of data)"
    )
  })

  # Pagination controls
  shiny::observeEvent(input$prevPage, {
    # Go back to first page
    last_plot_id(NULL)
    fetch_table_data()
  })

  shiny::observeEvent(input$nextPage, {
    if (has_more_data()) {
      fetch_table_data(prev_plot_id = last_plot_id())
    }
  })

  output$map <- leaflet::renderLeaflet({
    shiny::withProgress(message = "Loading map data...", value = 0, {
      shiny::incProgress(0.2, detail = "Fetching pins")

      # Timer for map data
      start_time_map <- Sys.time()
      map_data <- tryCatch(
        {
          message("Attempting to fetch map points from endpoint")
          resp <- httr::GET("http://127.0.0.1:28015/get_map_points")
          message("API response status: ", resp$status_code)
          raw_content <- httr::content(resp, "text")
          message("Response content length: ", nchar(raw_content))
          if (nchar(raw_content) == 0) {
            warning("Empty response from map endpoint")
            NULL
          } else if (!jsonlite::validate(raw_content)) {
            warning("Invalid JSON content from map endpoint")
            NULL
          } else {
            pins <- jsonlite::fromJSON(raw_content)
            message("Total pins fetched: ", nrow(pins))
            pins <- subset(pins, !is.na(pins$latitude) & !is.na(pins$longitude))
            message("Total valid pins: ", nrow(pins))
            pins
          }
        },
        error = function(e) {
          message("Error in API call: ", e$message)
          warning("Error fetching map data: ", e$message)
          NULL
        }
      )
      end_time_map <- Sys.time()
      message(paste(
        "Map data retrieval took",
        round(difftime(end_time_map, start_time_map, units = "secs"), 2), "seconds"
      ))

      shiny::incProgress(0.6, detail = "Processing map data")
      result_map <- if (is.null(map_data) || nrow(map_data) == 0) {
        message("No map data available, showing empty map")
        leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) |>
          leaflet::setMaxBounds(
            lng1 = -180,
            lat1 = -85,
            lng2 = 180,
            lat2 = 85
          ) |>
          leaflet::addTiles() |>
          leaflet::addControl("Data unavailable", position = "topright")
      } else {
        # TODO: Can this be done in the API instead of client side?
        message("Grouping labels for ", nrow(map_data), " map points")
        data_grouped <- map_data |>
          dplyr::arrange(.data$authorobscode) |>
          dplyr::group_by(.data$latitude, .data$longitude) |>
          dplyr::summarize(
            authorobscode_label = paste0(
              "<div style='max-height: 15.5rem; overflow-y: auto;'
              onwheel='event.stopPropagation()'
              onmousewheel='event.stopPropagation()'
              onDOMMouseScroll='event.stopPropagation()'>",
              paste(
                mapply(function(obs, acc) {
                  sprintf(
                    "<a
                    href=\"#\"
                    onclick=\"Shiny.setInputValue('label_link_click', '%s', {priority: 'event'})\"
                    >%s</a>",
                    acc, obs
                  )
                }, .data$authorobscode, .data$accessioncode),
                collapse = "<br>"
              ),
              "</div>"
            ),
            .groups = "drop"
          )

        message("Total grouped labels: ", nrow(data_grouped))

        shiny::incProgress(0.9, detail = "Rendering map")
        leaflet::leaflet(data_grouped, options = leaflet::leafletOptions(minZoom = 2)) |>
          leaflet::setMaxBounds(
            lng1 = -180,
            lat1 = -85,
            lng2 = 180,
            lat2 = 85
          ) |>
          leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 2) |>
          leaflet::addTiles() |>
          leaflet::addMarkers(
            lng = ~longitude,
            lat = ~latitude,
            layerId = ~ paste(latitude, longitude, sep = ", "),
            label = ~ authorobscode_label |> lapply(htmltools::HTML),
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              clickable = TRUE,
              direction = "bottom",
              style = list(
                "color" = "#2c5443",
                "font-weight" = "bold",
                "padding" = "3px 8px",
                "background" = "white",
                "border" = "1px solid #2c5443",
                "border-radius" = "3px"
              )
            ),
            clusterOptions = leaflet::markerClusterOptions()
          ) |>
          htmlwidgets::onRender("
            function(el, x) {
              var map = this;
              var zoomControl = L.control({position: 'bottomleft'});
              zoomControl.onAdd = function(map) {
                var div = L.DomUtil.create('div', 'zoom-control');
                div.style.background = 'white';
                div.style.padding = '5px';
                div.style.border = '1px solid #ccc';
                div.innerHTML = 'Zoom: ' + map.getZoom();
                return div;
              };
              zoomControl.addTo(map);
              map.on('zoomend', function() {
                document.getElementsByClassName('zoom-control')
                  [0].innerHTML = 'Zoom: ' + map.getZoom();
                Shiny.setInputValue('map_zoom', map.getZoom());
              });
            }
          ")
      }

      # Final progress update right before returning
      shiny::incProgress(1, detail = "Done")
      return(result_map)
    })
  })

  # Handle Events _________________________________________________________________________________

  # TODO: Parameterize this to pull out of server fn?
  update_and_open_details <- function(accession_code) {
    shiny::withProgress(message = "Loading details...", value = 0, {
      # Get details from API
      api_url <- paste0("http://127.0.0.1:28015/get_observation_details/", accession_code)

      # Timer for details
      start_time_details <- Sys.time()
      response <- tryCatch(
        {
          message("Fetching details for ", accession_code)
          httr::GET(api_url)
        },
        error = function(e) {
          message("Error connecting to API: ", e$message)
          NULL
        }
      )
      end_time_details <- Sys.time()
      message(paste(
        "Details retrieval took",
        round(difftime(end_time_details, start_time_details, units = "secs"), 2), "seconds"
      ))

      if (!is.null(response) && httr::status_code(response) == 200) {
        raw_content <- httr::content(response, "text")
        if (nchar(raw_content) > 0 && jsonlite::validate(raw_content)) {
          selected_data <- jsonlite::fromJSON(raw_content)

          # Build details from API response
          details <- build_details_view(selected_data)
          output$plot_id_details <- details$plot_id_details
          output$locationDetails <- details$location_details
          output$layout_details <- details$layout_details
          output$environmental_details <- details$environmental_details
          output$methods_details <- details$methods_details
          output$plot_quality_details <- details$plot_quality_details
          output$taxaDetails <- details$taxa_details

          # Set the selected accession code
          selected_accession(accession_code)
          details_open(TRUE)
          shiny::incProgress(1, detail = "Done")
          session$sendCustomMessage("openOverlay", list())
        } else {
          message("Invalid JSON response from API")
          shiny::showNotification("Error loading details: Invalid data format", type = "error")
        }
      } else {
        status <- if (!is.null(response)) httr::status_code(response) else "connection failed"
        message("API Error: Status ", status)
        shiny::showNotification("Failed to load details. Please try again.", type = "error")
      }
    })
  }

  # TODO: Parameterize this to pull out of server fn?
  update_map_view <- function(idx) {
    data <- rv_data()
    leaflet::leafletProxy("map", session) |>
      leaflet::setView(
        lng = data$longitude[idx],
        lat = data$latitude[idx],
        zoom = 18
      ) |>
      leaflet::clearPopups() |>
      leaflet::addPopups(
        lng = data$longitude[idx],
        lat = data$latitude[idx],
        popup = paste("Plot", data$authorobscode[idx], "is here!", sep = " ")
      )
  }

  # Handle map_request
  shiny::observe({
    shiny::req(input$page == "Map")
    shiny::req(map_request())

    data <- rv_data()
    idx <- map_request()

    if (length(idx) > 0) {
      shiny::invalidateLater(100) # Small delay to ensure map is ready
      update_map_view(idx)
      map_request(NULL) # Clear the request
    }
  })

  # Handle see details button click
  shiny::observeEvent(input$see_details, {
    i <- as.numeric(input$see_details)
    dt_proxy <- DT::dataTableProxy("dataTable")
    DT::selectRows(dt_proxy, i, ignore.selectable = TRUE)
    data_table <- rv_data()
    selected_row_accession <- data_table[i, "obsaccessioncode"]
    update_and_open_details(selected_row_accession)
  })

  # Handle close details button click
  shiny::observeEvent(input$close_details, {
    data <- rv_data()
    idx <- which(data$obsaccessioncode == selected_accession())
    if (length(idx) > 0) {
      dt_proxy <- DT::dataTableProxy("dataTable")
      # Wish there was a better way to do this, but I canʻt find a deleselect function anywhere
      DT::selectRows(dt_proxy, idx, ignore.selectable = FALSE)
    }
    details_open(FALSE)
    selected_accession(NULL)
    session$doBookmark()
  })

  # Handle show on map button click
  shiny::observeEvent(input$show_on_map, {
    idx <- as.numeric(input$show_on_map)
    shiny::updateNavbarPage(session, "page", selected = "Map")
    map_request(idx) # Set the request instead of updating map directly
  })

  # Handle label see details link click
  shiny::observeEvent(input$label_link_click, {
    accession_code <- input$label_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      update_and_open_details(accession_code)
    }
  })

  # TODO: Show all rows on empty search
  # Handle nav bar search submission
  shiny::observeEvent(input$search_enter, {
    shiny::updateNavbarPage(session, "page", selected = "Table")

    # In a production app, you would send the search term to the API
    # For now, reset to first page and search locally (less efficient)
    last_plot_id(NULL)
    fetch_table_data()

    # Future enhancement: Call a search-specific API endpoint
    # search_api_url <- paste0("http://127.0.0.1:28015/search_observations/",
    #                          URLencode(input$search_enter))
  })

  # Persist State _________________________________________________________________________________

  shiny::onBookmark(function(state) {
    state$values$selected_accession <- selected_accession()
    state$values$details_open <- details_open()
    state
  })

  shiny::onBookmarked(function(url) {
    shiny::updateQueryString(url)
  })

  # TODO: Restore map, search, and row selection state as well
  shiny::onRestore(function(state) {
    if (!is.null(state$values$selected_accession)) {
      acc <- state$values$selected_accession
      shiny::observeEvent(rv_data(),
        {
          update_and_open_details(acc)
          # Select the row in the table if possible
          data <- rv_data()
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

# Helper Functions _________________________________________________________________________________

#' Build Top Ten Bar Chart
#'
#' Constructs a ggplot bar chart based on the top 10 counts for a specified column.
#'
#' @param data Data frame containing the data.
#' @param column Name of the column to summarize.
#' @param xlab Label for the x-axis.
#' @param color Bar fill color.
#' @return A ggplot object.
#' @keywords internal
#' @importFrom ggplot2 .data
build_top10_barchart <- function(data, column, xlab, color) {
  counts <- table(data[[column]])
  df <- as.data.frame(counts)
  colnames(df) <- c("name", "count")
  df <- df[order(df$count, decreasing = TRUE), ]
  top_df <- utils::head(df, 10)
  ggplot2::ggplot(
    top_df,
    ggplot2::aes(
      x = stats::reorder(.data$name, .data$count),
      y = .data$count
    )
  ) +
    ggplot2::geom_bar(stat = "identity", fill = color) +
    ggplot2::geom_text(ggplot2::aes(label = .data$count), hjust = -0.1, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::labs(x = xlab, y = "Plot Occurrences") +
    ggplot2::theme_minimal()
}

#' Build Pie Chart
#'
#' Constructs an interactive pie chart using plotly.
#'
#' @param data Data frame containing the data.
#' @param field The column name used for pie chart slices.
#' @param palette Color palette.
#' @param label_top_n Number of top labels to display.
#' @return A plotly pie chart.
#' @keywords internal
build_pie_chart <- function(data, field, palette = c("#a1d99b", "#31a354"), label_top_n = 4) {
  counts <- as.data.frame(table(data[[field]]))
  colnames(counts) <- c("name", "value")
  counts$name <- as.character(counts$name)
  counts <- counts[order(-counts$value), ]
  counts$label <- ""
  counts$label[seq_len(min(label_top_n, nrow(counts)))] <- paste0(
    counts$name[seq_len(min(label_top_n, nrow(counts)))],
    ": ",
    counts$value[seq_len(min(label_top_n, nrow(counts)))]
  )
  colors <- grDevices::colorRampPalette(palette)(nrow(counts))
  plotly::plot_ly(
    counts,
    labels = ~name, values = ~value, type = "pie",
    text = ~label,
    textinfo = "text",
    insidetextorientation = "radial",
    marker = list(colors = colors)
  ) |>
    plotly::config(responsive = TRUE)
}

#' Build Most Recent Date List
#'
#' Creates a list of the most recent dates from the data.
#'
#' @param data Data frame containing date information.
#' @param n Maximum number of dates to display.
#' @param date_field Name of the date field.
#' @return A Shiny UI tag containing an unordered list.
#' @keywords internal
build_most_recent_date_list <- function(data, n = 16, date_field = "obsdateentered") {
  dates_df <- data.frame(
    original = data[[date_field]],
    parsed = lubridate::parse_date_time(
      data[[date_field]],
      orders = c("a, d b Y H:M:S z", "d b Y H:M:S", "Y-m-d H:M:S")
    )
  )
  dates_df <- dates_df[!is.na(dates_df$parsed), ]
  dates_df <- dates_df[!duplicated(dates_df$parsed), ]
  top_dates <- utils::head(dates_df[order(dates_df$parsed, decreasing = TRUE), ], n)
  items <- lapply(top_dates$original, function(d) {
    htmltools::tags$li(class = "list-unstyled", htmltools::tags$strong(d))
  })
  htmltools::tags$ul(items)
}

#' Build Plot Heatmap
#'
#' Constructs a heatmap (using ggplot2) of plot locations using density estimation.
#'
#' @param data Data frame containing longitude and latitude.
#' @return A ggplot object.
#' @keywords internal
#' @importFrom ggplot2 .data
build_plot_heatmap <- function(data) {
  na_map <- ggplot2::map_data("world", region = c("USA", "Canada", "Mexico"))
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = na_map,
      ggplot2::aes(
        x = .data$long,
        y = .data$lat, group = .data$group
      ),
      fill = "white", color = "gray70", linewidth = 0.3
    ) +
    ggplot2::stat_density2d(
      data = data,
      ggplot2::aes(
        x = .data$longitude,
        y = .data$latitude,
        fill = ggplot2::after_stat(.data$level)
      ),
      geom = "polygon", color = "black", linewidth = 0.5, contour = TRUE
    ) +
    ggplot2::scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "white") +
    ggplot2::coord_fixed(1.3) +
    ggplot2::xlim(-200, -50) +
    ggplot2::labs(title = "Plot Heatmap", x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal()
}

#' Build Taxa List
#'
#' Generates an HTML ordered list of taxa from a data row.
#'
#' @param data_row A row of data that contains taxa information.
#' @return A character string containing HTML markup.
#' @keywords internal
build_taxa_list <- function(data_row) {
  tryCatch(
    {
      # Access the taxa data as a data frame from jsonlite::fromJSON
      taxa <- data_row[["taxa"]]
      if (!is.null(taxa) && nrow(taxa) > 0) {
        # Sort by cover in descending order
        sorted_taxa <- taxa[order(-taxa$maxcover), ]
        top5 <- utils::head(sorted_taxa, 5)
        taxa_items <- sprintf("<li>%s <b>(%g%%)</b></li>", top5$authorplantname, top5$maxcover)
        paste("<ol>", paste(taxa_items, collapse = "\n"), "</ol>", sep = "\n")
      } else {
        "No taxa recorded"
      }
    },
    error = function(e) {
      print(paste("Error in build_taxa_list:", e$message))
      "Error processing taxa"
    }
  )
}

#' Build Action Buttons
#'
#' Creates action buttons for viewing details and mapping a plot.
#'
#' @param i Index used to uniquely identify buttons.
#' @return A character string containing the button HTML.
#' @keywords internal
build_action_buttons <- function(i) {
  as.character(
    htmltools::tagList(
      shiny::actionButton(
        inputId = paste0("see_details_", i),
        label = "See Details",
        class = "btn btn-info btn-sm details-btn mb-1",
        onclick = sprintf("Shiny.setInputValue('see_details', %d, {priority: 'event'})", i)
      ),
      shiny::actionButton(
        inputId = paste0("map_btn_", i),
        label = "Show on Map",
        class = "btn btn-primary btn-sm map-btn",
        onclick = sprintf("Shiny.setInputValue('show_on_map', '%s', {priority: 'event'})", i)
      )
    )
  )
}

#' Build Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed plot information.
#'
#' @param selected_data A data row representing the selected plot.
#' @return A list of Shiny UI outputs.
#' @keywords internal
build_details_view <- function(selected_data) {
  # Mapping internal field names to their display names.
  display_names <- list(
    authorplotcode = "Author Plot Code",
    authorobscode = "Author Observation Code",
    area = "Area",
    permanence = "Permanent",
    elevation = "Elevation",
    slopeaspect = "Slope Aspect",
    slopegradient = "Slope Gradient",
    confidentialitystatus = "Confidentiality Status",
    confidentialitytext = "Confidentiality Status",
    latitude = "Latitude",
    longitude = "Longitude",
    locationnarrative = "Location Description",
    stateprovince = "State/Province",
    country = "Country",
    obsstartdate = "Observation Start Date",
    project_id = "Project ID",
    projectname = "Project Name",
    covermethod_id = "Cover Method ID",
    covertype = "Cover Type",
    stratummethod_id = "Stratum Method ID",
    stratummethodname = "Stratum Method",
    stratummethoddescription = "Stratum Method Description",
    taxonobservationarea = "Taxon Observation Area",
    autotaxoncover = "Taxon Cover Automatically Calculated",
    plotvalidationlevel = "Plot Validation Level",
    plotvalidationleveldescr = "Validation Level"
  )

  create_table <- function(details, col_names) {
    htmltools::tags$table(
      class = "table table-sm table-striped table-hover",
      htmltools::tags$tbody(
        lapply(names(details), function(name) {
          display_name <- if (!is.null(col_names[[name]])) col_names[[name]] else name
          htmltools::tags$tr(
            htmltools::tags$td(htmltools::tags$strong(display_name)),
            htmltools::tags$td(class = "text-end", details[[name]])
          )
        })
      )
    )
  }

  safe_render_details <- function(fields) {
    shiny::renderUI({
      values <- lapply(selected_data[fields], function(x) {
        if (is.null(x) || all(is.na(x))) "Not recorded" else x
      })
      create_table(values, col_names = display_names)
    })
  }

  # Render taxa details with error handling.
  taxa_details_ui <- shiny::renderUI({
    tryCatch(
      {
        taxa <- selected_data[["taxa"]]
        if (is.null(taxa)) {
          return("No taxa recorded")
        }
        if (!is.data.frame(taxa)) taxa <- as.data.frame(taxa)
        if (nrow(taxa) == 0) {
          return("No taxa recorded")
        }

        taxa$cover <- as.numeric(taxa$cover)
        sorted_taxa <- taxa[order(-taxa$cover), ]
        rows <- lapply(seq_len(nrow(sorted_taxa)), function(i) {
          row <- sorted_taxa[i, ]
          htmltools::tags$tr(
            htmltools::tags$td(row$authorplantname),
            htmltools::tags$td(style = "text-align: right;", sprintf("%.2f%%", row$cover))
          )
        })
        htmltools::tags$table(
          class = "table table-sm table-striped table-hover",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th("Author Plant Name"),
              htmltools::tags$th("Cover")
            )
          ),
          htmltools::tags$tbody(rows)
        )
      },
      error = function(e) {
        paste("Error processing taxa:", e$message)
      }
    )
  })

  list(
    plot_id_details = safe_render_details(c("authorobscode", "authorplotcode")),
    location_details = safe_render_details(c(
      "confidentialitytext", "latitude", "longitude",
      "locationnarrative", "stateprovince", "country"
    )),
    layout_details = safe_render_details(c("area", "permanence")),
    environmental_details = safe_render_details(c("elevation", "slopeaspect", "slopegradient")),
    methods_details = safe_render_details(c(
      "obsstartdate", "projectname", "covertype",
      "stratummethodname", "stratummethoddescription",
      "taxonobservationarea", "autotaxoncover"
    )),
    plot_quality_details = safe_render_details("plotvalidationleveldescr"),
    taxa_details = taxa_details_ui
  )
}
