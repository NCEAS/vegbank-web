#' Shiny Server for Vegbank Web Application
#'
#' Initializes server-side functionality for data visualization and interactivity.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return Called for its side effects.
#' @importFrom ggplot2 .data

# ========================== API CLIENT MODULE ==========================

VegBankAPI <- (function() {
  base_url <- "http://127.0.0.1:28015"

  fetch <- function(path, ...) {
    api_url <- paste0(base_url, path)
    message("Fetching from API: ", api_url)
    start_time <- Sys.time()

    response <- tryCatch(
      {
        httr::GET(api_url, ...)
      },
      error = function(e) {
        message("Error connecting to API: ", e$message)
        NULL
      }
    )

    end_time <- Sys.time()
    message(paste("API call took", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

    return(response)
  }

  process_response <- function(response) {
    if (is.null(response)) {
      return(list(success = FALSE, data = NULL, status = "connection failed"))
    }

    status_code <- httr::status_code(response)
    if (status_code != 200) {
      return(list(success = FALSE, data = NULL, status = status_code))
    }

    raw_content <- httr::content(response, "text")
    if (nchar(raw_content) == 0 || !jsonlite::validate(raw_content)) {
      return(list(success = FALSE, data = NULL, status = "invalid json"))
    }

    data <- jsonlite::fromJSON(raw_content)
    return(list(success = TRUE, data = data, status = status_code))
  }

  list(
    get_table_data = function(page_size, prev_plot_id = NULL) {
      endpoint <- if (is.null(prev_plot_id)) {
        paste0("/get_observation_table/", page_size)
      } else {
        paste0("/get_observation_table/", page_size, "/", prev_plot_id)
      }

      response <- fetch(endpoint)
      return(process_response(response))
    },
    get_map_points = function() {
      response <- fetch("/get_map_points")
      return(process_response(response))
    },
    get_observation_details = function(accession_code) {
      response <- fetch(paste0("/get_observation_details/", accession_code))
      return(process_response(response))
    }
  )
})()

# ========================== PROGRESS HANDLER ==========================

# Simplified progress handler that works properly with Shiny
make_progress <- function(message) {
  function(code) {
    shiny::withProgress(
      message = message,
      value = 0,
      {
        # Create a local environment to track progress
        env <- new.env()
        env$current <- 0

        # Function to increment progress
        step <- function(amount, detail = NULL) {
          env$current <- min(1, env$current + amount)
          shiny::incProgress(amount = amount, detail = detail)
        }

        # Function to complete progress
        complete <- function(detail = "Done") {
          remaining <- 1 - env$current
          if (remaining > 0) {
            shiny::incProgress(amount = remaining, detail = detail)
          }
        }

        # Execute the provided code with access to progress functions
        result <- code(step, complete)

        # Ensure progress completes
        if (env$current < 1) {
          complete()
        }

        return(result)
      }
    )
  }
}

# ========================== MAP FUNCTIONS MODULE ==========================

MapModule <- (function() {
  create_empty_map <- function() {
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) |>
      leaflet::setMaxBounds(
        lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85
      ) |>
      leaflet::addTiles() |>
      leaflet::addControl("Data unavailable", position = "topright")
  }

  create_marker_popup <- function(obs_codes, accession_codes, count) {
    paste0(
      "<strong>",
      ifelse(count == 1, "1 Observation", paste(count, "Observations")),
      "</strong>",
      "<div style='max-height: 15.5rem; overflow-y: auto;'
        onwheel='event.stopPropagation()'
        onmousewheel='event.stopPropagation()'
        onDOMMouseScroll='event.stopPropagation()'>",
      paste(
        mapply(
          function(obs, acc) {
            sprintf(
              "<a href=\"#\" onclick=\"Shiny.setInputValue('label_link_click', '%s', {priority: 'event'})\">%s</a>",
              acc, obs
            )
          },
          obs_codes, accession_codes
        ),
        collapse = "<br>"
      ),
      "</div>"
    )
  }

  add_zoom_control <- function(map) {
    map |> htmlwidgets::onRender("
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
          document.getElementsByClassName('zoom-control')[0].innerHTML = 'Zoom: ' + map.getZoom();
          Shiny.setInputValue('map_zoom', map.getZoom());
        });
      }
    ")
  }

  process_map_data <- function(map_data) {
    if (is.null(map_data) || nrow(map_data) == 0) {
      return(create_empty_map())
    }

    # Filter valid points and group by location
    valid_points <- subset(map_data, !is.na(map_data$latitude) & !is.na(map_data$longitude))
    message("Total valid pins: ", nrow(valid_points))

    data_grouped <- valid_points |>
      dplyr::arrange(.data$authorobscode) |>
      dplyr::group_by(.data$latitude, .data$longitude) |>
      dplyr::summarize(
        obs_count = dplyr::n(),
        authorobscode_label = create_marker_popup(
          .data$authorobscode,
          .data$accessioncode,
          dplyr::n()
        ),
        .groups = "drop"
      )

    message("Total grouped labels: ", nrow(data_grouped))

    # Create and return the map
    leaflet::leaflet(data_grouped, options = leaflet::leafletOptions(minZoom = 2)) |>
      leaflet::setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85) |>
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
      add_zoom_control()
  }

  list(
    process_map_data = process_map_data,
    create_empty_map = create_empty_map,
    update_map_view = function(map_proxy, lng, lat, label, zoom = 18) {
      map_proxy |>
        leaflet::setView(lng = lng, lat = lat, zoom = zoom) |>
        leaflet::clearPopups() |>
        leaflet::addPopups(
          lng = lng,
          lat = lat,
          popup = label
        )
    }
  )
})()

# ================= MAIN SERVER FUNCTION =================

server <- function(input, output, session) {
  # STATE MANAGEMENT
  state <- list(
    data = shiny::reactiveVal(NULL),
    selected_accession = shiny::reactiveVal(NULL),
    details_open = shiny::reactiveVal(FALSE),
    map_request = shiny::reactiveVal(NULL),
    pagination = list(
      current_page_size = shiny::reactiveVal(100),
      last_plot_id = shiny::reactiveVal(NULL),
      has_more_data = shiny::reactiveVal(TRUE)
    )
  )

  # HELPER FUNCTIONS
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

    return(data)
  }

  # DATA OPERATIONS
  fetch_table_data <- function(page_size = NULL, prev_plot_id = NULL, force_refresh = FALSE) {
    make_progress("Loading table data...")(function(step, complete) {
      page_size <- if (is.null(page_size)) state$pagination$current_page_size() else page_size
      step(0.2, "Fetching data")

      result <- VegBankAPI$get_table_data(page_size, prev_plot_id)

      if (!result$success) {
        step(0.2, "Error loading data")
        shiny::showNotification("Failed to load data. Please try again.", type = "error")
        return(FALSE)
      }

      step(0.4, "Processing data")
      data <- update_pagination_state(result$data, page_size)
      state$data(data)

      complete("Done")
      return(TRUE)
    })
  }

  fetch_map_data <- function() {
    make_progress("Loading map data...")(function(step, complete) {
      step(0.2, "Fetching pins")

      result <- VegBankAPI$get_map_points()

      if (!result$success) {
        step(0.3, "Error loading map data")
        return(MapModule$create_empty_map())
      }

      step(0.5, "Processing map data")
      map <- MapModule$process_map_data(result$data)

      complete("Map rendered")
      return(map)
    })
  }

  update_and_open_details <- function(accession_code) {
    make_progress("Loading details...")(function(step, complete) {
      step(0.2, "Fetching details")

      result <- VegBankAPI$get_observation_details(accession_code)

      if (!result$success) {
        step(0.3, "Error loading details")
        shiny::showNotification("Failed to load details. Please try again.", type = "error")
        return(FALSE)
      }

      step(0.5, "Processing details")

      details <- build_details_view(result$data)
      output$plot_id_details <- details$plot_id_details
      output$locationDetails <- details$location_details
      output$layout_details <- details$layout_details
      output$environmental_details <- details$environmental_details
      output$methods_details <- details$methods_details
      output$plot_quality_details <- details$plot_quality_details
      output$taxaDetails <- details$taxa_details

      state$selected_accession(accession_code)
      state$details_open(TRUE)

      complete("Details ready")
      session$sendCustomMessage("openOverlay", list())

      return(TRUE)
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
          return(TRUE)
        }
      }

      if (state$pagination$has_more_data()) {
        fetch_table_data(prev_plot_id = state$pagination$last_plot_id())
        return(FALSE)
      } else {
        message("Accession code not found in any page: ", accession_code)
        return(TRUE)
      }
    }

    found <- search_for_accession()

    if (!found) {
      search_timer <- shiny::reactiveTimer(500)
      shiny::observe({
        search_timer()
        found <- search_for_accession()
        if (found) {
          shiny::observeEvent.priority <- -1
        }
      })
    }
  }

  update_map_view <- function(idx) {
    data <- state$data()
    leaflet::leafletProxy("map", session) |>
      MapModule$update_map_view(
        data$longitude[idx],
        data$latitude[idx],
        paste("Plot", data$authorobscode[idx], "is here!")
      )
  }

  # LOAD INITIAL DATA
  shiny::observe({
    fetch_table_data()
  })

  # RENDER UI ELEMENTS
  output$dataSummary <- shiny::renderUI({
    htmltools::tags$p(paste0(
      "Vegbank is a database of vegetation plot data. The data displayed in ",
      "this app is a subset of the full dataset, containing ", nrow(state$data()), " randomly ",
      "selectedplots. Each row in the table and link in a map label represents a plot. ",
      "Clicking on arow in the table or a link in the map will display detailed information ",
      "about the plot, including information about the plot location, species observed, and ",
      "other details."
    ))
  })

  # output$topPlaces <- shiny::renderPlot({
  #   data <- state$data()
  #   if (is.null(data)) {
  #     return(NULL)
  #   }
  #   build_top10_barchart(data, "plotstateprovince", "Place", "#4F8773")
  # })

  # output$topSpecies <- shiny::renderPlot({
  #   data <- state$data()
  #   if (is.null(data)) {
  #     return(NULL)
  #   }
  #   build_top10_barchart(data, "toptaxon1name", "Species", "#6AA26E")
  # })

  # output$authorPie <- plotly::renderPlotly({
  #   data <- state$data()
  #   if (is.null(data)) {
  #     return(NULL)
  #   }
  #   build_pie_chart(data, "interp_current_partyname")
  # })

  # output$mostRecentUploads <- shiny::renderUI({
  #   data <- state$data()
  #   if (is.null(data)) {
  #     return(NULL)
  #   }
  #   build_most_recent_date_list(data)
  # })

  # output$plotHeatmap <- shiny::renderPlot({
  #   data <- state$data()
  #   if (is.null(data)) {
  #     return(NULL)
  #   }
  #   build_plot_heatmap(data)
  # })

  output$dataTable <- DT::renderDataTable({
    data <- state$data()
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
          list(targets = c(0), width = "10%"),
          list(targets = c(1), width = "10%"),
          list(targets = c(2), width = "10%"),
          list(targets = c(3), width = "45%"),
          list(targets = c(4), width = "20%")
        )
      )
    )
  })

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

  output$paginationStatus <- shiny::renderText({
    data <- state$data()
    if (is.null(data)) {
      return("No data available")
    }

    paste0(
      "Showing ", nrow(data), " records",
      if (state$pagination$has_more_data()) " (more available)" else " (end of data)"
    )
  })

  output$map <- leaflet::renderLeaflet({
    fetch_map_data()
  })

  # EVENT HANDLERS
  shiny::observeEvent(input$prevPage, {
    state$pagination$last_plot_id(NULL)
    fetch_table_data()
  })

  shiny::observeEvent(input$nextPage, {
    if (state$pagination$has_more_data()) {
      fetch_table_data(prev_plot_id = state$pagination$last_plot_id())
    }
  })

  shiny::observe({
    shiny::req(input$page == "Map")
    shiny::req(state$map_request())

    data <- state$data()
    idx <- state$map_request()

    if (length(idx) > 0) {
      shiny::invalidateLater(100)
      update_map_view(idx)
      state$map_request(NULL)
    }
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
    session$doBookmark()
  })

  shiny::observeEvent(input$show_on_map, {
    idx <- as.numeric(input$show_on_map)
    shiny::updateNavbarPage(session, "page", selected = "Map")
    state$map_request(idx)
  })

  shiny::observeEvent(input$label_link_click, {
    accession_code <- input$label_link_click
    if (!is.null(accession_code) && nchar(accession_code) > 0) {
      update_and_open_details(accession_code)
    }
  })

  shiny::observeEvent(input$search_enter, {
    shiny::updateNavbarPage(session, "page", selected = "Table")
    state$pagination$last_plot_id(NULL)
    fetch_table_data()
  })

  # STATE PERSISTENCE
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

# =================== HELPER FUNCTIONS ===================

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
      taxa <- data_row[["taxa"]]
      if (!is.null(taxa) && nrow(taxa) > 0) {
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
