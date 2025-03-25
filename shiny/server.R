# TODO: What does dependency managment look like in R?
library(shiny)
library(leaflet)
library(magrittr)
library(DT)
library(ggplot2) # added for heatmap rendering
library(maps) # added to render world map boundaries
library(plotly) # added for interactive pie chart

server <- function(input, output, session) {
  rv_data <- reactiveVal(NULL)
  selected_accession <- reactiveVal(NULL)
  details_open <- reactiveVal(FALSE)
  map_request <- reactiveVal(NULL)

  observe({
    file_path <- "/Users/dariangill/git/vegbank-web/shiny/www/all_states_plot_obs.json"
    if (file.exists(file_path)) {
      tryCatch(
        {
          file_content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
          data <- jsonlite::fromJSON(file_content)
          rv_data(data)
        },
        error = function(e) {
          message("Error fetching data: ", e)
          rv_data(NULL)
        }
      )
    } else {
      message("Error fetching data: File not found: ", file_path)
      rv_data(NULL)
    }
  })

  # Render UI Outputs ____________________________________________________________________________
  output$dataSummary <- renderUI({
    tags$p(paste0("Vegbank is a database of vegetation plot data. The data displayed in this
            app is a subset of the full dataset, containing ", nrow(rv_data()), " randomly selected
            plots. Each row in the table represents a plot, and the columns contain information
            about the plot location, species observed, and observer details. This is a simple Shiny
            app that demonstrates how to use bookmarking to save the state of the app in the URL.
            The app displays a table of data, a map, and a detailed view of each row in the table.
            You can search for specific rows using the search bar in the navbar or on the table
            page."))
  })

  # Helper function: build top 10 bar chart
  build_top10_barchart <- function(data, column, xlab, color) {
    counts <- table(data[[column]])
    df <- as.data.frame(counts)
    colnames(df) <- c("name", "count")
    df <- df[order(df$count, decreasing = TRUE), ]
    top_df <- head(df, 10)
    ggplot(top_df, aes(x = reorder(name, count), y = count)) + # nolint: object_usage_linter.
      geom_bar(stat = "identity", fill = color) +
      geom_text(aes(label = count), hjust = -0.1, size = 3) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = xlab, y = "Plot Occurrences") +
      theme_minimal()
  }

  output$topPlaces <- renderPlot({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    build_top10_barchart(data, "plotstateprovince", "Place", "#4F8773")
  })

  output$topSpecies <- renderPlot({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    build_top10_barchart(data, "toptaxon1name", "Species", "#6AA26E")
  })

  output$authorPie <- renderPlotly({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    counts <- as.data.frame(table(data$interp_current_partyname))
    colnames(counts) <- c("author", "plots")
    # Sort by descending plot count
    counts <- counts[order(-counts$plots), ]
    # Create label only for the top five authors
    counts$label <- ""
    counts$label[seq_len(min(4, nrow(counts)))] <- paste0(
      counts$author[seq_len(min(4, nrow(counts)))],
      ": ",
      counts$plots[seq_len(min(4, nrow(counts)))]
    )
    # Generate a palette of distinct green shades
    colors <- colorRampPalette(c("#a1d99b", "#31a354"))(nrow(counts))
    plot_ly(counts,
      labels = ~author, values = ~plots, type = "pie",
      text = ~label,
      textinfo = "text",
      insidetextorientation = "radial",
      marker = list(colors = colors)
    ) %>%
      layout(
        showlegend = TRUE,
        autosize = TRUE,
        margin = list(l = 20, r = 20, b = 20, t = 20)
      ) %>%
      config(responsive = TRUE)
  })

  output$mostRecentUploads <- renderUI({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Parse dates with additional orders for different formats
    dates_df <- data.frame(
      original = data$obsdateentered,
      parsed = lubridate::parse_date_time(
        data$obsdateentered,
        orders = c(
          "a, d b Y H:M:S z", # For "Tue, 27 Feb 2018 18:57:32 GMT"
          "d b Y H:M:S", # Backup without weekday and timezone
          "Y-m-d H:M:S" # ISO format backup
        )
      )
    )

    # Remove any failed parses and get unique dates
    dates_df <- dates_df[!is.na(dates_df$parsed), ]
    dates_df <- dates_df[!duplicated(dates_df$parsed), ]

    # Sort by parsed datetime and get top 10 most recent
    top10 <- head(dates_df[order(dates_df$parsed, decreasing = TRUE), ], 16)

    # Create list items with original date strings
    items <- lapply(top10$original, function(d) {
      tags$li(class = "list-unstyled", tags$strong(d))
    })

    tags$ul(items)
  })

  output$plotHeatmap <- renderPlot({
    data <- rv_data()
    if (is.null(data)) {
      return(NULL)
    }
    na_map <- map_data("world", region = c("USA", "Canada", "Mexico"))
    ggplot() +
      geom_polygon(
        data = na_map, aes(
          x = long, y = lat, group = group # nolint: object_usage_linter.
        ),
        fill = "white", color = "gray70", size = 0.3
      ) +
      stat_density2d(
        data = data, aes(
          x = longitude, y = latitude, fill = after_stat(level) # nolint: object_usage_linter.
        ),
        geom = "polygon", color = "black", linewidth = 0.5, contour = TRUE
      ) +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "white") +
      coord_fixed(1.3) +
      xlim(-200, -50) +
      labs(title = "Plot Heatmap", x = "Longitude", y = "Latitude") +
      theme_minimal()
  })

  output$dataTable <- DT::renderDataTable({
    data <- rv_data()
    actions <- mapply(
      build_action_buttons,
      seq_len(nrow(data))
    )

    display_data <- data.frame(
      "Actions" = actions,
      "Author Plot Code" = data$authorobscode,
      "Location" = data$plotstateprovince,
      "Top Taxa" = apply(data, 1, build_taxa_list),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    DT::datatable(display_data,
      escape = FALSE,
      # TODO: Clicking another row deselects the programtically selected row. It shouldnʻt.
      #       Also probably worth using different styiing so select could be used for data cart.
      selection = list(mode = "single", target = "row", selectable = FALSE),
      options = list(
        pageLength = 100,
        scrollY = "calc(100vh - 300px)",
        scrollX = TRUE,
        scrollCollapse = TRUE
      )
    )
  })

  output$map <- leaflet::renderLeaflet({
    data <- rv_data()

    if (is.null(data)) {
      leaflet() %>%
        addTiles() %>%
        addControl("Data unavailable", position = "topright")
    } else {
      data_grouped <- data %>%
        dplyr::group_by(latitude, longitude) %>% # nolint: object_usage_linter.
        dplyr::mutate(
          authorobscode_label =
            paste(mapply(function(obs, acc) {
              sprintf(
                "<a href=\"#\" onclick=\"Shiny.setInputValue('label_link_click',
                '%s', {priority:'event'})\">%s</a>",
                acc, obs
              )
            }, authorobscode, obsaccessioncode), collapse = "<br>"), # nolint: object_usage_linter.
        ) %>%
        dplyr::ungroup()

      leaflet(data_grouped) %>%
        addTiles() %>%
        addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~ paste(latitude, longitude, sep = ", "),
          # Render the concatenated links
          label = ~ authorobscode_label %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
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
          clusterOptions = markerClusterOptions()
        ) %>%
        # Display Zoom level for debugging purposes
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
  })

  # Handle Events _________________________________________________________________________________

  # TODO: Parameterize this to pull out of server fn?
  update_and_open_details <- function(idx) {
    selected_data <- rv_data()[idx, ]
    details <- build_details_view(selected_data)
    output$plot_id_details <- details$plot_id_details
    output$locationDetails <- details$location_details
    output$layout_details <- details$layout_details
    output$environmental_details <- details$environmental_details
    output$methods_details <- details$methods_details
    output$plot_quality_details <- details$plot_quality_details
    output$taxaDetails <- details$taxa_details
    selected_accession(rv_data()[idx, "obsaccessioncode"])
    details_open(TRUE)
    session$sendCustomMessage("openOverlay", list())
  }

  # TODO: Parameterize this to pull out of server fn?
  update_map_view <- function(idx) {
    data <- rv_data()
    leafletProxy("map", session) %>%
      setView(
        lng = data$longitude[idx],
        lat = data$latitude[idx],
        zoom = 18
      ) %>%
      clearPopups() %>%
      addPopups(
        lng = data$longitude[idx],
        lat = data$latitude[idx],
        popup = paste("Plot", data$authorobscode[idx], "is here!", sep = " ")
      )
  }

  # Handle map_request
  observe({
    req(input$page == "Map")
    req(map_request())

    data <- rv_data()
    idx <- map_request()

    if (length(idx) > 0) {
      invalidateLater(100) # Small delay to ensure map is ready
      update_map_view(idx)
      map_request(NULL) # Clear the request
    }
  })

  # Handle see details button click
  observeEvent(input$see_details, {
    idx <- as.numeric(input$see_details)
    if (!is.na(idx) && idx > 0) {
      update_and_open_details(idx)
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, idx, ignore.selectable = TRUE)
    }
  })

  # Handle close details button click
  observeEvent(input$close_details, {
    data <- rv_data()
    idx <- which(data$obsaccessioncode == selected_accession())
    if (length(idx) > 0) {
      dt_proxy <- dataTableProxy("dataTable")
      # Wish there was a better way to do this, but I canʻt find a deleselect function anywhere
      selectRows(dt_proxy, idx, ignore.selectable = FALSE)
    }
    details_open(FALSE)
    selected_accession(NULL)
    session$doBookmark()
  })

  # Handle show on map button click
  observeEvent(input$show_on_map, {
    idx <- as.numeric(input$show_on_map)
    updateNavbarPage(session, "page", selected = "Map")
    map_request(idx) # Set the request instead of updating map directly
  })

  # Handle label see details link click
  observeEvent(input$label_link_click, {
    data <- rv_data()
    idx <- which(data$obsaccessioncode == input$label_link_click)
    if (length(idx) > 0) {
      update_and_open_details(idx)
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, idx, ignore.selectable = TRUE)
    }
  })

  # TODO: Show all rows on empty search
  # Handle nav bar search submission
  observeEvent(input$search_enter, {
    updateNavbarPage(session, "page", selected = "Table")
    data <- rv_data()
    filtered <- data[
      apply(data, 1, function(row) any(grepl(input$search_enter, as.character(row)))),
    ]
    rv_data(filtered)
    dt_proxy <- dataTableProxy("dataTable")
    replaceData(dt_proxy, rv_data(), resetPaging = TRUE)
  })

  # Persist State _________________________________________________________________________________

  onBookmark(function(state) {
    state$values$selected_accession <- selected_accession()
    state$values$details_open <- details_open()
    state
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  # TODO: Restore map, search, and row selection state as well
  onRestore(function(state) {
    if (!is.null(state$values$selected_accession)) {
      acc <- state$values$selected_accession
      observeEvent(rv_data(),
        {
          data <- rv_data()
          idx <- match(acc, data$obsaccessioncode)
          if (!is.na(idx)) {
            selected_accession(acc)
            update_and_open_details(idx)
            dt_proxy <- dataTableProxy("dataTable")
            selectRows(dt_proxy, idx, ignore.selectable = TRUE)
          }
        },
        once = TRUE
      )
    }
    invisible(NULL)
  })

  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
}

# Helper Functions_________________________________________________________________________________

build_top_five_list <- function(data, column, suffix = "") {
  freq_table <- sort(table(data[[column]]), decreasing = TRUE)
  top_names <- names(head(freq_table, 5))
  items <- lapply(top_names, function(s) {
    count <- freq_table[[s]]
    tags$li(tags$strong(s), paste0(" ", count, suffix))
  })
  tags$ul(class = "list-unstyled", items)
}

build_taxa_list <- function(data_row) {
  taxa_cols <- grep("^toptaxon[0-9]+name$", names(data_row), value = TRUE)
  taxa <- data_row[taxa_cols]
  taxa <- taxa[!is.na(taxa)]
  if (length(taxa) > 0) {
    paste(head(taxa, 5), collapse = ", ")
  } else {
    "No taxa recorded"
  }
}

build_action_buttons <- function(i) {
  as.character(
    tagList(
      actionButton(
        inputId = paste0("see_details_", i),
        label = "See Details",
        class = "btn btn-info btn-sm details-btn mb-1",
        onclick = sprintf("Shiny.setInputValue('see_details', %d, {priority: 'event'})", i)
      ),
      actionButton(
        inputId = paste0("map_btn_", i),
        label = "Show on Map",
        class = "btn btn-primary btn-sm map-btn",
        onclick = sprintf("Shiny.setInputValue('show_on_map', '%s', {priority: 'event'})", i)
      )
    )
  )
}

# TODO: Refactor for redundancy with update_and_open_details
build_details_view <- function(selected_data) {
  row_details <- renderUI({
    valid <- selected_data[!sapply(selected_data, function(x) is.null(x) || all(is.na(x)))]
    lapply(names(valid), function(n) {
      tags$p(tags$strong(paste0(n, ": ")), valid[[n]])
    })
  })
  # TODO: Refactor for redundancy with build_taxa_list
  taxa_details <- renderUI({
    cols <- grep("^toptaxon[0-9]+name$", names(selected_data), value = TRUE)
    list_data <- selected_data[cols]
    list_data <- list_data[!is.na(list_data)]
    lapply(seq_along(list_data), function(i) {
      tags$p(class = "list-unstyled", tags$strong(paste0(i, ". ")), list_data[[i]])
    })
  })
  # TODO: Pull out into constant? Populate from existing dict in db?
  column_names <- list(
    "authorplotcode" = "Author Plot Code",
    "authorobscode" = "Author Observation Code",
    "area" = "Area",
    "permenance" = "Permenance",
    "elevation" = "Elevation",
    "slopeaspect" = "Slope Aspect",
    "slopegradient" = "Slope Gradient",
    "confidentialitystatus" = "Confidentiality Status",
    "latitude" = "Latitude",
    "longitude" = "Longitude",
    "locationnarrative" = "Location Description",
    "plotstateprovince" = "State/Province",
    "country" = "Country",
    "obsstartdate" = "Observation Start Date",
    "project_id" = "Project ID",
    "projectname" = "Project Name",
    "covermethod_id" = "Cover Method ID",
    "covertype" = "Cover Type",
    "stratummethod_id" = "Stratum Method ID",
    "stratummethodname" = "Stratum Method",
    "stratummethoddescription" = "Stratum Method Description",
    "taxonobservationarea" = "Taxon Observation Area",
    "autotaxoncover" = "Taxon Cover Automatically Calculated",
    "plotvalidationlevel" = "Plot Validation Level",
    "permanence" = "Permanent"
  )
  create_table <- function(dl) {
    tbody <- lapply(names(dl), function(name) {
      tags$tr(
        tags$td(tags$strong(column_names[[name]] %||% name)),
        tags$td(class = "text-end", dl[[name]])
      )
    })
    tags$table(
      class = "table table-sm table-striped table-hover",
      tags$tbody(tbody)
    )
  }
  render_details <- function(cols) {
    renderUI({
      dt <- selected_data[cols]
      dt <- dt[!sapply(dt, function(x) is.null(x) || all(is.na(x)))]
      create_table(dt)
    })
  }
  list(
    row_details = renderUI({
      row_details
    }),
    taxa_details = taxa_details,
    plot_id_details = render_details(c("authorobscode", "authorplotcode")),
    location_details = render_details(c(
      "confidentialitystatus", "latitude", "longitude",
      "locationnarrative", "plotstateprovince", "country"
    )),
    layout_details = render_details(c("area", "permanence")),
    environmental_details = render_details(c("elevation", "slopeaspect")), # "slopegradient"
    methods_details = render_details(c(
      "obsstartdate", "projectname", "covertype",
      "stratummethodname", "stratummethoddescription" # "taxonobservationarea", "autotaxoncover"
    )),
    plot_quality_details = render_details(c("plotvalidationlevel"))
  )
}
