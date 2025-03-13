library(shiny)
library(leaflet)
library(magrittr)
library(DT)

server <- function(input, output, session) {
  rv_data <- reactiveVal(NULL)
  selected_accession <- reactiveVal(NULL)
  details_open <- reactiveVal(FALSE)
  map_request <- reactiveVal(NULL)
  current_zoom <- reactiveVal(NULL)

  observe({
    tryCatch(
      {
        data <- jsonlite::fromJSON("http://127.0.0.1:28015/gen_all_states_test_data")
        rv_data(data)
      },
      error = function(e) {
        message("Error fetching data: ", e)
        rv_data(NULL)
      }
    )
  })

  # Render UI Outputs ____________________________________________________________________________
  output$dataSummary <- renderUI({
    tags$p(paste0("This dataset contains ", nrow(rv_data()), " plots."))
  })

  output$topPlaces <- renderUI({
    build_top_five_list(rv_data(), "stateprovince")
  })

  output$topSpecies <- renderUI({
    build_top_five_list(rv_data(), "toptaxon1name", " occurrences")
  })

  output$topObservers <- renderUI({
    build_top_five_list(rv_data(), "interp_current_partyname", " plots")
  })

  output$topYears <- renderUI({
    build_top_five_list(rv_data(), "dateentered", " plots")
  })

  output$dataTable <- DT::renderDataTable({
    data <- rv_data()
    actions <- mapply(
      build_action_buttons,
      seq_len(nrow(data)),
      data$accessioncode
    )

    display_data <- data.frame(
      "Actions" = actions,
      "Author Plot Code" = data$authorobscode,
      "Location" = data$stateprovince,
      "Top Taxa" = apply(data, 1, build_taxa_list),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    DT::datatable(display_data,
      escape = FALSE,
      # TODO: Clicking another row deselects the programtically selected row. It shouldnʻt.
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
      leaflet(data) %>%
        addTiles() %>%
        addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~accessioncode,
          popup = ~ build_popup_link(accessioncode),
        ) %>%
        htmlwidgets::onRender("
        function(el, x) {
          this.on('zoomend', function(e) {
            Shiny.setInputValue('map_zoom', this.getZoom());
          });
        }
      ")
    }
  })

  # Handle Events _________________________________________________________________________________

  # Change the visibility of the marker labels based on the zoom level
  observeEvent(input$map_zoom, {
    current_zoom(input$map_zoom)
    data <- rv_data()
    if (!is.null(data)) {
      # Handle duplicates by grouping on lat & long, and concatenate all authorobscode values
      data_grouped <- data %>%
        dplyr::group_by(latitude, longitude) %>% # nolint: object_usage_linter.
        dplyr::mutate(
          authorobscode_label =
            paste(authorobscode, collapse = "<br>") # nolint: object_usage_linter.
        ) %>%
        dplyr::ungroup()

      show_labels <- input$map_zoom >= 14
      leafletProxy("map", session) %>%
        clearMarkers() %>%
        addMarkers(
          data = data_grouped,
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~accessioncode,
          popup = ~ build_popup_link(accessioncode),
          # Display concatenated authorobscode for duplicates
          label = ~ authorobscode_label %>% lapply(htmltools::HTML),
          # TODO: Explore cluster options for markers & labels
          labelOptions = labelOptions(
            noHide = show_labels,
            direction = "bottom",
            textOnly = TRUE,
            # TODO: Tie these colors to the theme
            style = list(
              "color" = "#2c5443",
              "font-weight" = "bold",
              "padding" = "3px 8px",
              "background" = "white",
              "border" = "1px solid #2c5443",
              "border-radius" = "3px"
            )
          )
        )
    }
  })

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
    selected_accession(rv_data()[idx, "accessioncode"])
    details_open(TRUE)
    session$sendCustomMessage("openOverlay", list())
  }

  # TODO: Parameterize this to pull out of server fn?
  update_map_view <- function(acc, idx) {
    data <- rv_data()
    leafletProxy("map", session) %>%
      setView(
        lng = data$longitude[idx],
        lat = data$latitude[idx],
        zoom = 14
      ) %>%
      clearPopups() %>%
      addPopups(
        lng = data$longitude[idx],
        lat = data$latitude[idx],
        popup = build_popup_link(acc)
      )
    selected_accession(acc)
  }

  # Handle map_request
  observe({
    req(input$page == "Map")
    req(map_request())

    data <- rv_data()
    acc <- map_request()
    idx <- which(data$accessioncode == acc)

    if (length(idx) > 0) {
      invalidateLater(100) # Small delay to ensure map is ready
      update_map_view(acc, idx)
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
    details_open(FALSE)
    selected_accession(NULL)
    idx <- as.numeric(input$see_details)
    if (!is.na(idx) && idx > 0) {
      dt_proxy <- dataTableProxy("dataTable")
      # Wish there was a better way to do this, but I canʻt find a deleselect function anywhere
      selectRows(dt_proxy, idx, ignore.selectable = FALSE)
    }
    session$doBookmark()
  })

  # Handle show on map button click
  observeEvent(input$show_on_map, {
    acc <- input$show_on_map
    updateNavbarPage(session, "page", selected = "Map")
    map_request(acc) # Set the request instead of updating map directly
  })

  # Handle marker popup see details link click
  observeEvent(input$popup_link_click, {
    data <- rv_data()
    idx <- which(data$accessioncode == input$popup_link_click)
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
          idx <- match(acc, data$accessioncode)
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

  # TODO: Refactor this for redundancy. Use onRestored to open details view instead of js in ui?
  # observe({
  #   req(rv_data())
  #   restored <- getQueryString()
  #   if (!is.null(restored$details_open) && restored$details_open == "true" &&
  #         !is.null(restored$selected_accession)) {
  #     idx <- match(restored$selected_accession, rv_data()$accessioncode)
  #     if (!is.na(idx)) {
  #       update_and_open_details(idx)
  #     }
  #   }
  # })
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

build_action_buttons <- function(i, acc) {
  sprintf(
    '<button class="btn btn-info btn-sm details-btn" data-row="%d">See Details</button>
      <button class="btn btn-primary btn-sm map-btn" data-acc="%s">Show on Map</button>',
    i, acc
  )
}

build_popup_link <- function(accessioncode) {
  paste0(
    accessioncode,
    '\n<a href="#" onclick="Shiny.setInputValue(\'popup_link_click\', \'',
    accessioncode,
    '\', {priority: \'event\'})">See Details</a>'
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
    "stateprovince" = "State/Province",
    "country" = "Country",
    "obsstartdate" = "Observation Start Date",
    "project_id" = "Project ID",
    "covermethod_id" = "Cover Method ID",
    "stratummethod_id" = "Stratum Method ID",
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
      "locationnarrative", "stateprovince", "country"
    )),
    layout_details = render_details(c("area", "permanence")),
    environmental_details = render_details(c("elevation", "slopeaspect", "slopegradient")),
    methods_details = render_details(c(
      "obsstartdate", "project_id", "covermethod_id",
      "stratummethod_id", "taxonobservationarea", "autotaxoncover"
    )),
    plot_quality_details = render_details(c("plotvalidationlevel"))
  )
}
