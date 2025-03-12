library(shiny)
library(leaflet)
library(magrittr)
library(DT)

server <- function(input, output, session) {
  rv_data <- reactiveVal(NULL)
  selected_accession <- reactiveVal(NULL)
  details_open <- reactiveVal(FALSE)
  map_proxy <- leafletProxy("map")

  observe({
    tryCatch(
      {
        data <- jsonlite::fromJSON("http://127.0.0.1:28015/gen_test_data")
        rv_data(data)
      },
      error = function(e) {
        message("Error fetching data: ", e)
        rv_data(NULL)
      }
    )
  })

  # ...Render UI outputs...
  output$dataSummary <- renderUI({
    tags$p(paste0("This dataset contains ", nrow(rv_data()), " plots."))
  })

  render_top_five_list <- function(data, column, suffix = "") {
    freq_table <- sort(table(data[[column]]), decreasing = TRUE)
    top_names <- names(head(freq_table, 5))
    items <- lapply(top_names, function(s) {
      count <- freq_table[[s]]
      tags$li(tags$strong(s), paste0(" ", count, suffix))
    })
    tags$ul(class = "list-unstyled", items)
  }

  output$topPlaces <- renderUI({
    render_top_five_list(rv_data(), "stateprovince")
  })
  output$topSpecies <- renderUI({
    render_top_five_list(rv_data(), "toptaxon1name", " occurrences")
  })
  output$topObservers <- renderUI({
    render_top_five_list(rv_data(), "interp_current_partyname", " plots")
  })
  output$topYears <- renderUI({
    render_top_five_list(rv_data(), "dateentered", " plots")
  })

  format_taxa_list <- function(data_row) {
    taxa_cols <- grep("^toptaxon[0-9]+name$", names(data_row), value = TRUE)
    taxa <- data_row[taxa_cols]
    taxa <- taxa[!is.na(taxa)]
    if (length(taxa) > 0) {
      paste(head(taxa, 5), collapse = ", ")
    } else {
      "No taxa recorded"
    }
  }

  format_action_buttons <- function(i, acc) {
    sprintf(
      '<button class="btn btn-info btn-sm details-btn" data-row="%d">See Details</button>
      <button class="btn btn-primary btn-sm map-btn" data-acc="%s">Show on Map</button>',
      i, acc
    )
  }

  output$dataTable <- DT::renderDataTable({
    data <- rv_data()
    actions <- mapply(
      format_action_buttons,
      seq_len(nrow(data)),
      data$accessioncode
    )

    # Create simplified dataset with only required columns
    display_data <- data.frame(
      "Actions" = actions,
      "Author Plot Code" = data$authorobscode,
      "Location" = data$stateprovince,
      "Top Taxa" = apply(data, 1, format_taxa_list),
      stringsAsFactors = FALSE,
      check.names = FALSE # Prevent conversion of spaces to periods
    )

    DT::datatable(display_data,
      escape = FALSE,
      selection = "none",
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
      leaflet(data = data) %>%
        addTiles() %>%
        addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~accessioncode, # Add unique ID for each marker
          popup = ~ create_popup_link(accessioncode)
        )
    }
  })

  # Move helper function definition before it's used
  update_and_open_details <- function(idx) {
    selected_data <- rv_data()[idx, ]
    details <- create_details_view(selected_data)
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

  observeEvent(input$see_details, {
    idx <- as.numeric(input$see_details)
    if (!is.na(idx) && idx > 0) {
      update_and_open_details(idx)
    }
  })

  observeEvent(input$show_on_map, {
    data <- rv_data()
    acc <- input$show_on_map
    idx <- which(data$accessioncode == acc)

    if (length(idx) > 0) {
      # First switch to map view
      updateNavbarPage(session, "page", selected = "Map")

      # Wait briefly for the map to be ready
      shinyjs::delay(500, {
        # Center map
        map_proxy %>%
          setView(
            lng = data$longitude[idx],
            lat = data$latitude[idx],
            zoom = 12
          )

        # Show popup and details
        selected_accession(acc)
        session$sendInputValue("marker_click", acc)
      })
    }
  })

  observeEvent(input$marker_click, {
    data <- rv_data()
    sel <- which(data$accessioncode == input$marker_click)
    if (length(sel) > 0) {
      update_and_open_details(sel)
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, sel)
    }
  })

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

  # Bookmarking: save and restore selected accession code
  onBookmark(function(state) {
    state$values$selected_accession <- selected_accession()
    state$values$details_open <- details_open()
    state
  })

  onRestore(function(state) {
    if (!is.null(state$values$selected_accession)) {
      acc <- state$values$selected_accession
      observeEvent(rv_data(), {
        data <- rv_data()
        idx <- match(acc, data$accessioncode)
        if (!is.na(idx)) {
          selected_accession(acc)
          update_and_open_details(idx)
          dt_proxy <- dataTableProxy("dataTable")
          selectRows(dt_proxy, idx)
        }
      }, once = TRUE)
    }
    invisible(NULL)
  })

  observeEvent(input$close_details, {
    details_open(FALSE)
    selected_accession(NULL)
    session$doBookmark()
  })

  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })

  observe({
    req(rv_data())
    restored <- getQueryString()

    if (!is.null(restored$details_open) && restored$details_open == "true" &&
      !is.null(restored$selected_accession)) {
      idx <- match(restored$selected_accession, rv_data()$accessioncode)
      if (!is.na(idx)) {
        update_and_open_details(idx)
      }
    }
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}

# Details view helper
create_details_view <- function(selected_data) {
  row_details <- renderUI({
    valid <- selected_data[!sapply(selected_data, function(x) is.null(x) || all(is.na(x)))]
    lapply(names(valid), function(n) {
      tags$p(tags$strong(paste0(n, ": ")), valid[[n]])
    })
  })
  taxa_details <- renderUI({
    cols <- grep("^toptaxon[0-9]+name$", names(selected_data), value = TRUE)
    list_data <- selected_data[cols]
    list_data <- list_data[!is.na(list_data)]
    lapply(seq_along(list_data), function(i) {
      tags$p(class = "list-unstyled", tags$strong(paste0(i, ". ")), list_data[[i]])
    })
  })
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
