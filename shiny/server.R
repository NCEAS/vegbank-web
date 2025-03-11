library(shiny)
library(bslib)
library(magrittr)
library(DT)
library(leaflet)
library(htmltools)
library(jsonlite)

source("helpers.R")

server <- function(input, output, session) {
  rv_data <- reactiveVal(NULL)
  observe({
    tryCatch({
      data <- jsonlite::fromJSON("http://127.0.0.1:28015/gen_test_data")
      rv_data(data)
    }, error = function(e) {
      message("Error fetching data: ", e)
      rv_data(NULL)  # or you can assign fallback data here
    })
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

  output$dataTable <- DT::renderDataTable({
    data <- rv_data()
    details <- sapply(seq_len(nrow(data)), function(i) {
      sprintf('<button class="btn btn-info details-btn" data-row="%d">See Details</button>', i)
    })
    data <- cbind(Details = details, data)
    DT::datatable(data,
      escape = FALSE, selection = "none",
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
      leaflet() %>% addTiles() %>% addControl("Data unavailable", position = "topright")
    } else {
      leaflet(data = data) %>%
        addTiles() %>%
        addMarkers(lng = ~longitude, lat = ~latitude, popup = ~ create_popup_link(accessioncode))
    }
  })

  output$rowDetails <- renderUI({
    NULL
  })
  output$taxaDetails <- renderUI({
    NULL
  })

  # Helper: update panel details and open overlay
  selected_accession <- reactiveVal(NULL)
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
    # Save accession code
    selected_accession(selected_data$accessioncode)
    session$sendCustomMessage("openOverlay", list())
  }

  selected_row <- reactiveVal(NULL)
  observeEvent(input$see_details, {
    idx <- as.numeric(input$see_details)
    if (!is.na(idx) && idx > 0) {
      selected_row(idx)
      update_and_open_details(idx)
    }
  })

  observeEvent(input$marker_click, {
    data <- rv_data()
    sel <- which(data$accessioncode == input$marker_click)
    if (length(sel) > 0) {
      selected_row(sel)
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
    state
  })

  onRestore(function(state) {
    if (!is.null(state$values$selected_accession)) {
      acc <- state$values$selected_accession
      data <- rv_data()
      idx <- match(acc, data$accessioncode)
      if (!is.na(idx)) {
        selected_accession(acc)
        update_and_open_details(idx)
        dt_proxy <- dataTableProxy("dataTable")
        selectRows(dt_proxy, idx)
      }
    }
  })

  observeEvent(input$close_details, {
    selected_row(NULL)
    selected_accession(NULL)
  })

  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
