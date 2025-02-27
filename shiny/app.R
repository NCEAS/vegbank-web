library(shiny)
library(bslib)
library(magrittr)
library(markdown)
library(DT)
library(leaflet)

# Define custom theme using bslib
custom_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#19201d",
  primary = "#72b9a2",
  secondary = "#d8fb5a",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# Wrap UI in a function(req) for bookmarking support
ui <- function(req) {
  page_navbar(
    id = "page",
    theme = custom_theme,  # Apply the custom theme
    title = "Ole Faithful!",

    # Add a search bar to the navbar (aligned to right)
    nav_spacer(),
    div(class = "navbar-form navbar-right",
      style = "display: inline-flex; align-items: center;",
      textInput("search", label = NULL, placeholder = "Search..."),
      actionButton("search_btn", "Search")
    ),

    # First page: Histogram
    nav_panel(
      title = "Histogram",
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "bins",
            label = "Number of bins:",
            min = 1,
            max = 50,
            value = 30
          )
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )
    ),

    # Second page: Searchable Table
    nav_panel(
      title = "Table",
      fluidRow(
        column(6, DT::dataTableOutput("dataTable")),
        column(6, uiOutput("cardView"))
      )
    ),

    # Third page: Map
    nav_panel(
      title = "Map",
      leafletOutput("map")
    ),

    nav_spacer(),
    nav_menu(
      title = "About",
      align = "right",
      nav_panel(
        title = "Old Faithful",
        includeMarkdown("/Users/dariangill/git/vegbank-web/shiny/static_content/old_faithful.md")
      ),
      nav_panel(
        title = "Geysers",
        includeMarkdown("/Users/dariangill/git/vegbank-web/shiny/static_content/geysers.md")
      )
    )
  )
}

server <- function(input, output, session) {

  # Reactive value to hold table data; starts with full faithful dataset
  rv_data <- reactiveVal(faithful)

  # Render histogram based on the 'bins' input
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#3b8a71", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })

  # Restore state for 'bins' and datatable row selection
  onRestore(function(state) {
    updateSliderInput(session, "bins", value = state$input$bins)
    if (!is.null(state$input$dataTable_rows_selected) &&
        length(state$input$dataTable_rows_selected) > 0) {
      selected_row <- state$input$dataTable_rows_selected
      selected_data <- faithful[selected_row, ]
      output$cardView <- renderUI({
        tagList(
          h3(paste("Row", selected_row)),
          p(paste("Duration of eruption:", selected_data$eruptions, "minutes")),
          p(paste("Waiting time to next eruption:", selected_data$waiting, "minutes"))
        )
      })
      # Use the DT proxy to update the highlighted row in the datatable
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, selected_row)
    }
  })

  # Render the data table from reactive data rt
  output$dataTable <- DT::renderDataTable({
    DT::datatable(rv_data(), selection = "single")
  })

  # Update cardView when a row is selected
  observeEvent(input$dataTable_rows_selected, {
    selected_row <- input$dataTable_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- rv_data()[selected_row, ]
      output$cardView <- renderUI({
        card(
          card_header("Old Faithful Geyser Data"),
          card_body(
            h3(paste("Row ", selected_row)),
            p(paste("Duration of eruption:",
                    selected_data$eruptions,
                    "minutes")),
            p(paste("Waiting time to next eruption:",
                    selected_data$waiting,
                    "minutes"))
          )
        )
      })
    } else {
      output$cardView <- renderUI({ NULL })
    }
  })

  # Initially hide the card view (in case no row is selected at start)
  output$cardView <- renderUI({ NULL })

  # Render the map
  output$map <- leaflet::renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -110.8281, lat = 44.4605, zoom = 10) %>%
      addMarkers(lng = -110.8281, lat = 44.4605, popup = "Old Faithful!")
  })

  # Search button observer to filter the table and navigate to "Table" page
  observeEvent(input$search_btn, {
    # Use updateNavbarPage to switch to the "Table" tab (page id "page" and title "Table")
    updateNavbarPage(session, "page", selected = "Table")
    
    # Filter faithful by matching the search term against character versions of eruptions or waiting
    search_term <- tolower(input$search)
    filtered <- faithful[
      grepl(search_term, as.character(faithful$eruptions)) |
      grepl(search_term, as.character(faithful$waiting)), ]
    
    # Update the reactive data and then update the table via DT proxy
    rv_data(filtered)
    dt_proxy <- dataTableProxy("dataTable")
    replaceData(dt_proxy, rv_data(), resetPaging = TRUE)
  })

  # Automatically trigger bookmarking on any input change
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })

  # Update the browser's URL query string when bookmarking completes
  onBookmarked(function(url) {
    updateQueryString(url)
  })
}

shinyApp(ui, server, enableBookmarking = "url")
