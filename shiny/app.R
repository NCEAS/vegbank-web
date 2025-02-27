library(shiny)
library(bslib)
library(magrittr)
library(markdown)
library(DT)
library(leaflet)

# Define custom theme using bslib
custom_theme <- bs_theme(
  bg = "#FFFFFF",        # Background color
  fg = "#19201d",        # Foreground color
  primary = "#72b9a2",   # Primary color
  secondary = "#d8fb5a", # Secondary color
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# Wrap UI in a function(req) for bookmarking support
ui <- function(req) {
  page_navbar(
    id = "page",
    theme = custom_theme,  # Apply the custom theme
    title = "Ole Faithful!",

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

  # Render the data table
  output$dataTable <- DT::renderDataTable({
    DT::datatable(faithful, selection = "single")
  })

  # Update cardView when a row is selected
  observeEvent(input$dataTable_rows_selected, {
    selected_row <- input$dataTable_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- faithful[selected_row, ]
      output$cardView <- renderUI({
        tagList(
          h3(paste("Row", selected_row)),
          p(paste("Duration of eruption:", selected_data$eruptions, "minutes")),
          p(paste("Waiting time to next eruption:", selected_data$waiting, "minutes"))
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
