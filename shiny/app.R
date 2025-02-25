library(shiny)
library(bslib)
library(magrittr)
library(markdown)
library(DT)

# Define UI for app that draws a histogram ----
ui <- page_navbar(
  id = "page",

  # App title ----
  title = "Ole Faithful!",

  # First page: Histogram ----
  nav_panel(
    title = "Histogram",
    sidebarLayout(
      sidebarPanel(
        # Input: Slider for the number of bins ----
        sliderInput(
          inputId = "bins",
          label = "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        )
      ),
      mainPanel(
        # Output: Histogram ----
        plotOutput(outputId = "distPlot")
      )
    )
  ),

  # Second page: Searchable Table ----
  nav_panel(
    title = "Table",
    fluidRow(
      column(6, DT::dataTableOutput(outputId = "dataTable")),  # Table on left
      column(6, uiOutput(outputId = "cardView"))  # Card view on right
    )
  ),

  # Third page: Map ----
  nav_panel(
    title = "Map",
    leaflet::leafletOutput(outputId = "map")
  ),

  nav_spacer(),
  nav_menu(
    title = "About",
    align = "right",
    nav_panel(
      title = "Old Faithful",
      includeMarkdown(
        "/Users/dariangill/git/vegbank-web/shiny/static_content/old_faithful.md"
      )
    ),
    nav_panel(
      title = "Geysers",
      includeMarkdown(
        "/Users/dariangill/git/vegbank-web/shiny/static_content/geysers.md"
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })

  output$dataTable <- DT::renderDataTable({
    DT::datatable(faithful, selection = "single")  # Enable single row selection
  })

  observeEvent(input$dataTable_rows_selected, {
    selected_row <- input$dataTable_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- faithful[selected_row, ]
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
      output$cardView <- renderUI({ NULL })  # Hide the card when no selection
    }
  })

  output$cardView <- renderUI(({ NULL }))  # Initially hide the card view

  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -110.8281, lat = 44.4605, zoom = 10) %>%
      leaflet::addMarkers(lng = -110.8281, lat = 44.4605,
                          popup = "Old Faithful!")
  })
}

shinyApp(ui = ui, server = server)