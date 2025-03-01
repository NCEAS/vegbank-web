library(shiny)
library(bslib)
library(magrittr)
library(markdown)
library(DT)
library(leaflet)
library(htmltools)
library(jsonlite)

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
  # Define the search <li> to be appended
  search_div <- tags$li(
    class = "nav-item",
    tags$div(
      class = "navbar-form",
      textInput(
        inputId = "search",
        placeholder = "Search...",
        label = NULL
      ),
      # JavaScript to trigger search_enter when Enter is pressed in the text input
      tags$script(HTML(
        "$(document).on('keypress', '#search', function(e) {
            if(e.which == 13) {
              Shiny.setInputValue('search_enter', $(this).val(), {priority: 'event'});
            }
         });"
      ))
    )
  )

  # Build the UI using page_navbar normally...
  navbar <- page_navbar(
    id = "page",
    theme = custom_theme, # Apply the custom theme
    title = "Vegbank",

    # First page: Overview
    nav_panel(
      title = "Overview",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            card(
              card_header("Top Places"),
              card_body()
            )
          ),
          column(
            width = 3,
            card(
              card_header("Top Species"),
              card_body(),
            )
          ),
          column(
            width = 3,
            card(
              card_header("Top Observers"),
              card_body()
            )
          ),
          column(
            width = 3,
            card(
              card_header("Top Years"),
              card_body()
            )
          )
        )
      )
    ),

    # Second page: Table (only datatable here)
    nav_panel(
      title = "Table",
      DT::dataTableOutput("dataTable")
    ),

    # Third page: Map
    nav_panel(
      title = "Map",
      leafletOutput("map")
    ),

    # Fourth page: Details (card view)
    nav_panel(
      title = "Details",
      uiOutput("cardView")
    ),
    nav_menu(
      title = "About",
      align = "right",
      nav_panel(
        title = "Frequently Asked Questions",
        includeMarkdown("/Users/dariangill/git/vegbank-web/shiny/static_content/faq.md")
      )
    ),
  )
  navbar_with_search <- tagQuery(navbar)$find("ul#page")$append(search_div)$allTags()
  navbar_with_search
}

server <- function(input, output, session) {
  # Reactive value to hold table data from the JSON file
  rv_data <- reactiveVal(jsonlite::fromJSON("/Users/dariangill/git/vegbank-web/shiny/100_plot_obs.json"))

  # Render histogram based on the 'bins' input remains unchanged
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x,
      breaks = bins, col = "#3b8a71", border = "white",
      xlab = "Waiting time to next eruption (in mins)",
      main = "Histogram of waiting times"
    )
  })

  # Restore state for 'bins' and datatable row selection
  onRestore(function(state) {
    updateSliderInput(session, "bins", value = state$input$bins)
    if (!is.null(state$input$dataTable_rows_selected) &&
      length(state$input$dataTable_rows_selected) > 0) {
      selected_row <- state$input$dataTable_rows_selected
      selected_data <- rv_data()[selected_row, ]
      output$cardView <- renderUI({
        # Display all non NULL/NA fields from the selected row
        valid_data <- selected_data[!sapply(selected_data, function(x) is.null(x) || all(is.na(x)))]
        details <- lapply(names(valid_data), function(n) {
          tags$p(tags$strong(paste0(n, ": ")), valid_data[[n]])
        })
        card(
          card_header("Row Details"),
          card_body(details)
        )
      })
      # Update the highlighted row in the datatable
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, selected_row)
    }
  })

  # Render datatable using rv_data()
  output$dataTable <- DT::renderDataTable({
    DT::datatable(
      rv_data(),
      selection = "single",
      options = list(
        pageLength = 100,
        scrollY = "calc(100vh - 300px)",
        scrollX = TRUE,
        scrollCollapse = TRUE
      )
    )
  })

  # Update Details panel when a row is selected in the table
  observeEvent(input$dataTable_rows_selected, {
    selected_row <- input$dataTable_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- rv_data()[selected_row, ]
      output$cardView <- renderUI({
        # Build a card displaying each non-null field from the selected row
        valid_data <- selected_data[!sapply(selected_data, function(x) is.null(x) || all(is.na(x)))]
        details <- lapply(names(valid_data), function(n) {
          tags$p(tags$strong(paste0(n, ": ")), valid_data[[n]])
        })
        card(
          card_header("Row Details"),
          card_body(details)
        )
      })
      # Switch to the "Details" nav panel
      updateNavbarPage(session, "page", selected = "Details")
    }
  })

  # Initially hide the card view (in case no row is selected at start)
  output$cardView <- renderUI({
    NULL
  })

  # Render the leaflet map using fields "longitude" and "latitude"
  output$map <- leaflet::renderLeaflet({
    df <- rv_data()
    leaflet(data = df) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ sprintf(
          '<a href="#" onclick="Shiny.setInputValue(\'marker_click\', \'%s\', {priority: \'event\'})">%s</a>',
          accessioncode, accessioncode
        )
      )
  })

  # Listener for marker popup link clicks
  observeEvent(input$marker_click, {
    code_clicked <- input$marker_click
    data <- rv_data()
    sel <- which(data$accessioncode == code_clicked)
    if (length(sel) > 0) {
      selected_data <- data[sel, ]
      output$cardView <- renderUI({
        valid_data <- selected_data[!sapply(selected_data, function(x) is.null(x) || all(is.na(x)))]
        details <- lapply(names(valid_data), function(n) {
          tags$p(tags$strong(paste0(n, ": ")), valid_data[[n]])
        })
        card(
          card_header("Row Details"),
          card_body(details)
        )
      })
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, sel)
      updateNavbarPage(session, "page", selected = "Details")
    }
  })

  # Search observer triggered when the user presses Enter in the search bar.
  observeEvent(input$search_enter, {
    # Switch to the "Table" page
    updateNavbarPage(session, "page", selected = "Table")

    # Get the search term and the current JSON table data
    search_term <- input$search_enter
    data <- rv_data()

    # Filter rows: check across all fields in each row for a match with the search term
    filtered <- data[apply(data, 1, function(row) {
      any(grepl(search_term, as.character(row)))
    }), ]

    # Update the reactive data and then update the datatable via DT proxy
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
