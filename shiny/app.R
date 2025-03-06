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
) %>%
  bs_add_rules(
    ".card-header {
      background-color: #2c5443;
      color: #FFFFFF;
      font-weight: bold;
    }
    .navbar-brand {
      color: #2c5443 !important;
      font-weight: bold;
    }"
    # .navbar-brand img {
    #   height: 30px;
    #   margin-right: 10px;
    # }"
  )

create_popup_link <- function(accessioncode) {
  paste0(
    accessioncode,
    '\n<a href="#" onclick="Shiny.setInputValue(\'marker_click\', \'',
    accessioncode,
    '\', {priority: \'event\'})">See Details</a>'
  )
}

# Add this function before the server function
create_details_view <- function(selected_data) {
  # Create row details (unchanged)
  row_details <- renderUI({
    valid_data <- selected_data[!sapply(selected_data, function(x) is.null(x) || all(is.na(x)))]
    details <- lapply(names(valid_data), function(n) {
      tags$p(tags$strong(paste0(n, ": ")), valid_data[[n]])
    })
    details
  })

  # Create taxa details (unchanged)
  taxa_details <- renderUI({
    taxa_columns <- grep("^toptaxon[0-9]+name$", names(selected_data), value = TRUE)
    taxa_list <- selected_data[taxa_columns]
    taxa_list <- taxa_list[!is.na(taxa_list)]

    taxa_details <- lapply(seq_along(taxa_list), function(i) {
      tags$p(class = "list-unstyled", tags$strong(paste0(i, ". ")), taxa_list[[i]])
    })
    taxa_details
  })

  # Dictionary for human-readable names (unchanged)
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

  # Modified create_table function to properly create table rows
  create_table <- function(data_list) {
    tbody_contents <- lapply(names(data_list), function(name) {
      tags$tr(
        tags$td(tags$strong(column_names[[name]] %||% name)), # Use %||% for NULL fallback
        tags$td(class = "text-end", data_list[[name]])
      )
    })

    tags$table(
      class = "table table-sm table-striped table-hover",
      tags$tbody(
        tbody_contents
      )
    )
  }

  # Create all detail sections using the helper function
  plot_id_details <- renderUI({
    plot_id_columns <- c("authorobscode", "authorplotcode")
    plot_id_list <- selected_data[plot_id_columns]
    plot_id_list <- plot_id_list[!sapply(plot_id_list, function(x) is.null(x) || all(is.na(x)))]
    create_table(plot_id_list)
  })

  location_details <- renderUI({
    location_columns <- c(
      "confidentialitystatus", "latitude", "longitude",
      "locationnarrative", "stateprovince", "country"
    )
    location_list <- selected_data[location_columns]
    location_list <- location_list[!sapply(location_list, function(x) is.null(x) || all(is.na(x)))]
    create_table(location_list)
  })

  layout_details <- renderUI({
    layout_columns <- c("area", "permanence")
    layout_list <- selected_data[layout_columns]
    layout_list <- layout_list[!sapply(layout_list, function(x) is.null(x) || all(is.na(x)))]
    create_table(layout_list)
  })

  environmental_details <- renderUI({
    environmental_columns <- c("elevation", "slopeaspect", "slopegradient")
    environmental_list <- selected_data[environmental_columns]
    environmental_list <- environmental_list[!sapply(environmental_list, function(x) is.null(x) || all(is.na(x)))]
    create_table(environmental_list)
  })

  methods_details <- renderUI({
    methods_columns <- c(
      "obsstartdate", "project_id", "covermethod_id",
      "stratummethod_id", "taxonobservationarea", "autotaxoncover"
    )
    methods_list <- selected_data[methods_columns]
    methods_list <- methods_list[!sapply(methods_list, function(x) is.null(x) || all(is.na(x)))]
    create_table(methods_list)
  })

  plot_quality_details <- renderUI({
    plot_quality_columns <- c("plotvalidationlevel")
    plot_quality_list <- selected_data[plot_quality_columns]
    plot_quality_list <- plot_quality_list[!sapply(plot_quality_list, function(x) is.null(x) || all(is.na(x)))]
    create_table(plot_quality_list)
  })

  list(
    row_details = row_details,
    taxa_details = taxa_details,
    location_details = location_details,
    plot_id_details = plot_id_details,
    layout_details = layout_details,
    environmental_details = environmental_details,
    methods_details = methods_details,
    plot_quality_details = plot_quality_details
  )
}

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

  navbar <- page_navbar(
    id = "page",
    theme = custom_theme,
    title = "Vegbank",
    # title = tags$span(
    #   tags$img(src = "static_content/logo_vegbank_leaves.svg"),
    #   "Vegbank"
    # ),

    # First page: Overview
    nav_panel(
      title = "Overview",
      fluidPage(
        fluidRow(
          column(
            width = 12,
            card(
              card_header("App Overview"),
              card_body(
                "Vegbank is a database of vegetation plot data. The data displayed in this app is a
                subset of the full dataset, containing 100 randomly selected plots. Each row in the
                table represents a plot, and the columns contain information about the plot
                location, species observed, and observer details. This is a simple Shiny app that
                demonstrates how to use bookmarking to save the state of the app in the URL. The
                app displays a table of data, a map, and a detailed view of each row in the table.
                You can search for specific rows using the search bar in the navbar or on the table
                page."
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            card(
              card_header("Data in Vegbank"),
              card_body(uiOutput("dataSummary"))
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            card(
              card_header("Top Places"),
              card_body(
                uiOutput("topPlaces")
              )
            )
          ),
          column(
            width = 3,
            card(
              card_header("Top Species"),
              card_body(
                uiOutput("topSpecies")
              )
            )
          ),
          column(
            width = 3,
            card(
              card_header("Top Observers"),
              card_body(
                uiOutput("topObservers")
              )
            )
          ),
          column(
            width = 3,
            card(
              card_header("Top Years"),
              card_body(
                uiOutput("topYears")
              )
            )
          )
        )
      )
    ),

    # Second page: Table
    nav_panel(
      title = "Table",
      DT::dataTableOutput("dataTable")
    ),

    # Third page: Map
    nav_panel(
      title = "Map",
      leafletOutput("map")
    ),

    # Removed Details nav panel
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

  # Add sliding overlay panel for details
  overlay <- tags$div(
    id = "detail-overlay",
    style = "position: fixed; top: 0; right: -400px; width: 400px; height: 100vh; overflow-y: auto;
             background: #fff; border-left: 1px solid #ccc; z-index: 1050; padding:20px;
             transition: right 0.4s;",
    # Close button
    tags$button("Close",
      onclick = "document.getElementById('detail-overlay').style.right='-400px';",
      style = "float:right; margin-bottom:10px;"
    ),
    # Details content (same as previous details nav panel)
    fluidRow(
      column(
        width = 12,
        card(
          card_header("Plot IDs"),
          uiOutput("plot_id_details")
        ),
        card(
          card_header("Location"),
          uiOutput("locationDetails")
        ),
        card(
          card_header("Layout"),
          uiOutput("layout_details")
        ),
        card(
          card_header("Environment"),
          uiOutput("environmental_details")
        ),
        card(
          card_header("Methods"),
          uiOutput("methods_details")
        ),
        card(
          card_header("Plot Quality"),
          uiOutput("plot_quality_details")
        ),
        card(
          card_header("Top Taxa"),
          uiOutput("taxaDetails")
        )
      )
    )
  )

  # Custom JS to open overlay from server message.
  script <- tags$script(HTML(
    "Shiny.addCustomMessageHandler('openOverlay', function(message) {
         document.getElementById('detail-overlay').style.right = '0px';
     });"
  ))

  # Add JS binding for "see details" buttons so they trigger input$see_details
  btn_script <- tags$script(HTML(
    '$(document).on("click", ".details-btn", function() {
         var idx = $(this).data("row");
         Shiny.setInputValue("see_details", idx, {priority:"event"});
    });'
  ))

  tagList(navbar_with_search, overlay, script, btn_script)
}

server <- function(input, output, session) {
  # Reactive value to hold table data from the JSON file
  rv_data <- reactiveVal(NULL)

  observe({
    # Reading local file
    # data <- jsonlite::fromJSON("/Users/dariangill/git/vegbank-web/shiny/100_plot_obs.json")
    # Hit api for same data
    data <- jsonlite::fromJSON("http://127.0.0.1:28015/gen_test_data")
    rv_data(data)
  })

  # Make outputs... ____________________________________________________________

  # Get number of plots
  output$dataSummary <- renderUI({
    data <- rv_data()
    summary <- paste0("This dataset contains ", nrow(data), " plots.")
    tags$p(summary)
  })

  # Get top 5 states
  output$topPlaces <- renderUI({
    data <- rv_data()
    state_counts <- table(data$state)
    top_states <- head(sort(state_counts, decreasing = TRUE), 5)

    items <- lapply(names(top_states), function(state) {
      tags$li(tags$strong(paste0(state, ": ")), top_states[state])
    })

    tags$ul(class = "list-unstyled", items)
  })

  # Get top 5 species
  output$topSpecies <- renderUI({
    data <- rv_data()
    species_counts <- table(data$toptaxon1name)
    top_species <- head(sort(species_counts, decreasing = TRUE), 5)

    items <- lapply(names(top_species), function(species) {
      tags$li(tags$strong(paste0(species, ": ")), paste(top_species[species], "occurrences"))
    })

    tags$ul(class = "list-unstyled", items)
  })

  # Get top 5 authors
  output$topObservers <- renderUI({
    data <- rv_data()
    interpreter_counts <- table(data$interp_current_partyname)
    top_interpreters <- head(sort(interpreter_counts, decreasing = TRUE), 5)

    items <- lapply(names(top_interpreters), function(interpreter) {
      tags$li(tags$strong(paste0(interpreter, ": ")), paste(top_interpreters[interpreter], "plots"))
    })

    tags$ul(class = "list-unstyled", items)
  })

  # Get top 5 years
  output$topYears <- renderUI({
    data <- rv_data()
    year_counts <- table(data$dateentered)
    top_years <- head(sort(year_counts, decreasing = TRUE), 5)

    items <- lapply(names(top_years), function(year) {
      tags$li(tags$strong(paste0(year, ": ")), paste(top_years[year], "plots"))
    })

    tags$ul(class = "list-unstyled", items)
  })

  # Render datatable using rv_data()
  output$dataTable <- DT::renderDataTable({
    data <- rv_data()
    # Create "Details" button for each row
    details_col <- sapply(seq_len(nrow(data)), function(i) {
      sprintf('<button class="btn btn-info details-btn" data-row="%d">See Details</button>', i)
    })
    # Insert the Details column as the first column (after row names)
    data <- cbind(Details = details_col, data)

    DT::datatable(
      data,
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


  # Render the leaflet map using fields "longitude" and "latitude"
  output$map <- leaflet::renderLeaflet({
    df <- rv_data()
    leaflet(data = df) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ create_popup_link(accessioncode)
      )
  })

  # Initially hide the detailed view (in case no row/pin is selected at start)
  output$rowDetails <- renderUI({
    NULL
  })
  output$taxaDetails <- renderUI({
    NULL
  })


  # Hanlde Events... _________________________________________________________

  # Store the selected row in a reactiveVal for persistence
  selected_row <- reactiveVal(NULL)

  # Helper function to update details view
  update_details_view <- function(selected_data) {
    details <- create_details_view(selected_data)
    # Update all output elements with details
    output$rowDetails <- details$row_details
    output$locationDetails <- details$location_details
    output$plot_id_details <- details$plot_id_details
    output$layout_details <- details$layout_details
    output$environmental_details <- details$environmental_details
    output$methods_details <- details$methods_details
    output$plot_quality_details <- details$plot_quality_details
    output$taxaDetails <- details$taxa_details
  }

  # Add new observer for "see details" button clicks
  observeEvent(input$see_details, {
    idx <- as.numeric(input$see_details)
    if (!is.na(idx) && idx > 0) {
      selected_row <- idx
      selected_data <- rv_data()[idx, ]
      update_details_view(selected_data)
      print("clicked")
      session$sendCustomMessage("openOverlay", list())
    }
  })

  # Restore state from bookmark
  onRestore(function(state) {
    if (!is.null(state$values$selected_row)) {
      selected_row(state$values$selected_row)
      selected_data <- rv_data()[state$values$selected_row, ]
      update_details_view(selected_data)
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, state$values$selected_row)
    }
  })

  # Add selected row to bookmark
  onBookmark(function(state) {
    state$values$selected_row <- selected_row()
    state
  })

  # Handle map marker clicks
  observeEvent(input$marker_click, {
    code_clicked <- input$marker_click
    data <- rv_data()
    sel <- which(data$accessioncode == code_clicked)
    if (length(sel) > 0) {
      selected_row(sel)
      selected_data <- data[sel, ]
      update_details_view(selected_data)
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, sel)
      session$sendCustomMessage("openOverlay", list())
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
