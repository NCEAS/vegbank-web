library(shiny)
library(bslib)
library(magrittr)
library(markdown)
library(DT)
library(leaflet)
library(htmltools)
library(jsonlite)

# Define custom theme and common rules
custom_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#19201d",
  info = "#2c5443",
  primary = "#72b9a2",
  secondary = "#d8fb5a",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
) %>%
  bs_add_rules(
    ".card-header { background-color: #2c5443; color: #FFFFFF; font-weight: bold; }
     .navbar-brand { color: #2c5443 !important; font-weight: bold; }
     .navbar-brand img { height: 30px; margin-right: 10px; }"
  )

create_popup_link <- function(accessioncode) {
  paste0(
    accessioncode,
    '\n<a href="#" onclick="Shiny.setInputValue(\'marker_click\', \'',
    accessioncode,
    '\', {priority: \'event\'})">See Details</a>'
  )
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
  # Helper function to generate renderUI details sections
  render_details <- function(cols) {
    renderUI({
      dt <- selected_data[cols]
      dt <- dt[!sapply(dt, function(x) is.null(x) || all(is.na(x)))]
      create_table(dt)
    })
  }

  plot_id_details <- render_details(c("authorobscode", "authorplotcode"))
  location_details <- render_details(c(
    "confidentialitystatus", "latitude", "longitude",
    "locationnarrative", "stateprovince", "country"
  ))
  layout_details <- render_details(c("area", "permanence"))
  environmental_details <- render_details(c("elevation", "slopeaspect", "slopegradient"))
  methods_details <- render_details(c(
    "obsstartdate", "project_id", "covermethod_id",
    "stratummethod_id", "taxonobservationarea",
    "autotaxoncover"
  ))
  plot_quality_details <- render_details(c("plotvalidationlevel"))
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

# Helper to build navbar and overlay (minimal duplication)
build_navbar <- function() {
  search_div <- tags$li(
    class = "nav-item",
    tags$div(
      class = "navbar-form",
      textInput("search", "", "Search..."),
      tags$script(HTML(
        "$(document).on('keypress', '#search', function(e) {
          if(e.which == 13){
            Shiny.setInputValue('search_enter', $(this).val(), {priority:'event'});
          }
         });"
      ))
    )
  )
  navbar <- page_navbar(
    id = "page",
    theme = custom_theme,
    title = tags$span(tags$img(src = "logo_vegbank_leaves.svg"), "Vegbank"),
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
            12,
            card(card_header("Data in Vegbank"), card_body(uiOutput("dataSummary")))
          )
        ),
        fluidRow(
          column(3, card(card_header("Top Places"), card_body(uiOutput("topPlaces")))),
          column(3, card(card_header("Top Species"), card_body(uiOutput("topSpecies")))),
          column(3, card(card_header("Top Observers"), card_body(uiOutput("topObservers")))),
          column(3, card(card_header("Top Years"), card_body(uiOutput("topYears"))))
        )
      )
    ),
    nav_menu(
      title = "Plots",
      nav_panel(title = "Table", DT::dataTableOutput("dataTable")),
      nav_panel(title = "Map", leafletOutput("map"))
    ),
    nav_menu(
      title = "Plants",
      nav_panel(
        title = "Table",
      ),
      nav_panel(
        title = "Map",
      )
    ),
    nav_menu(
      title = "Commuinties",
      nav_panel(
        title = "Table",
      ),
      nav_panel(
        title = "Map",
      )
    ),
    nav_menu(
      title = "Places",
      nav_panel(
        title = "Table",
      ),
      nav_panel(
        title = "Map",
      )
    ),
    nav_menu(
      title = "People",
      nav_panel(
        title = "Table",
      ),
      nav_panel(
        title = "Map",
      )
    ),
    nav_menu(
      title = "About",
      align = "right",
      nav_panel(
        title = "FAQ",
        includeMarkdown("/Users/dariangill/git/vegbank-web/shiny/www/faq.md")
      )
    )
  )
  navbar_with_search <- tagQuery(navbar)$find("ul#page")$append(search_div)$allTags()
  navbar_with_search
}

build_overlay <- function() {
  overlay <- tags$div(
    id = "detail-overlay",
    style = "position: fixed; top: 0; right: -400px; width: 400px; height: 100vh; overflow-y: auto;
             background: #fff; border-left: 1px solid #ccc; z-index: 1050; padding:20px;
             transition: right 0.4s;",
    actionButton("close_overlay", "",
      onclick = "document.getElementById('detail-overlay').style.right='-400px';
                            Shiny.setInputValue('close_details', true, {priority:'event'});",
      class = "btn-close", style = "float:right; margin-bottom:10px;"
    ),
    fluidRow(
      column(
        12,
        card(card_header("Plot IDs"), uiOutput("plot_id_details")),
        card(card_header("Location"), uiOutput("locationDetails")),
        card(card_header("Layout"), uiOutput("layout_details")),
        card(card_header("Environment"), uiOutput("environmental_details")),
        card(card_header("Methods"), uiOutput("methods_details")),
        card(card_header("Plot Quality"), uiOutput("plot_quality_details")),
        card(card_header("Top Taxa"), uiOutput("taxaDetails"))
      )
    )
  )
  overlay
}

# UI: use helper functions and add custom JS
ui <- function(req) {
  navbar_with_search <- build_navbar()
  overlay <- build_overlay()
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

# In server, factor out common update logic
server <- function(input, output, session) {
  rv_data <- reactiveVal(NULL)
  observe({
    data <- jsonlite::fromJSON("http://127.0.0.1:28015/gen_test_data")
    rv_data(data)
  })

  # Make outputs... ____________________________________________________________

  # Get number of plots
  output$dataSummary <- renderUI({
    tags$p(paste0("This dataset contains ", nrow(rv_data()), " plots."))
  })
  # Get top 5 states

  output$topPlaces <- renderUI({
    data <- rv_data()
    items <- lapply(
      names(head(sort(table(data$state), decreasing = TRUE), 5)),
      function(s) {
        tags$li(tags$strong(s), table(data$state)[[s]])
      }
    )
    tags$ul(class = "list-unstyled", items)
  })
  # Get top 5 species

  output$topSpecies <- renderUI({
    data <- rv_data()
    items <- lapply(
      names(head(sort(table(data$toptaxon1name), decreasing = TRUE), 5)),
      function(s) {
        tags$li(tags$strong(s), paste(table(data$toptaxon1name)[[s]], "occurrences"))
      }
    )
    tags$ul(class = "list-unstyled", items)
  })
  # Get top 5 authors

  output$topObservers <- renderUI({
    data <- rv_data()
    items <- lapply(
      names(head(sort(table(data$interp_current_partyname), decreasing = TRUE), 5)),
      function(s) {
        tags$li(tags$strong(s), paste(table(data$interp_current_partyname)[[s]], "plots"))
      }
    )
    tags$ul(class = "list-unstyled", items)
  })
  # Get top 5 years

  output$topYears <- renderUI({
    data <- rv_data()
    items <- lapply(
      names(head(sort(table(data$dateentered), decreasing = TRUE), 5)),
      function(s) {
        tags$li(tags$strong(s), paste(table(data$dateentered)[[s]], "plots"))
      }
    )
    tags$ul(class = "list-unstyled", items)
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
        pageLength = 100, scrollY = "calc(100vh - 300px)",
        scrollX = TRUE, scrollCollapse = TRUE
      )
    )
  })

  output$map <- leaflet::renderLeaflet({
    df <- rv_data()
    leaflet(data = df) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude, popup = ~ create_popup_link(accessioncode))
  })

  output$rowDetails <- renderUI({
    NULL
  })
  output$taxaDetails <- renderUI({
    NULL
  })

  # Helper function: update details and open overlay
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
    session$sendCustomMessage("openOverlay", list())
  }

  # Observer for see details button clicks
  selected_row <- reactiveVal(NULL)
  observeEvent(input$see_details, {
    idx <- as.numeric(input$see_details)
    if (!is.na(idx) && idx > 0) {
      selected_row(idx)
      update_and_open_details(idx)
    }
  })
  # Observer for map marker clicks (reuse same update)
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

  # Bookmarking: save and restore selected row
  onBookmark(function(state) {
    state$values$selected_row <- selected_row()
    state
  })
  onRestore(function(state) {
    if (!is.null(state$values$selected_row)) {
      selected_row(state$values$selected_row)
      update_and_open_details(state$values$selected_row)
      dt_proxy <- dataTableProxy("dataTable")
      selectRows(dt_proxy, state$values$selected_row)
    }
  })

  observeEvent(input$close_details, {
    selected_row(NULL)
  })

  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
}

shinyApp(ui, server, enableBookmarking = "url")
