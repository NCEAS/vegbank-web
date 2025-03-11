library(shiny)
library(bslib)
library(magrittr)
library(markdown)
library(DT)
library(leaflet)
library(htmltools)
library(jsonlite)

# Custom theme and rules
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

# Navbar and overlay builders
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
            12,
            card(
              card_header("App Overview"),
              card_body("Vegbank is a database of vegetation plot data. The data displayed in this
            app is a subset of the full dataset, containing 100 randomly selected plots. Each row
            in the table represents a plot, and the columns contain information about the plot
            location, species observed, and observer details. This is a simple Shiny app that
            demonstrates how to use bookmarking to save the state of the app in the URL. The
            app displays a table of data, a map, and a detailed view of each row in the table.
            You can search for specific rows using the search bar in the navbar or on the table
            page.")
            )
          )
        ),
        fluidRow(
          column(12, card(card_header("Data in Vegbank"), card_body(uiOutput("dataSummary"))))
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
      nav_panel(title = "FAQ", 
                includeMarkdown("/Users/dariangill/git/vegbank-web/shiny/www/faq.md"))
    )
  )
  tagQuery(navbar)$find("ul#page")$append(search_div)$allTags()
}

build_overlay <- function() {
  tags$div(
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
}
