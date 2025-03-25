library(shiny)
library(htmltools)
library(bslib)
library(magrittr)
library(shinyjs)

ui <- function(req) {
  navbar_with_search <- build_navbar()
  overlay <- build_detail_overlay()

  script <- tags$script(HTML(
    "Shiny.addCustomMessageHandler('openOverlay', function(message) {
         document.getElementById('detail-overlay').style.right = '0px';
     });

     $(document).ready(function() {
         var params = new URLSearchParams(window.location.search);
         if(params.get('details_open') === 'true') {
             document.getElementById('detail-overlay').style.right = '0px';
         }
     });"
  ))

  btn_script <- tags$script(HTML(
    '$(document).on("click", ".details-btn", function() {
         var idx = $(this).data("row");
         Shiny.setInputValue("see_details", idx, {priority:"event"});
    });
    $(document).on("click", ".map-btn", function() {
         var acc = $(this).data("acc");
         Shiny.setInputValue("show_on_map", acc, {priority:"event"});
    });'
  ))
  tagList(navbar_with_search, overlay, script, btn_script)
}

# Custom theme and rules
custom_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#19201d",
  info = "#2c5443",
  primary = "#72b9a2",
  secondary = "#72b9a2",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
) %>%
  bs_add_rules(
    ".card-header { background-color: #2c5443; color: #FFFFFF; font-weight: bold; }
     .navbar-brand { color: #2c5443 !important; font-weight: bold; }
     .navbar-brand img { height: 30px; margin-right: 10px; }"
  )

# Navbar and overlay builders
build_navbar <- function() {
  search_div <- tags$li(
    class = "nav-item",
    tags$div(
      class = "navbar-form",
      textInput(inputId = "search", label = "", value = "", placeholder = "Search"),
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
              card_body(uiOutput("dataSummary"))
            )
          )
        ),
        fluidRow(
          column(4, card(card_header("Top Places"), card_body(plotOutput("topPlaces")))),
          column(4, card(card_header("Top Species"), card_body(plotOutput("topSpecies")))),
          column(4, card(card_header("Top Years"), card_body(uiOutput("topYears"))))
        ),
        fluidRow(
          column(
            6,
            card(
              card_header("Authors"),
              card_body(plotlyOutput("authorPie")) # nolint: object_usage_linter.
            )
          ),
          column(6, card(card_header("Plot Heatmap"), card_body(plotOutput("plotHeatmap"))))
        )
      )
    ),
    nav_menu(
      title = "Plots",
      nav_panel(title = "Table", DT::dataTableOutput("dataTable")),
      nav_panel(title = "Map", leaflet::leafletOutput("map"))
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
      title = "Communities",
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
      title = "Projects",
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
  tagQuery(navbar)$find("ul#page")$append(search_div)$allTags()
}

build_detail_overlay <- function() {
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
