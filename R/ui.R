#' Shiny UI for Vegbank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#' @importFrom magrittr %>%
ui <- function(req) {
  shiny::addResourcePath("assets", "/Users/dariangill/git/vegbank-web/inst/shiny/www")

  navbar_with_search <- build_navbar()
  overlay <- build_detail_overlay()

  script <- htmltools::tags$script(htmltools::HTML(
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

  btn_script <- htmltools::tags$script(htmltools::HTML(
    '$(document).on("click", ".details-btn", function() {
         var idx = $(this).data("row");
         Shiny.setInputValue("see_details", idx, {priority:"event"});
    });
    $(document).on("click", ".map-btn", function() {
         var acc = $(this).data("acc");
         Shiny.setInputValue("show_on_map", acc, {priority:"event"});
    });'
  ))
  htmltools::tagList(navbar_with_search, overlay, script, btn_script)
}

#' Custom Bootstrap Theme for Vegbank Web Application
#'
#' Defines a bslib theme with custom rules.
#'
#' @return A Bootstrap theme object.
#' @keywords internal
custom_theme <- bslib::bs_theme(
  bg = "#FFFFFF",
  fg = "#19201d",
  info = "#2c5443",
  primary = "#72b9a2",
  secondary = "#72b9a2",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)
custom_theme <- bslib::bs_add_rules(
  custom_theme,
  ".card-header { background-color: #2c5443; color: #FFFFFF; font-weight: bold; }
     .navbar {
         min-height: 56px !important;
         display: flex;
         align-items: center;
     }
     .navbar-nav {
         display: flex;
         align-items: center;
         height: 100%;
     }
     .navbar-brand {
         color: #2c5443 !important;
         font-weight: bold;
         padding: 0;
         display: flex;
         align-items: center;
     }
     .navbar-brand img {
         height: 30px;
         margin-right: 10px;
     }
     .nav-item {
         display: flex;
         align-items: center;
         height: 100%;
     }
     .navbar-form {
         display: flex;
         align-items: center;
         margin-bottom: 5px;
     }
     .navbar-form .form-group {
         margin: 0;
     }
     .form-control {
         height: 36px;
     }"
)

#' Build Navigation Bar for Vegbank UI
#'
#' Constructs and returns the navigation bar to be used in the UI.
#'
#' @return A Shiny tag list representing the navigation bar.
#' @keywords internal
build_navbar <- function() {
  search_div <- htmltools::tags$li(
    class = "nav-item",
    htmltools::tags$div(
      class = "navbar-form",
      shiny::textInput(inputId = "search", label = "", value = "", placeholder = "Search"),
      htmltools::tags$script(htmltools::HTML(
        "$(document).on('keypress', '#search', function(e) {
           if(e.which == 13){
             Shiny.setInputValue('search_enter', $(this).val(), {priority:'event'});
           }
         });"
      ))
    )
  )

  navbar <- bslib::page_navbar(
    id = "page",
    theme = custom_theme,
    title = htmltools::tags$span(
      htmltools::tags$img(
        src = "assets/logo_vegbank_leaves.svg"
      ),
      "Vegbank"
    ),
    bslib::nav_panel(
      title = "Overview",
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            12,
            bslib::card(
              bslib::card_header("App Overview"),
              bslib::card_body(shiny::uiOutput("dataSummary"))
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(4, bslib::card(
            bslib::card_header("Top Places"),
            bslib::card_body(shiny::plotOutput("topPlaces"))
          )),
          shiny::column(4, bslib::card(
            bslib::card_header("Top Species"),
            bslib::card_body(shiny::plotOutput("topSpecies"))
          )),
          shiny::column(4, bslib::card(
            bslib::card_header("Most Recent Uploads"),
            bslib::card_body(shiny::uiOutput("mostRecentUploads"))
          ))
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            bslib::card(
              bslib::card_header("Authors"),
              bslib::card_body(plotly::plotlyOutput("authorPie"))
            )
          ),
          shiny::column(
            6,
            bslib::card(
              bslib::card_header("Plot Heatmap"),
              bslib::card_body(shiny::plotOutput("plotHeatmap"))
            )
          )
        )
      )
    ),
    bslib::nav_menu(
      title = "Plots",
      bslib::nav_panel(title = "Table", DT::dataTableOutput("dataTable")),
      bslib::nav_panel(title = "Map", leaflet::leafletOutput("map"))
    ),
    bslib::nav_menu(
      title = "Plants",
      bslib::nav_panel(title = "Table"),
      bslib::nav_panel(title = "Map")
    ),
    bslib::nav_menu(
      title = "Communities",
      bslib::nav_panel(title = "Table"),
      bslib::nav_panel(title = "Map")
    ),
    bslib::nav_menu(
      title = "Places",
      bslib::nav_panel(title = "Table"),
      bslib::nav_panel(title = "Map")
    ),
    bslib::nav_menu(
      title = "People",
      bslib::nav_panel(title = "Table"),
      bslib::nav_panel(title = "Map")
    ),
    bslib::nav_menu(
      title = "Projects",
      bslib::nav_panel(title = "Table"),
      bslib::nav_panel(title = "Map")
    ),
    bslib::nav_menu(
      title = "About",
      align = "right",
      bslib::nav_panel(
        title = "FAQ",
        shiny::includeMarkdown(file.path("inst", "shiny", "www", "faq.md"))
      )
    )
  )
  htmltools::tagQuery(navbar)$find("ul#page")$append(search_div)$allTags()
}

#' Build Detail Overlay for Vegbank UI
#'
#' Constructs the overlay panel that displays detailed plot information.
#'
#' @return A Shiny tag representing the detail overlay.
#' @keywords internal
build_detail_overlay <- function() {
  htmltools::tags$div(
    id = "detail-overlay",
    style = "position: fixed; top: 0; right: -400px; width: 400px; height: 100vh; overflow-y: auto;
             background: #fff; border-left: 1px solid #ccc; z-index: 1050; padding:20px;
             transition: right 0.4s;",
    shiny::actionButton("close_overlay", "",
      onclick = "document.getElementById('detail-overlay').style.right='-400px';
                            Shiny.setInputValue('close_details', true, {priority:'event'});",
      class = "btn-close", style = "float:right; margin-bottom:10px;"
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        bslib::card(bslib::card_header("Plot IDs"), shiny::uiOutput("plot_id_details")),
        bslib::card(bslib::card_header("Location"), shiny::uiOutput("locationDetails")),
        bslib::card(bslib::card_header("Layout"), shiny::uiOutput("layout_details")),
        bslib::card(bslib::card_header("Environment"), shiny::uiOutput("environmental_details")),
        bslib::card(bslib::card_header("Methods"), shiny::uiOutput("methods_details")),
        bslib::card(bslib::card_header("Plot Quality"), shiny::uiOutput("plot_quality_details")),
        bslib::card(bslib::card_header("Top Taxa"), shiny::uiOutput("taxaDetails"))
      )
    )
  )
}
