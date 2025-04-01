#' Shiny UI for Vegbank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#' @export
ui <- function(req) {
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
      tags$script(htmltools::HTML(
        "$(document).on('keypress', '#search', function(e) {
           if(e.which == 13){
             Shiny.setInputValue('search_enter', $(this).val(), {priority:'event'});
           }
         });"
      ))
    )
  )

  navbar <- shiny::page_navbar( # use triple-colon to access unexported function
    id = "page",
    theme = custom_theme,
    title = htmltools::tags$span(tags$img(src = "logo_vegbank_leaves.svg"), "Vegbank"),
    shiny::nav_panel( # use triple-colon here as well
      title = "Overview",
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            12,
            bslib::card(
              bslib::card_header("App Overview"),
              bslib::card_body(uiOutput("dataSummary"))
            )
          )
        ),
        fluidRow(
          column(4, card(card_header("Top Places"), card_body(plotOutput("topPlaces")))),
          column(4, card(card_header("Top Species"), card_body(plotOutput("topSpecies")))),
          column(4, card(
            card_header("Most Recent Uploads"),
            card_body(uiOutput("mostRecentUploads"))
          ))
        ),
        fluidRow(
          column(
            6,
            card(
              card_header("Authors"),
              card_body(plotly::plotlyOutput("authorPie"))
            )
          ),
          column(6, card(card_header("Plot Heatmap"), card_body(plotOutput("plotHeatmap"))))
        )
      )
    ),
    shiny::nav_menu( # and here
      title = "Plots",
      nav_panel(title = "Table", DT::dataTableOutput("dataTable")),
      nav_panel(title = "Map", leaflet::leafletOutput("map"))
    ),
    shiny::nav_menu(
      title = "Plants",
      nav_panel(title = "Table"),
      nav_panel(title = "Map")
    ),
    shiny::nav_menu(
      title = "Communities",
      nav_panel(title = "Table"),
      nav_panel(title = "Map")
    ),
    shiny::nav_menu(
      title = "Places",
      nav_panel(title = "Table"),
      nav_panel(title = "Map")
    ),
    shiny::nav_menu(
      title = "People",
      nav_panel(title = "Table"),
      nav_panel(title = "Map")
    ),
    shiny::nav_menu(
      title = "Projects",
      nav_panel(title = "Table"),
      nav_panel(title = "Map")
    ),
    shiny::nav_menu(
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

#' Build Detail Overlay for Vegbank UI
#'
#' Constructs the overlay panel that displays detailed plot information.
#'
#' @return A Shiny tag representing the detail overlay.
#' @keywords internal
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
