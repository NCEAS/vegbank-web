#' Shiny UI for Vegbank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
ui <- function(req) {
  shiny::addResourcePath("assets", system.file("shiny/www", package = "vegbankweb"))

  navbar_with_search <- build_navbar()
  overlay <- build_detail_overlay()

  script <- htmltools::tags$script(htmltools::HTML(
    "Shiny.addCustomMessageHandler('openOverlay', function(message) {
      if (document.getElementById('detail-overlay')) {
        document.getElementById('detail-overlay').style.right = '0px';
      }
    });

    Shiny.addCustomMessageHandler('invalidateMapSize', function(message) {
      var mapWidget = HTMLWidgets.find('#map');
      if (mapWidget) {
        var map = mapWidget.getMap();
        if(map) {
          map.invalidateSize();
        }
      }
    });

    $(document).ready(function() {
      var params = new URLSearchParams(window.location.search);
      if(params.get('details_open') === 'true') {
        if (document.getElementById('detail-overlay')) {
            document.getElementById('detail-overlay').style.right = '0px';
        }
      }
     });

    Shiny.addCustomMessageHandler('updateDetailType', function(message) {
      const type = message.type;
      const plotCards = document.getElementById('plot-details-cards');
      const communityConceptCards = document.getElementById('community-concept-details-cards');
      const communityClassificationCards = document.getElementById('community-classification-details-cards');
      const taxonObservationCards = document.getElementById('taxon-observation-details-cards');

      console.log('Updating detail type to:', type);

      if (plotCards && communityConceptCards && communityClassificationCards && taxonObservationCards) {
        // Hide all card types first
        plotCards.style.display = 'none';
        communityConceptCards.style.display = 'none';
        communityClassificationCards.style.display = 'none';
        taxonObservationCards.style.display = 'none';

        // Show the requested type
        if (type === 'plot-observation') {
          console.log('Showing plot details');
          plotCards.style.display = 'block';
        } else if (type === 'community-concept') {
          console.log('Showing community details');
          communityConceptCards.style.display = 'block';
        } else if (type === 'community-classification') {
          console.log('Showing taxon observation details');
          communityClassificationCards.style.display = 'block';
        } else if (type === 'taxon-observation') {
          console.log('Showing taxon observation details');
          taxonObservationCards.style.display = 'block';
        }
      }
    });
  "
  ))

  htmltools::tagList(navbar_with_search, overlay, script)
}

#' Custom Bootstrap Theme for Vegbank Web Application
#'
#' Defines a bslib theme with custom rules.
#'
#' @return A Bootstrap theme object.
#'
#' @noRd
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
     }
     .detail-section {
         display: none; /* Hide by default */
     }
     #community-description p {
         margin-bottom: 0.75rem;
     }"
)

#' Build Navigation Bar for Vegbank UI
#'
#' Constructs and returns the navigation bar to be used in the UI.
#'
#' @return A Shiny tag list representing the navigation bar.
#'
#' @noRd
build_navbar <- function() {
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
        )
      )
    ),
    bslib::nav_panel(
      title = "Map",
      leaflet::leafletOutput("map")
    ),
    bslib::nav_panel(
      title = "Plots",
      shiny::fluidPage(
        DT::dataTableOutput("plot_table"),
      )
    ),
    bslib::nav_panel(title = "Plants"),
    bslib::nav_panel(
      title = "Communities",
      shiny::fluidPage(
        DT::dataTableOutput("comm_table"),
      )
    ),
    bslib::nav_panel(title = "Places"),
    bslib::nav_panel(title = "People"),
    bslib::nav_panel(title = "Projects"),
    bslib::nav_menu(
      title = "About",
      align = "right",
      bslib::nav_panel(
        title = "FAQ",
        shiny::includeMarkdown(file.path("inst", "shiny", "www", "faq.md"))
      )
    )
  )
}

#' Build Detail Overlay for Vegbank UI
#'
#' Constructs the overlay panel that displays detailed plot information.
#'
#' @return A Shiny tag representing the detail overlay.
#'
#' @noRd
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
        # Plot Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plot-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Plot IDs"), shiny::uiOutput("plot_id_details")),
          bslib::card(bslib::card_header("Location"), shiny::uiOutput("location_details")),
          bslib::card(bslib::card_header("Layout"), shiny::uiOutput("layout_details")),
          bslib::card(bslib::card_header("Environment"), shiny::uiOutput("environmental_details")),
          bslib::card(bslib::card_header("Methods"), shiny::uiOutput("methods_details")),
          bslib::card(bslib::card_header("Plot Quality"), shiny::uiOutput("plot_quality_details")),
          bslib::card(bslib::card_header("Communities"), shiny::uiOutput("communities_details")),
          bslib::card(bslib::card_header("Top Taxa"), shiny::uiOutput("taxa_details"))
        ),

        # Community Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Community Name"), shiny::uiOutput("community_name")),
          bslib::card(bslib::card_header("Occurrences"), shiny::uiOutput("observation_count")),
          bslib::card(bslib::card_header("Community Description"), shiny::uiOutput("community_description")),
          bslib::card(bslib::card_header("Aliases"), shiny::uiOutput("community_aliases"))
        ),

        # Community Classification Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-classification-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Observation Details"), shiny::uiOutput("observation_details")),
          bslib::card(bslib::card_header("Community Interpretation"), shiny::uiOutput("community_interpretation"))
        ),

        # Taxon Observation Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "taxon-observation-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Taxon Name"), shiny::uiOutput("taxon_name")),
          bslib::card(bslib::card_header("Coverage"), shiny::uiOutput("taxon_coverage")),
          bslib::card(bslib::card_header("Aliases"), shiny::uiOutput("taxon_aliases")),
          bslib::card(bslib::card_header("Identifiers"), shiny::uiOutput("taxon_identifiers"))
        )
      )
    )
  )
}
