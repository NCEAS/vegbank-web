#' Shiny UI for Vegbank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
ui <- function(req) {
  shiny::addResourcePath("assets", system.file("shiny/www", package = "vegbankweb"))

  # Ensure Inter font loads from CDN before any CSS
  font_head <- htmltools::tags$head(
    htmltools::tags$link(rel = "preconnect", href = "https://rsms.me/"),
    htmltools::tags$link(rel = "stylesheet", href = "https://rsms.me/inter/inter.css")
  )

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

    // Store pending row selections until tables are ready
    var pendingSelections = [];
    var currentSelection = null; // Track the currently selected accession code
    
    // Listen for native DataTables draw events to restore selections
    $(document).on('draw.dt', function(e, settings) {
      var tableId = settings.sTableId;
      console.log('DataTable draw event for:', tableId);
      
      // Restore current selection after table redraw
      if (currentSelection) {
        console.log('Restoring current selection after table redraw:', currentSelection);
        attemptRowSelection(currentSelection, false); // false = don't clear current selection
      }
      
      // Check for any pending selections for this table or any table
      for (var i = pendingSelections.length - 1; i >= 0; i--) {
        var pendingSelection = pendingSelections[i];
        console.log('Processing pending selection:', pendingSelection.accessionCode);
        
        // Try to select the row now that table is ready
        if (attemptRowSelection(pendingSelection.accessionCode)) {
          console.log('Successfully processed pending selection for:', pendingSelection.accessionCode);
          // Remove from pending list and set as current selection
          currentSelection = pendingSelection.accessionCode;
          pendingSelections.splice(i, 1);
        }
      }
    });
    
    // Function to attempt row selection
    function attemptRowSelection(accessionCode, clearCurrent) {
      if (clearCurrent !== false) { // Default to true unless explicitly set to false
        console.log('Attempting to select row with accession:', accessionCode);
        // Clear all selections from all tables first
        $('table tbody tr').removeClass('selected-entity');
      }
      
      // Find the button with the specific accession code
      var targetButton = $('button').filter(function() {
        var onclick = $(this).attr('onclick');
        return onclick && onclick.includes(\"'\" + accessionCode + \"'\");
      });
      
      console.log('Found', targetButton.length, 'buttons with accession code');
      
      if (targetButton.length > 0) {
        // Get the row containing the button and select it
        var targetRow = targetButton.closest('tr');
        targetRow.addClass('selected-entity');
        console.log('SUCCESS: Selected row for accession', accessionCode);
        return true;
      } else {
        console.log('No buttons found for accession', accessionCode);
        return false;
      }
    }

    Shiny.addCustomMessageHandler('selectTableRowByAccession', function(message) {
      console.log('Received selection request for:', message.accessionCode);
      
      // Try immediate selection first
      if (!attemptRowSelection(message.accessionCode)) {
        console.log('Immediate selection failed, adding to pending queue');
        // Add to pending selections if immediate attempt fails
        pendingSelections.push({
          accessionCode: message.accessionCode,
          timestamp: Date.now()
        });
      } else {
        // Set as current selection if successful
        currentSelection = message.accessionCode;
      }
    });

    Shiny.addCustomMessageHandler('clearAllTableSelections', function(message) {
      $('table tbody tr').removeClass('selected-entity');
      currentSelection = null;
      console.log('Cleared all table selections');
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
      const projectCards = document.getElementById('project-details-cards');
      const partyCards = document.getElementById('party-details-cards');
      const plantConceptCards = document.getElementById('plant-concept-details-cards');

      console.log('Updating detail type to:', type);

      if (plotCards && communityConceptCards && communityClassificationCards &&
      taxonObservationCards && projectCards && partyCards && plantConceptCards) {
        // Hide all card types first
        plotCards.style.display = 'none';
        communityConceptCards.style.display = 'none';
        communityClassificationCards.style.display = 'none';
        taxonObservationCards.style.display = 'none';
        projectCards.style.display = 'none';
        partyCards.style.display = 'none';
        plantConceptCards.style.display = 'none';

        // Show the requested type
        if (type === 'plot-observation') {
          console.log('Showing plot details');
          plotCards.style.display = 'block';
        } else if (type === 'community-concept') {
          console.log('Showing community details');
          communityConceptCards.style.display = 'block';
        } else if (type === 'community-classification') {
          console.log('Showing community classification details');
          communityClassificationCards.style.display = 'block';
        } else if (type === 'taxon-observation') {
          console.log('Showing taxon observation details');
          taxonObservationCards.style.display = 'block';
        } else if (type === 'project') {
          console.log('Showing project details');
          projectCards.style.display = 'block';
        } else if (type === 'party') {
          console.log('Showing party details');
          partyCards.style.display = 'block';
        } else if (type === 'plant-concept') {
          console.log('Showing plant concept details');
          plantConceptCards.style.display = 'block';
        }
      }
    });
  "
  ))

  htmltools::tagList(font_head, navbar_with_search, overlay, script)
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
  base_font = bslib::font_collection("Inter", "InterVariable", "system-ui", "sans-serif"),
  heading_font = bslib::font_collection("Inter", "InterVariable", "system-ui", "sans-serif"),
  "font-size-base" = "0.875rem"
)
custom_theme <- bslib::bs_add_rules(
  custom_theme,
  ":root {
    font-family: Inter, sans-serif !important;
    font-feature-settings: 'liga' 1, 'calt' 1;
    --bs-font-sans-serif: Inter, sans-serif !important;
    
    /* Status badge colors */
    --no-status-bg: hsl(204, 6%, 90%);
    --no-status-text: hsl(0, 0%, 20%);
    --accepted-bg: hsl(153, 31%, 79%);
    --accepted-text: hsl(152, 69%, 19%);
    --not-current-bg: hsl(45, 100%, 85%);
    --not-current-text: hsl(45, 94%, 21%);
  }
  @supports (font-variation-settings: normal) {
    :root { 
      font-family: InterVariable, sans-serif !important;
      --bs-font-sans-serif: InterVariable, sans-serif !important;
    }
  }

  *, *::before, *::after {
    font-family: Inter, sans-serif !important;
    font-variant-numeric: tabular-nums slashed-zero !important;
    font-feature-settings: 'tnum' 1, 'zero' 1, 'ss01' 1, 'ss02' 1, 'liga' 1, 'calt' 1 !important;
  }
  @supports (font-variation-settings: normal) {
    *, *::before, *::after {
      font-family: InterVariable, sans-serif !important;
    }
  }

  .card-header {
    background-color: #2c5443;
    color: #FFFFFF;
    font-weight: bold;
  }
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
      display: none;
  }
  #community-description p {
      margin-bottom: 0.75rem;
  }
  .dataTables_wrapper table th {
      font-weight: 500 !important;
  }
  .datatables .dataTables_wrapper div.dataTables_info {
      padding-top: 0.75rem;
      font-size: 0.875rem !important;
  }
  .dataTables_wrapper tbody tr.selected-entity,
  table.dataTable tbody tr.selected-entity,
  .table tbody tr.selected-entity {
      background-color: rgba(114, 185, 162, 0.15) !important;
  }
  .dataTables_wrapper tbody tr.selected-entity:hover,
  table.dataTable tbody tr.selected-entity:hover,
  .table tbody tr.selected-entity:hover {
      background-color: rgba(114, 185, 162, 0.25) !important;
  }
"
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
    bslib::nav_panel(title = "Plants"
      , shiny::fluidPage(
        DT::dataTableOutput("plant_table"),
      )
    ),
    bslib::nav_panel(
      title = "Communities",
      shiny::fluidPage(
        DT::dataTableOutput("comm_table"),
      )
    ),
    bslib::nav_panel(
      title = "People",
      shiny::fluidPage(
        DT::dataTableOutput("party_table"),
      )
    ),
    bslib::nav_panel(
      title = "Projects",
      shiny::fluidPage(
        DT::dataTableOutput("proj_table"),
      )
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
          bslib::card(bslib::card_header("Taxa Observed"), shiny::uiOutput("taxa_details"))
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

        # Project Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "project-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Project Name"), shiny::uiOutput("project_name")),
          bslib::card(bslib::card_header("Description"), shiny::uiOutput("project_description")),
          bslib::card(bslib::card_header("Dates"), shiny::uiOutput("project_dates")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("project_contributors")),
          bslib::card(bslib::card_header("Plot Observation Count"), shiny::uiOutput("project_observations"))
        ),

        # Taxon Observation Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "taxon-observation-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Taxon Name"), shiny::uiOutput("taxon_name")),
          bslib::card(bslib::card_header("Coverage"), shiny::uiOutput("taxon_coverage")),
          bslib::card(bslib::card_header("Aliases"), shiny::uiOutput("taxon_aliases")),
          bslib::card(bslib::card_header("Identifiers"), shiny::uiOutput("taxon_identifiers"))
        ),

        # Party Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "party-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Name"), shiny::uiOutput("party_name")),
          bslib::card(bslib::card_header("Organization"), shiny::uiOutput("party_organization")),
          bslib::card(bslib::card_header("Contact Information"), shiny::uiOutput("party_contact")),
          bslib::card(bslib::card_header("Projects"), shiny::uiOutput("party_projects"))
        ),

        # Plant Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plant-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Plant Concept"), shiny::uiOutput("plant_concept_name")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("plant_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("plant_party_perspective")),
        )
      )
    )
  )
}
