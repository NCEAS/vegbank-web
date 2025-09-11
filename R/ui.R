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

    Shiny.addCustomMessageHandler('selectTableRow', function(message) {
      setTimeout(function() {
        try {
          var table = null;
          
          console.log('Trying to select row in table:', message.tableId);
          console.log('Current registry:', Object.keys(window.vegbankTables || {}));
          
          // Try to find the table in the global registry
          if (window.vegbankTables && window.vegbankTables[message.tableId]) {
            table = window.vegbankTables[message.tableId];
            console.log('Found table in registry');
          } else {
            // Fallback: try to find the table by ID in the DOM
            var tableElement = $('#' + message.tableId + ' table');
            console.log('Looking for DOM element:', '#' + message.tableId + ' table', 'found:', tableElement.length);
            
            if (tableElement.length && $.fn.DataTable.isDataTable(tableElement)) {
              table = tableElement.DataTable();
              console.log('Found DataTable via DOM fallback');
              
              // Add the programmatic selection method if missing
              if (!table.selectRowsProgrammatic) {
                table.selectRowsProgrammatic = function(rowIndex) {
                  // Remove selection class from all rows across all pages
                  $(this.table().body()).find('tr').removeClass('selected');
                  console.log('Cleared all selections');
                  
                  if (rowIndex !== null && rowIndex !== undefined) {
                    var self = this;
                    var pageInfo = self.page.info();
                    var totalRows = self.data().count();
                    var rowPage = Math.floor(rowIndex / pageInfo.length);
                    
                    console.log('Selection details:');
                    console.log('  Target row:', rowIndex);
                    console.log('  Total rows:', totalRows);
                    console.log('  Page length:', pageInfo.length);
                    console.log('  Current page:', pageInfo.page);
                    console.log('  Calculated target page:', rowPage);
                    
                    // Check if row index is valid
                    if (rowIndex >= totalRows) {
                      console.warn('Row index', rowIndex, 'is beyond total rows', totalRows);
                      return;
                    }
                    
                    // Function to select the row
                    var selectRow = function() {
                      // Try different methods to get the row
                      console.log('Debugging row access for index', rowIndex);
                      
                      // Method 1: Direct row access
                      var rowNode = self.row(rowIndex).node();
                      console.log('Method 1 - self.row(' + rowIndex + ').node():', rowNode);
                      
                      // Method 2: Check if row exists in data
                      var rowData = self.row(rowIndex).data();
                      console.log('Method 2 - Row data exists:', rowData !== undefined);
                      
                      // Method 3: Try to find the row on current page
                      var currentPageRows = self.rows({page: 'current'}).nodes().toArray();
                      console.log('Method 3 - Rows on current page:', currentPageRows.length);
                      
                      // Method 4: Get all row indices on current page
                      var currentPageIndices = self.rows({page: 'current'}).indexes().toArray();
                      console.log('Method 4 - Row indices on current page:', currentPageIndices);
                      
                      if (rowNode) {
                        $(rowNode).addClass('selected');
                        console.log('SUCCESS: Added selected class to row', rowIndex);
                        console.log('Row classes:', $(rowNode).attr('class'));
                      } else {
                        console.warn('FAILED: Row node is null for index', rowIndex);
                        
                        // Try alternative approach - find by position within current page
                        var pageStart = pageInfo.page * pageInfo.length;
                        var positionInPage = rowIndex - pageStart;
                        console.log('Alternative: Position in page:', positionInPage);
                        
                        if (positionInPage >= 0 && positionInPage < currentPageRows.length) {
                          var altRowNode = currentPageRows[positionInPage];
                          console.log('Alternative row node:', altRowNode);
                          if (altRowNode) {
                            $(altRowNode).addClass('selected');
                            console.log('SUCCESS: Selected row using alternative method');
                          }
                        }
                      }
                    };
                    
                    // If the row is not on the current page, navigate to it first
                    if (rowPage !== pageInfo.page) {
                      console.log('Navigating from page', pageInfo.page, 'to page', rowPage);
                      self.page(rowPage).draw(false);
                      
                      // Wait for the page to redraw before selecting
                      setTimeout(selectRow, 150);
                    } else {
                      // Row is on current page
                      selectRow();
                    }
                  }
                };
                console.log('Added programmatic selection method');
              }
              
              // Add to registry for future use
              if (!window.vegbankTables) window.vegbankTables = {};
              window.vegbankTables[message.tableId] = table;
              console.log('Added table to registry');
            }
          }
          
          if (table && typeof table.selectRowsProgrammatic === 'function') {
            table.selectRowsProgrammatic(message.rowIndex);
            console.log('Successfully selected row', message.rowIndex, 'in table', message.tableId);
          } else {
            console.warn('Table not found or method not available for:', message.tableId);
            console.log('Available tables:', Object.keys(window.vegbankTables || {}));
            console.log('Table object:', table);
          }
        } catch (e) {
          console.warn('Could not select table row:', e);
        }
      }, 200);
    });

    Shiny.addCustomMessageHandler('selectTableRowByAccession', function(message) {
      console.log('Selecting row by accession code:', message.accessionCode, 'in table:', message.tableId);
      
      var trySelect = function(attempt) {
        attempt = attempt || 1;
        var maxAttempts = 10;
        
        try {
          // Clear all selections from all tables first
          $('table tbody tr').removeClass('selected');
          console.log('Cleared all existing selections (attempt ' + attempt + ')');
          
          // Find the button with the specific accession code
          var buttonSelector = 'button[onclick*=\"' + message.accessionCode.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&') + '\"]';
          var targetButton = $(buttonSelector);
          
          console.log('Looking for button with selector:', buttonSelector);
          console.log('Found buttons:', targetButton.length);
          
          if (targetButton.length > 0) {
            // Get the row containing the button
            var targetRow = targetButton.closest('tr');
            console.log('Found target row:', targetRow.length);
            
            if (targetRow.length > 0) {
              // Add the selected class
              targetRow.addClass('selected');
              console.log('SUCCESS: Added selected class to row with accession code:', message.accessionCode);
              console.log('Row classes:', targetRow.attr('class'));
              return true;
            } else {
              console.warn('Could not find row containing the button');
            }
          } else {
            console.warn('Could not find button with accession code:', message.accessionCode);
            
            // If button not found and we haven't exceeded max attempts, retry
            if (attempt < maxAttempts) {
              console.log('Retrying selection in 500ms (attempt ' + (attempt + 1) + ' of ' + maxAttempts + ')');
              setTimeout(function() {
                trySelect(attempt + 1);
              }, 500);
              return false;
            }
          }
        } catch (e) {
          console.warn('Error selecting table row by accession (attempt ' + attempt + '):', e);
          if (attempt < maxAttempts) {
            setTimeout(function() {
              trySelect(attempt + 1);
            }, 500);
          }
        }
        
        return false;
      };
      
      // Start the selection attempt
      setTimeout(function() {
        trySelect(1);
      }, 100);
    });

    Shiny.addCustomMessageHandler('clearAllTableSelections', function(message) {
      // Simply remove selected class from all table rows
      $('table tbody tr').removeClass('selected');
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

      console.log('Updating detail type to:', type);

      if (plotCards && communityConceptCards && communityClassificationCards &&
      taxonObservationCards && projectCards && partyCards) {
        // Hide all card types first
        plotCards.style.display = 'none';
        communityConceptCards.style.display = 'none';
        communityClassificationCards.style.display = 'none';
        taxonObservationCards.style.display = 'none';
        projectCards.style.display = 'none';
        partyCards.style.display = 'none';

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
    bslib::nav_panel(title = "Plants"),
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
        )
      )
    )
  )
}
