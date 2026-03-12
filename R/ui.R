#' Shiny UI for VegBank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
ui <- function(req) {
  # Handle /cite/IDENTIFIER paths: --> HTTP 302 redirect --> /?cite=IDENTIFIER
  # Old VegBank had citation URLs like http://vegbank.org/cite/VB.Ob.22743.INW32086
  # This redirect converts path-based citations to query parameter form for server processing.
  # after the app loads.
  identifier <- extract_citation_identifier(req)
  if (!is.null(identifier)) {
    return(build_citation_redirect(identifier))
  }

  # Parse initial state from URL query parameters
  query_string <- req$QUERY_STRING
  initial_tab <- "Home"
  has_cite_param <- FALSE

  if (!is.null(query_string) && nzchar(query_string)) {
    query_params <- shiny::parseQueryString(query_string)
    if (!is.null(query_params$cite) && nzchar(query_params$cite)) {
      # Citation will be resolved server-side; start on Home with loading overlay
      has_cite_param <- TRUE
    } else if (!is.null(query_params$tab) && nzchar(query_params$tab)) {
      initial_tab <- query_params$tab
    }
  }

  shiny::addResourcePath("assets", system.file("shiny/www", package = "vegbankweb"))

  # Ensure Inter font loads from CDN before any CSS
  font_head <- htmltools::tags$head(
    htmltools::tags$link(rel = "preconnect", href = "https://rsms.me/"),
    htmltools::tags$link(rel = "stylesheet", href = "https://rsms.me/inter/inter.css"),
    htmltools::tags$link(rel = "stylesheet", href = "assets/vegbank_styles.css")
  )

  navbar <- build_navbar(initial_tab)
  overlay <- build_detail_overlay()
  map_loading_overlay <- build_map_loading_overlay(
    visible = identical(initial_tab, "Map")
  )
  overview_loading_overlay <- build_overview_loading_overlay(
    visible = identical(initial_tab, "Overview") && !has_cite_param
  )
  citation_loading_overlay <- build_citation_loading_overlay(
    visible = has_cite_param
  )
  download_loading_overlay <- build_download_loading_overlay()

  # Inline script with application constants injected from R
  constants_script <- htmltools::tags$script(htmltools::HTML(paste0(
    "// Application constants - single source of truth from R\n",
    "window.DOWNLOAD_MAX_RECORDS = ", DOWNLOAD_MAX_RECORDS, ";\n"
  )))

  # External JavaScript file with main application logic
  app_script <- htmltools::tags$script(src = "assets/vegbank_app.js")

  htmltools::tagList(
    font_head,
    navbar,
    overlay,
    map_loading_overlay,
    overview_loading_overlay,
    citation_loading_overlay,
    download_loading_overlay,
    constants_script,
    app_script
  )
}

#' Custom Bootstrap Theme for VegBank Web Application
#'
#' Defines a bslib theme with custom rules.
#'
#' @return A Bootstrap theme object.
#'
#' @noRd
# Bootstrap theme configuration
# Custom CSS styles are in inst/shiny/www/vegbank-styles.css
custom_theme <- bslib::bs_theme(
  bg = "hsl(0, 0%, 100%)",
  fg = "hsl(156, 12%, 11%)",
  info = "hsl(160, 69%, 30%)",
  # WCAG AA requires ≥ 4.5:1 contrast for normal text.
  # hsl(160, 29%, 40%) only achieved ~4.07:1 against white.
  # Dropping lightness to 35% raises that to ~4.97:1, keeping
  # white-on-primary filled buttons at the same ratio (both pass).
  primary = "hsl(165, 41%, 34%)",
  # secondary at 59% lightness (~3.3:1) also fails if used as text,
  # darkened to 42% (~4.56:1) as a proactive fix.
  secondary = "hsl(160, 34%, 42%)",
  base_font = bslib::font_collection("Inter", "InterVariable", "system-ui", "sans-serif"),
  heading_font = bslib::font_collection("Inter", "InterVariable", "system-ui", "sans-serif"),
  "font-size-base" = "0.875rem"
)

#' Build Navigation Bar for VegBank UI
#'
#' Constructs and returns the navigation bar to be used in the UI.
#'
#' @param initial_tab Character string specifying which tab to show initially
#' @return A Shiny tag list representing the navigation bar.
#'
#' @noRd
build_navbar <- function(initial_tab = "Home") {
  navbar <- bslib::page_navbar(
    id = "page",
    selected = initial_tab,
    theme = custom_theme,
    title = htmltools::tags$span(
      htmltools::tags$img(
        src = "assets/logo_vegbank_leaves.svg"
      ),
      "VegBank"
    ),
    build_home_panel(),
    bslib::nav_panel(
      title = "Overview",
      shiny::fluidPage(
        bslib::layout_columns(
          col_widths = bslib::breakpoints(
            sm = 12,
            md = 6,
            lg = 4
          ),
          bslib::card(
            bslib::card_header("Data in VegBank"),
            bslib::card_body(
              shiny::uiOutput("core_counts_list")
            )
          ),
          bslib::card(
            bslib::card_header("Recently Updated Projects"),
            bslib::card_body(
              shiny::uiOutput("latest_projects_table")
            )
          ),
          bslib::card(
            bslib::card_header("Projects with Most Plots"),
            bslib::card_body(
              shiny::uiOutput("top_projects_plot")
            )
          ),
          bslib::card(
            bslib::card_header("Common Communities"),
            bslib::card_body(
              shiny::uiOutput("top_communities_plot")
            )
          ),
          bslib::card(
            bslib::card_header("Common Plants"),
            bslib::card_body(
              shiny::uiOutput("top_plants_plot")
            )
          ),
          bslib::card(
            bslib::card_header("Top Contributors"),
            bslib::card_body(
              shiny::uiOutput("top_contributors_plot")
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
        shiny::uiOutput("plot_filter_alert"),
        # Hidden download button triggered by DT button click
        # Use position absolute and move off-screen instead of display:none
        # so Shiny properly initializes the href attribute
        htmltools::tags$div(
          style = "position: absolute; left: -9999px; top: -9999px;",
          shiny::downloadButton("download_plot_table", "Download")
        ),
        DT::dataTableOutput("plot_table")
      )
    ),
    bslib::nav_panel(
      title = "Plants",
      shiny::fluidPage(
        DT::dataTableOutput("plant_table")
      )
    ),
    bslib::nav_panel(
      title = "Communities",
      shiny::fluidPage(
        shiny::uiOutput("comm_filter_alert"),
        DT::dataTableOutput("comm_table")
      )
    ),
    bslib::nav_panel(
      title = "Parties",
      shiny::fluidPage(
        DT::dataTableOutput("party_table")
      )
    ),
    bslib::nav_panel(
      title = "Projects",
      shiny::fluidPage(
        DT::dataTableOutput("proj_table")
      )
    ),
    bslib::nav_menu(
      title = "About",
      align = "right",
      bslib::nav_panel(
        title = "Getting Started",
        htmltools::tags$div(
          class = "vb-markdown-page",
          shiny::includeMarkdown(system.file("shiny", "www", "getting_started.md", package = "vegbankweb"))
        )
      ),
      bslib::nav_panel(
        title = "FAQ",
        htmltools::tags$div(
          class = "vb-markdown-page",
          shiny::includeMarkdown(system.file("shiny", "www", "faq.md", package = "vegbankweb"))
        )
      ),
      bslib::nav_panel(
        title = "Cite",
        htmltools::tags$div(
          class = "vb-markdown-page",
          shiny::includeMarkdown(system.file("shiny", "www", "cite.md", package = "vegbankweb"))
        )
      )
    )
  )
}

#' Build Detail Overlay for VegBank UI
#'
#' Constructs the overlay panel that displays detailed plot information.
#'
#' @return A Shiny tag representing the detail overlay.
#'
#' @noRd
build_detail_overlay <- function() {
  htmltools::tags$div(
    id = "detail-overlay",
    shiny::actionButton("close_overlay", "",
      onclick = "var overlay = document.getElementById('detail-overlay');
                 overlay.classList.add('closed');
                 document.body.classList.remove('overlay-open');
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
          shiny::uiOutput("plot_notification"),
          bslib::card(bslib::card_header("Plot Observation"), shiny::uiOutput("plot_header")),
          bslib::card(bslib::card_header("Author Codes"), shiny::uiOutput("author_code_details")),
          bslib::card(bslib::card_header("Dates"), shiny::uiOutput("date_details")),
          bslib::card(bslib::card_header("Location"), shiny::uiOutput("location_details")),
          bslib::card(bslib::card_header("Layout"), shiny::uiOutput("layout_details")),
          bslib::card(bslib::card_header("Environment"), shiny::uiOutput("environmental_details")),
          bslib::card(bslib::card_header("Methods"), shiny::uiOutput("methods_details")),
          bslib::card(bslib::card_header("Plot Quality"), shiny::uiOutput("plot_quality_details")),
          bslib::card(bslib::card_header("Plot Vegetation"), shiny::uiOutput("plot_vegetation_details")),
          bslib::card(bslib::card_header("Communities"), shiny::uiOutput("communities_details")),
          bslib::card(bslib::card_header("Taxa Observed"), shiny::uiOutput("taxa_details")),
          bslib::card(bslib::card_header("Disturbances"), shiny::uiOutput("disturbances_details")),
          bslib::card(bslib::card_header("Soils"), shiny::uiOutput("soils_details")),
          bslib::card(bslib::card_header("Miscellaneous"), shiny::uiOutput("plot_misc_details"))
        ),

        # Community Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Community Concept"), shiny::uiOutput("community_concept_header")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("community_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("community_party_perspective"))
        ),

        # Community Classification Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-classification-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Community Classification"), shiny::uiOutput("comm_class_header")),
          bslib::card(bslib::card_header("Classification Details"), shiny::uiOutput("comm_class_details")),
          bslib::card(bslib::card_header("Community Interpretations"), shiny::uiOutput("comm_class_interpretations")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("comm_class_contributors"))
        ),

        # Project Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "project-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Project"), shiny::uiOutput("project_header")),
          bslib::card(bslib::card_header("Description"), shiny::uiOutput("project_description")),
          bslib::card(bslib::card_header("Dates"), shiny::uiOutput("project_dates")),
          bslib::card(bslib::card_header("Plot Observation Count"), shiny::uiOutput("project_observations")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("project_contributors"))
        ),

        # Party Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "party-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Party"), shiny::uiOutput("party_header")),
          bslib::card(bslib::card_header("Organization"), shiny::uiOutput("party_organization")),
          bslib::card(bslib::card_header("Contact Information"), shiny::uiOutput("party_contact")),
          bslib::card(bslib::card_header("Contributions"), shiny::uiOutput("party_contributions"))
        ),

        # Plant Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plant-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Plant Concept"), shiny::uiOutput("plant_concept_header")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("plant_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("plant_party_perspective"))
        ),

        # Reference Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "reference-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Reference"), shiny::uiOutput("reference_header")),
          bslib::card(bslib::card_header("Identifiers"), shiny::uiOutput("reference_identifiers")),
          bslib::card(bslib::card_header("Publication"), shiny::uiOutput("reference_publication"))
        ),

        # Cover Method Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "cover-method-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Cover Method"), shiny::uiOutput("cover_method_header")),
          bslib::card(bslib::card_header("Details"), shiny::uiOutput("cover_method_details")),
          bslib::card(bslib::card_header("Cover Indexes"), shiny::uiOutput("cover_method_indexes"))
        ),

        # Stratum Method Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "stratum-method-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Stratum Method"), shiny::uiOutput("stratum_method_header")),
          bslib::card(bslib::card_header("Details"), shiny::uiOutput("stratum_method_details")),
          bslib::card(bslib::card_header("Stratum Types"), shiny::uiOutput("stratum_types"))
        )
      )
    )
  )
}

# ================= CITATION REDIRECT HELPERS ======================================================

#' Extract Citation Identifier from /cite/ Path
#'
#' Checks if the request path is a /cite/IDENTIFIER URL and returns the identifier.
#' Only checks path-based citations — query-based (?cite=) citations are handled
#' by the server after the app loads.
#'
#' @param req A Shiny request object
#' @return The citation identifier string, or NULL if not a /cite/ path
#' @noRd
extract_citation_identifier <- function(req) {
  path_info <- req$PATH_INFO
  if (!is.null(path_info) && length(path_info) > 0 && grepl("^/cite/", path_info)) {
    identifier <- sub("^/cite/", "", path_info)
    if (nzchar(identifier)) {
      return(identifier)
    }
  }
  NULL
}

#' Build an HTTP 302 Redirect for Citation URLs
#'
#' Returns an HTTP 302 response that redirects /cite/IDENTIFIER to /?cite=IDENTIFIER.
#' This is an immediate server-level redirect — no HTML is rendered and no resources
#' are loaded — which avoids the problem of relative asset paths being misinterpreted
#' as citation identifiers when uiPattern = ".*" routes all requests through ui().
#'
#' @param identifier The citation identifier to redirect
#' @return A Shiny httpResponse object with status 302
#' @noRd
build_citation_redirect <- function(identifier) {
  encoded_id <- utils::URLencode(identifier, reserved = TRUE)
  shiny::httpResponse(
    status = 302L,
    headers = list(
      Location = paste0("/?cite=", encoded_id)
    ),
    content = ""
  )
}
