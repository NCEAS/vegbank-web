#' Shiny UI for VegBank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
ui <- function(req) {
  # Reject any path that isn't the app root or a /cite/ redirect with a 404 page.
  path_info <- req$PATH_INFO
  if (!is.null(path_info) && nzchar(path_info) &&
    !identical(path_info, "/") &&
    !grepl("^/cite/", path_info, perl = TRUE)) {
    return(shiny::httpResponse(
      status = 404L,
      content_type = "text/html; charset=utf-8",
      content = .html_404
    ))
  }

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

  # Ensure Inter font loads from CDN before any CSS
  font_head <- htmltools::tags$head(
    htmltools::tags$link(rel = "icon", type = "image/svg+xml", href = "assets/logo_vegbank_leaves.svg"),
    htmltools::tags$link(rel = "preconnect", href = "https://rsms.me/"),
    htmltools::tags$link(rel = "stylesheet", href = "https://rsms.me/inter/inter.css"),
    htmltools::tags$link(rel = "stylesheet", href = "assets/vegbank_styles.css")
  )

  navbar <- build_navbar(initial_tab)
  map_loading_overlay <- .map_loading_overlays[[as.character(identical(initial_tab, "Map"))]]
  overview_loading_overlay <- .overview_loading_overlays[[as.character(identical(initial_tab, "Overview") && !has_cite_param)]]
  citation_loading_overlay <- .citation_loading_overlays[[as.character(has_cite_param)]]

  htmltools::tagList(
    font_head,
    navbar,
    .overlay,
    map_loading_overlay,
    overview_loading_overlay,
    citation_loading_overlay,
    .download_loading_overlay,
    .constants_script,
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
  primary = "hsl(165, 41%, 34%)",
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
#' @import markdown
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
          .md_getting_started
        )
      ),
      bslib::nav_panel(
        title = "FAQ",
        htmltools::tags$div(
          class = "vb-markdown-page",
          .md_faq
        )
      ),
      bslib::nav_panel(
        title = "Citing Data",
        htmltools::tags$div(
          class = "vb-markdown-page",
          .md_cite
        )
      ),
      bslib::nav_panel(
        title = "Downloading Plots",
        htmltools::tags$div(
          class = "vb-markdown-page",
          .md_download
        )
      ),
      bslib::nav_item(
        htmltools::tags$a(
          "Uploading Plots",
          href = "https://nceas.github.io/vegbankr/articles/upload.html",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      bslib::nav_item(
        htmltools::tags$a(
          "vegbankr Package",
          href = "https://nceas.github.io/vegbankr/index.html",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      bslib::nav_item(
        htmltools::tags$a(
          "VegBank REST API",
          href = "https://nceas.github.io/vegbank2/api/",
          target = "_blank",
          rel = "noopener noreferrer"
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
    htmltools::tags$div(
      id = "detail-type-banner",
      htmltools::tags$span(
        class = "detail-type-icon",
        id = "detail-type-icon",
        `aria-hidden` = "true"
      ),
      htmltools::tags$span(
        class = "detail-type-label",
        id = "detail-type-label"
      ),
      shiny::actionButton("close_overlay",
        htmltools::HTML(.BTN_ICON_CLOSE),
        onclick = "var overlay = document.getElementById('detail-overlay');
                   overlay.classList.add('closed');
                   document.body.classList.remove('overlay-open');
                   Shiny.setInputValue('close_details', true, {priority:'event'});",
        class = "vb-close-btn",
        `aria-label` = "Close details",
        title = "Close details"
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        # Plot Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plot-details-cards",
          class = "detail-section",
          shiny::uiOutput("plot_notification"),
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("plot_header")),
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
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("community_concept_header")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("community_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("community_party_perspective"))
        ),

        # Community Classification Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-classification-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("comm_class_header")),
          bslib::card(bslib::card_header("Classification Details"), shiny::uiOutput("comm_class_details")),
          bslib::card(bslib::card_header("Community Interpretations"), shiny::uiOutput("comm_class_interpretations")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("comm_class_contributors"))
        ),

        # Project Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "project-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("project_header")),
          bslib::card(bslib::card_header("Description"), shiny::uiOutput("project_description")),
          bslib::card(bslib::card_header("Dates"), shiny::uiOutput("project_dates")),
          bslib::card(bslib::card_header("Plot Observation Count"), shiny::uiOutput("project_observations")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("project_contributors"))
        ),

        # Party Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "party-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("party_header")),
          bslib::card(bslib::card_header("Organization"), shiny::uiOutput("party_organization")),
          bslib::card(bslib::card_header("Contact Information"), shiny::uiOutput("party_contact")),
          bslib::card(bslib::card_header("Contributions"), shiny::uiOutput("party_contributions"))
        ),

        # Plant Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plant-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("plant_concept_header")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("plant_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("plant_party_perspective"))
        ),

        # Reference Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "reference-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("reference_header")),
          bslib::card(bslib::card_header("Identifiers"), shiny::uiOutput("reference_identifiers")),
          bslib::card(bslib::card_header("Publication"), shiny::uiOutput("reference_publication"))
        ),

        # Cover Method Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "cover-method-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Name"), shiny::uiOutput("cover_method_header")),
          bslib::card(bslib::card_header("Details"), shiny::uiOutput("cover_method_details")),
          bslib::card(bslib::card_header("Cover Indexes"), shiny::uiOutput("cover_method_indexes"))
        ),

        # Stratum Method Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "stratum-method-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Name"), shiny::uiOutput("stratum_method_header")),
          bslib::card(bslib::card_header("Details"), shiny::uiOutput("stratum_method_details")),
          bslib::card(bslib::card_header("Stratum Types"), shiny::uiOutput("stratum_types"))
        ),

        # Taxon Observation Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "taxon-observation-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("taxon_obs_header")),
          bslib::card(bslib::card_header("Observation Details"), shiny::uiOutput("taxon_obs_details")),
          bslib::card(bslib::card_header("Taxon Importance"), shiny::uiOutput("taxon_obs_importance")),
          bslib::card(bslib::card_header("Taxon Interpretations"), shiny::uiOutput("taxon_obs_interpretations"))
        ),

        # User Dataset Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "user-dataset-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Showing"), shiny::uiOutput("dataset_header")),
          bslib::card(bslib::card_header("Dataset Details"), shiny::uiOutput("dataset_details")),
          bslib::card(bslib::card_header("Citation"), shiny::uiOutput("dataset_citation"))
        )
      )
    )
  )
}

# ================= PACKAGE-LEVEL PRECOMPUTED CONSTANTS ===========================================
# External JS tag for the app script which won't change per request.
app_script <- htmltools::tags$script(src = "assets/vegbank_app.js")

# About submenu markdown pages. The pages areread from disk once at package load.
.md_getting_started <- shiny::includeMarkdown(system.file("shiny", "www", "getting_started.md", package = "vegbankweb"))
.md_faq <- shiny::includeMarkdown(system.file("shiny", "www", "faq.md", package = "vegbankweb"))
.md_cite <- shiny::includeMarkdown(system.file("shiny", "www", "cite.md", package = "vegbankweb"))
.md_download <- shiny::includeMarkdown(system.file("shiny", "www", "download.md", package = "vegbankweb"))

# Detail overlay sidebar. A static HTML structure whose uiOutput placeholders are
# filled reactively by the server. Built once since it has no request-specific inputs.
.overlay <- build_detail_overlay()

# Download loading overlay. Also no request-specific inputs.
.download_loading_overlay <- build_download_loading_overlay()

# Per-request loading overlays have only two possible states (visible = TRUE/FALSE).
# Precompute both so ui() does a list lookup instead of constructing tag trees.
.map_loading_overlays <- list(
  "FALSE" = build_map_loading_overlay(visible = FALSE),
  "TRUE"  = build_map_loading_overlay(visible = TRUE)
)
.overview_loading_overlays <- list(
  "FALSE" = build_overview_loading_overlay(visible = FALSE),
  "TRUE"  = build_overview_loading_overlay(visible = TRUE)
)
.citation_loading_overlays <- list(
  "FALSE" = build_citation_loading_overlay(visible = FALSE),
  "TRUE"  = build_citation_loading_overlay(visible = TRUE)
)

# Inline <script> tag injecting R constants into the browser. Built once at package load time
# since every value it depends on is a package-level constant.
.constants_script <- local({
  detail_label_entries <- Filter(function(e) !is.null(e$detail_type), RESOURCE_REGISTRY)
  detail_labels_js <- paste(
    vapply(detail_label_entries, function(e) {
      paste0("  '", e$detail_type, "': '", tools::toTitleCase(e$singular), "'")
    }, character(1)),
    collapse = ",\n"
  )
  htmltools::tags$script(htmltools::HTML(paste0(
    "// Application constants - single source of truth from R\n",
    "window.DOWNLOAD_MAX_RECORDS = ", DOWNLOAD_MAX_RECORDS, ";\n",
    "window.DETAIL_TYPE_LABELS = {\n", detail_labels_js, "\n};\n",
    "window.DETAIL_ICONS = ", jsonlite::toJSON(DETAIL_ICONS, auto_unbox = TRUE), ";\n",
    "window.VB_VALID_CONCEPT_STATUSES = ", jsonlite::toJSON(VALID_CONCEPT_STATUSES), ";\n",
    "window.VB_VALID_PLOT_STATUSES = ", jsonlite::toJSON(VALID_PLOT_STATUSES), ";\n",
    "window.VB_DEFAULT_CONCEPT_STATUS = ", jsonlite::toJSON(DEFAULT_CONCEPT_STATUS, auto_unbox = TRUE), ";\n",
    "window.VB_DEFAULT_PLOT_STATUS = ", jsonlite::toJSON(DEFAULT_PLOT_STATUS, auto_unbox = TRUE), ";\n"
  )))
})

# 404 response page. Built once; returned for any unrecognised path. Contains hard-coded
# css values that may need updating to match any theme changes.
.html_404 <- paste0(
  "<!DOCTYPE html>",
  "<html lang='en'>",
  "<head>",
  "<meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>404: Page Not Found</title>",
  "<style>",
  "body{margin:0;font-family:system-ui,sans-serif;background:#f7faf9;color:#1a2e28;",
  "display:flex;align-items:center;justify-content:center;min-height:100vh;text-align:center;}",
  ".box{max-width:420px;padding:2.5rem 2rem;}",
  "h1{font-size:5rem;margin:0;color:#2a7a5c;line-height:1;}",
  "h2{font-size:1.25rem;margin:.5rem 0 1.25rem;font-weight:600;}",
  "p{margin:0 0 1.75rem;color:#4a6860;line-height:1.6;}",
  "a{display:inline-block;padding:.55rem 1.4rem;background:#2a7a5c;color:#fff;",
  "border-radius:.375rem;text-decoration:none;font-weight:500;}",
  "a:hover{background:#1f5e46;}",
  "</style>",
  "</head>",
  "<body>",
  "<div class='box'>",
  "<h1>404</h1>",
  "<h2>Looks like this page has gone to seed.</h2>",
  "<p>We couldn't find what you were looking for.<br>",
  "It may have wilted away or never taken root.</p>",
  "<a href='/'>Back to VegBank</a>",
  "</div>",
  "</body>",
  "</html>"
)

# ================= CITATION REDIRECT HELPERS ======================================================

#' Extract Citation Identifier from /cite/ Path
#'
#' Checks if the request path is a /cite/IDENTIFIER URL and returns the identifier.
#' Only checks path-based citations. Query-based (?cite=) citations are handled
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
#' This is an immediate server-level redirect (no HTML is rendered and no resources
#' are loaded) which avoids the problem of relative asset paths being misinterpreted
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
