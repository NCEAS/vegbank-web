#' Loading Overlay Components for Vegbank UI
#'
#' This module provides functions for building full-screen loading overlays
#' with animated spinners and rotating messages for various long-running
#' operations in the Vegbank application.
#'
#' @name loaders
#' @noRd
NULL

#' Build Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay with animated spinner and rotating messages.
#' This is a generalized component that can be used for map loading, downloads, or other
#' long-running operations.
#'
#' @param overlay_type A string identifier for this overlay (e.g., "map", "download").
#'   Used to generate unique element IDs.
#' @param default_title The default title text shown at the top of the overlay.
#' @param messages Character vector of messages to rotate through while loading.
#' @param completion_message Message to display when loading completes.
#' @param show_detail If TRUE, includes a detail line for showing counts or progress.
#' @param visible_on_load If TRUE, the overlay is displayed immediately on page load.
#'   Used to prevent FOUC (Flash of Unstyled Content) on initial app load.
#'
#' @return A Shiny tag representing the loading overlay.
#'
#' @noRd
build_loading_overlay <- function(overlay_type, default_title, messages, completion_message, show_detail = FALSE, visible_on_load = FALSE) {
  overlay_id <- paste0(overlay_type, "-loading-overlay")
  title_id <- paste0(overlay_type, "-loading-title")
  detail_id <- paste0(overlay_type, "-loading-detail")
  pun_id <- paste0(overlay_type, "-loading-pun")
  ellipses_class <- "loading-ellipses"

  initial_display <- if (visible_on_load) "flex" else "none"

  htmltools::tags$div(
    id = overlay_id,
    class = "loading-overlay",
    role = "alert",
    `aria-live` = "polite",
    `aria-busy` = "true",
    `data-messages` = jsonlite::toJSON(messages, auto_unbox = TRUE),
    `data-completion-message` = completion_message,
    style = sprintf("display: %s; position: fixed; top: var(--navbar-height); left: 0;
             width: 100vw; height: calc(100vh - var(--navbar-height));
             background: rgba(255, 255, 255, 0.98); z-index: 1200;
             justify-content: center; align-items: center; flex-direction: column;", initial_display),
    htmltools::tags$div(
      class = "loading-content",
      style = "text-align: center; margin-top: -5rem;",
      htmltools::tags$h2(
        id = title_id,
        style = "font-size: 0.875rem; color: var(--no-status-text); margin-bottom: 1.5rem;",
        default_title,
        if (show_detail) {
          htmltools::tags$span(
            id = detail_id,
            style = "font-size: 0.875rem; color: var(--no-status-text); margin-bottom: 1.5rem; display: none;"
          )
        }
      ),
      htmltools::tags$div(
        class = ellipses_class,
        htmltools::tags$div(htmltools::tags$div()),
        htmltools::tags$div(htmltools::tags$div()),
        htmltools::tags$div(htmltools::tags$div()),
        htmltools::tags$div(htmltools::tags$div())
      ),
      htmltools::tags$div(
        id = pun_id,
        style = "font-size: 1rem; color: var(--vb-green); font-weight: 500;"
      )
    )
  )
}

#' Build Map Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for initial map loading with animated
#' spinner and rotating plant puns.
#'
#' @param visible If TRUE, the overlay is displayed immediately on page load.
#'   Used to prevent flash of empty cards when loading the map for the first time.
#' @return A Shiny tag representing the map loading overlay.
#'
#' @noRd
build_map_loading_overlay <- function(visible = FALSE) {
  build_loading_overlay(
    overlay_type = "map",
    default_title = "Loading the map for the first time can take a few seconds. It's busy:",
    messages = c(
      "Planting seeds...",
      "Branching out...",
      "Rooting through the database...",
      "Monitoring mycelial networks...",
      "Looking through the leaf litter...",
      "Disturbing the substrate...",
      "Digging soil samples...",
      "Planning successful succession...",
      "Aggregating observations (there are a lot)...",
      "Braving the brush...",
      "Tackling the topography...",
      "Navigating the terrain...",
      "Extracting the latitudes and longitudes...",
      "Last bud not leaf..."
    ),
    completion_message = "Go fir launch!",
    visible_on_load = visible
  )
}

#' Build Overview Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for overview statistics with animated
#' spinner and rotating messages.
#'
#' @param visible If TRUE, the overlay is displayed immediately on page load.
#'   Used to prevent flash of empty cards when loading Overview page.
#' @return A Shiny tag representing the overview loading overlay.
#'
#' @noRd
build_overview_loading_overlay <- function(visible = FALSE) {
  build_loading_overlay(
    overlay_type = "overview",
    default_title = "Loading the overview can take a few seconds. Hold tight, we're:",
    messages = c(
      "Rooting through the database...",
      "Monitoring mycelial networks...",
      "Running multivariate analysis on plots...",
      "Disturbing the substrate...",
      "Counting all the plots (there are a lot)...",
      "Surveying the survey data...",
      "Celebrating our top contributors...",
      "Crunching the numbers...",
      "Classifying communities...",
      "Sorting through plant species...",
      "Compiling project data...",
      "Organizing taxonomic hierarchies..."
    ),
    completion_message = "Take a look!",
    visible_on_load = visible
  )
}

#' Build Download Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for data downloads with animated
#' spinner and rotating messages.
#'
#' @return A Shiny tag representing the download loading overlay.
#'
#' @noRd
build_download_loading_overlay <- function() {
  build_loading_overlay(
    overlay_type = "download",
    default_title = "Preparing your download:",
    messages = c(
      "Gathering plot observations...",
      "Untangling nested data...",
      "Pressing specimens into CSVs...",
      "Double-checking species IDs...",
      "Bundling the herbarium...",
      "Zipping up the collection..."
    ),
    completion_message = "Your data is ready!",
    show_detail = TRUE
  )
}

#' Build Citation Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for citation resolution with animated
#' spinner and rotating messages.
#'
#' @param visible If TRUE, the overlay is displayed immediately on page load.
#'   Used when navigating directly to a citation URL.
#' @return A Shiny tag representing the citation loading overlay.
#'
#' @noRd
build_citation_loading_overlay <- function(visible = FALSE) {
  build_loading_overlay(
    overlay_type = "citation",
    default_title = "Resolving citation...",
    messages = c(
      "Looking up identifier...",
      "Consulting the archives...",
      "Following the paper trail...",
      "Tracking down that reference...",
      "Dusting off old records...",
      "Pulling specimens from the herbarium..."
    ),
    completion_message = "Citation resolved!",
    visible_on_load = visible
  )
}
