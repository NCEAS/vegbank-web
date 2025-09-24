#' Map Constants Module
#'
#' Provides constants and default settings for the map functionality.

#' Get Map Defaults
#'
#' Returns a list of default settings for the leaflet map.
#'
#' @return A list containing default map settings
#' @noRd
get_map_defaults <- function() {
  list(
    lat = 39.8283,                  # Default center latitude (US center)
    lng = -98.5795,                 # Default center longitude (US center)
    zoom = 4,                       # Default zoom level
    detail_zoom = 14,               # Zoom level for detailed view
    min_zoom = 2,                   # Minimum allowed zoom level
    max_bounds = list(              # Map boundaries
      lng1 = -180,                  # Western limit
      lat1 = -85,                   # Southern limit
      lng2 = 180,                   # Eastern limit
      lat2 = 85                     # Northern limit
    )
  )
}

#' Initialize Map State
#'
#' Creates reactive values for tracking map state.
#'
#' @return A list of reactive values for map state
#' @importFrom shiny reactiveVal
#' @noRd
initialize_map_state <- function() {
  map_defaults <- get_map_defaults()

  list(
    map_center_lat = shiny::reactiveVal(map_defaults$lat),
    map_center_lng = shiny::reactiveVal(map_defaults$lng),
    map_zoom = shiny::reactiveVal(map_defaults$zoom)
  )
}