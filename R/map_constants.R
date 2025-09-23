#' Map Configuration Constants
#'
#' Central location for map-related configuration values to avoid duplication
#' across modules.

#' Default map settings for initial view
#' @noRd
MAP_DEFAULTS <- list(
  # Geographic center of the continental United States
  lat = 39.8283,
  lng = -98.5795,
  zoom = 2,
  min_zoom = 2,
  # Zoom level when focusing on a specific plot
  detail_zoom = 18,
  max_bounds = list(
    lng1 = -180, lat1 = -85,
    lng2 = 180, lat2 = 85
  )
)

#' Get default map center latitude
#' @return Numeric latitude value
#' @noRd
get_default_map_lat <- function() {
  MAP_DEFAULTS$lat
}

#' Get default map center longitude
#' @return Numeric longitude value
#' @noRd
get_default_map_lng <- function() {
  MAP_DEFAULTS$lng
}

#' Get default map zoom level
#' @return Numeric zoom value
#' @noRd
get_default_map_zoom <- function() {
  MAP_DEFAULTS$zoom
}

#' Get all default map settings as a named list
#' @return List with lat, lng, zoom, min_zoom, and max_bounds
#' @noRd
get_map_defaults <- function() {
  MAP_DEFAULTS
}

#' Initialize reactive values for map state with defaults
#' @return List of reactive values initialized with map defaults
#' @importFrom shiny reactiveVal
#' @noRd
initialize_map_state <- function() {
  defaults <- get_map_defaults()
  list(
    map_center_lat = shiny::reactiveVal(defaults$lat),
    map_center_lng = shiny::reactiveVal(defaults$lng),
    map_zoom = shiny::reactiveVal(defaults$zoom)
  )
}
