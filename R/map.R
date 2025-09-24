#' Map Functions Module
#'
#' Provides functions for creating and manipulating leaflet maps.

# ---- App-facing functions ----

#' Process map data and return a leaflet map object
#'
#' Creates a leaflet map with clustered markers from plot observation data.
#' This function handles the entire process from data validation to map rendering.
#'
#' @param map_data Data frame with map points. Must contain latitude, longitude,
#'                author_obs_code and obs_accession_code columns.
#' @param center_lng Longitude for map center (numeric)
#' @param center_lat Latitude for map center (numeric)
#' @param zoom Initial zoom level (integer)
#' @param map_options Optional list of map configuration options to override defaults
#' @returns A leaflet map object containing clustered markers for each plot location with a list of
#'          observations in their labels
#'
#' @importFrom leaflet leaflet leafletOptions setMaxBounds addTiles addMarkers markerClusterOptions
#' @importFrom dplyr arrange group_by summarize n
#' @importFrom htmltools HTML
#' @importFrom shiny withProgress incProgress showNotification
#' @importFrom htmlwidgets onRender
#' @noRd
process_map_data <- function(map_data, center_lng, center_lat, zoom, map_options = NULL) {
  # Get map defaults and possibly override with provided options
  map_defaults <- get_map_defaults()
  if (!is.null(map_options)) {
    map_defaults <- utils::modifyList(map_defaults, map_options)
  }

  shiny::withProgress(
    expr = {
      # Validate input data
      if (is_invalid_map_data(map_data)) {
        shiny::showNotification(
          "Missing required data. Please try again or check your connection.",
          type = "error"
        )
        return(create_empty_map(map_defaults, center_lng, center_lat, zoom))
      }

      # Process data for mapping
      shiny::incProgress(0.2, detail = "Filtering valid points...")
      valid_points <- filter_valid_map_points(map_data)

      if (nrow(valid_points) == 0) {
        shiny::incProgress(0.8, detail = "No valid points found")
        return(create_empty_map(map_defaults, center_lng, center_lat, zoom))
      }

      # Group points by location
      shiny::incProgress(0.3, detail = "Grouping plots by location...")
      data_grouped <- group_map_points(valid_points)

      # Build the map
      shiny::incProgress(0.3, detail = "Building map...")
      build_leaflet_map(data_grouped, map_defaults, center_lng, center_lat, zoom)
    },
    message = "Processing map data",
    value = 0
  )
}

#' Move the map and show a popup at a given location
#'
#' Updates a leaflet map proxy by changing the view and adding a popup at the
#' specified location.
#'
#' @param map_proxy A leaflet proxy object
#' @param lng Longitude (numeric)
#' @param lat Latitude (numeric)
#' @param label Popup content (HTML or text)
#' @param zoom Zoom level (integer, optional). If NULL, uses detail_zoom from map defaults.
#' @returns A leaflet map proxy with updated view and popup
#'
#' @importFrom leaflet setView clearPopups addPopups
#' @noRd
update_map_view <- function(map_proxy, lng, lat, label, zoom = NULL) {
  # Input validation
  if (!is.numeric(lng) || !is.numeric(lat)) {
    warning("Invalid coordinates provided to update_map_view")
    return(map_proxy)
  }

  if (is.null(zoom)) {
    zoom <- get_map_defaults()$detail_zoom
  }

  map_proxy |>
    leaflet::setView(lng = lng, lat = lat, zoom = zoom) |>
    leaflet::clearPopups() |>
    leaflet::addPopups(
      lng = lng,
      lat = lat,
      popup = label
    )
}

# ---- Data processing functions ----

#' Check if map data is invalid
#'
#' @param map_data Data frame to check
#' @return TRUE if data is invalid, FALSE otherwise
#' @noRd
is_invalid_map_data <- function(map_data) {
  is.null(map_data) ||
    nrow(map_data) == 0 ||
    !all(c("latitude", "longitude", "author_obs_code", "obs_accession_code") %in% colnames(map_data))
}

#' Filter map data to only include points with valid coordinates
#'
#' @param map_data Data frame with map points
#' @return Filtered data frame containing only rows with valid coordinates
#' @noRd
filter_valid_map_points <- function(map_data) {
  subset(map_data, !is.na(map_data$latitude) &
    !is.na(map_data$longitude) &
    is.numeric(map_data$latitude) &
    is.numeric(map_data$longitude))
}

#' Group map points by location and create marker content
#'
#' @param valid_points Data frame with valid map points
#' @return Data frame grouped by latitude and longitude with marker content
#' @importFrom dplyr arrange group_by summarize n
#' @noRd
group_map_points <- function(valid_points) {
  valid_points |>
    dplyr::arrange(.data$author_obs_code) |>
    dplyr::group_by(.data$latitude, .data$longitude) |>
    dplyr::summarize(
      obs_count = dplyr::n(),
      author_obs_code_label = create_marker_popup(
        .data$author_obs_code,
        .data$obs_accession_code,
        dplyr::n()
      ),
      .groups = "drop"
    )
}

# ---- Map building functions ----

#' Build a leaflet map with the provided data
#'
#' @param data_grouped Grouped data frame with map points
#' @param map_defaults List of map default settings
#' @param center_lng Longitude for map center
#' @param center_lat Latitude for map center
#' @param zoom Initial zoom level
#' @return A leaflet map object
#' @importFrom leaflet leaflet leafletOptions setMaxBounds addTiles addMarkers markerClusterOptions
#' @importFrom htmltools HTML
#' @noRd
build_leaflet_map <- function(data_grouped, map_defaults, center_lng, center_lat, zoom) {
  leaflet::leaflet(
    data_grouped,
    options = leaflet::leafletOptions(minZoom = map_defaults$min_zoom)
  ) |>
    leaflet::setMaxBounds(
      lng1 = map_defaults$max_bounds$lng1,
      lat1 = map_defaults$max_bounds$lat1,
      lng2 = map_defaults$max_bounds$lng2,
      lat2 = map_defaults$max_bounds$lat2
    ) |>
    leaflet::addTiles() |>
    leaflet::setView(lng = center_lng, lat = center_lat, zoom = zoom) |>
    leaflet::addMarkers(
      lng = ~longitude,
      lat = ~latitude,
      layerId = ~ paste(latitude, longitude, sep = ", "),
      label = ~ author_obs_code_label |> lapply(htmltools::HTML),
      labelOptions = create_marker_label_options(),
      clusterOptions = leaflet::markerClusterOptions(disableClusteringAtZoom = 17)
    ) |>
    add_zoom_control()
}

#' Create marker label options for consistent styling
#'
#' @return A labelOptions object
#' @importFrom leaflet labelOptions
#' @noRd
create_marker_label_options <- function() {
  leaflet::labelOptions(
    noHide = TRUE,
    clickable = TRUE,
    direction = "bottom",
    style = list(
      "color" = "#2c5443",
      "font-weight" = "bold",
      "padding" = "3px 8px",
      "background" = "white",
      "border" = "1px solid #2c5443",
      "border-radius" = "3px"
    )
  )
}

#' Create an empty leaflet map with a 'Data unavailable' message
#'
#' @param map_defaults List of map default settings
#' @param center_lng Longitude for map center
#' @param center_lat Latitude for map center
#' @param zoom Initial zoom level
#' @return An empty leaflet map with a message indicating that data is unavailable
#'
#' @importFrom leaflet leaflet leafletOptions setMaxBounds addTiles addControl
#' @noRd
create_empty_map <- function(map_defaults = NULL, center_lng = NULL, center_lat = NULL, zoom = NULL) {
  if (is.null(map_defaults)) {
    map_defaults <- get_map_defaults()
  }

  map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = map_defaults$min_zoom)) |>
    leaflet::setMaxBounds(
      lng1 = map_defaults$max_bounds$lng1,
      lat1 = map_defaults$max_bounds$lat1,
      lng2 = map_defaults$max_bounds$lng2,
      lat2 = map_defaults$max_bounds$lat2
    ) |>
    leaflet::addTiles() |>
    leaflet::addControl("Data unavailable", position = "topright")

  if (!is.null(center_lng) && !is.null(center_lat) && !is.null(zoom)) {
    map <- map |> leaflet::setView(lng = center_lng, lat = center_lat, zoom = zoom)
  }

  return(map)
}

#' Create a marker popup HTML for a group of observations
#'
#' @param obs_codes Character vector of author observation codes
#' @param accession_codes Character vector of plot observation accession codes
#' @param count Integer, number of observations for that plot
#' @return A string containing HTML for the popup label
#' @noRd
create_marker_popup <- function(obs_codes, accession_codes, count) {
  # Create header text
  header <- ifelse(count == 1, "1 Observation", paste(count, "Observations"))

  # Create list of links
  links <- mapply(
    function(obs, acc) {
      sprintf(
        "<a href=\"#\" onclick=\"Shiny.setInputValue('label_link_click',\n '%s', {priority: 'event'})\">%s</a>",
        acc, obs
      )
    },
    obs_codes, accession_codes
  )

  # Combine into a single HTML string
  paste0(
    "<strong>", header, "</strong>",
    "<div style='max-height: 15.5rem; overflow-y: auto;' onwheel='event.stopPropagation()'",
    " onmousewheel='event.stopPropagation()' onDOMMouseScroll='event.stopPropagation()'>",
    paste(links, collapse = "<br>"),
    "</div>"
  )
}

#' Add a custom zoom control to a leaflet map
#'
#' @param map A leaflet map object
#' @return The leaflet map object with a zoom control added
#'
#' @importFrom htmlwidgets onRender
#' @noRd
add_zoom_control <- function(map) {
  map |> htmlwidgets::onRender("
    function(el, x) {
      // Create map reference
      var map = this;

      // Create zoom control
      var zoomControl = L.control({position: 'bottomleft'});
      zoomControl.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'zoom-control');
        div.style.background = 'white';
        div.style.padding = '5px';
        div.style.border = '1px solid #ccc';
        div.innerHTML = 'Zoom: ' + map.getZoom();
        return div;
      };
      zoomControl.addTo(map);

      // Add event listeners with debouncing
      var updateTimeout;
      map.on('zoomend', function() {
        // Update UI immediately
        document.getElementsByClassName('zoom-control')[0].innerHTML = 'Zoom: ' + map.getZoom();

        // Debounce Shiny communication
        clearTimeout(updateTimeout);
        updateTimeout = setTimeout(function() {
          Shiny.setInputValue('map_zoom', map.getZoom(), {priority: 'event'});
        }, 300);
      });

      map.on('moveend', function() {
        var center = map.getCenter();

        // Debounce Shiny communication
        clearTimeout(updateTimeout);
        updateTimeout = setTimeout(function() {
          Shiny.setInputValue('map_center',
                          {lat: center.lat, lng: center.lng},
                          {priority: 'event'});
        }, 300);
      });
    }
  ")
}
