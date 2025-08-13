#' Map Functions Module
#'
#' Provides functions for creating and manipulating leaflet maps.

# ---- App-facing functions ----

#' Process map data and return a leaflet map object
#' @param map_data Data frame with map points
#' @param center_lng Longitude for map center
#' @param center_lat Latitude for map center
#' @param zoom Initial zoom level
#' @returns A leaflet map object containing clustered markers for each plot location with a list of
#'          observations in their labels
#'
#' @importFrom leaflet leaflet leafletOptions setMaxBounds addTiles addMarkers markerClusterOptions
#' @importFrom dplyr arrange group_by summarize n
#' @importFrom htmltools HTML
#' @importFrom shiny withProgress incProgress showNotification
#' @importFrom htmlwidgets onRender
#' @noRd
process_map_data <- function(map_data, center_lng = -98.5795, center_lat = 39.8283, zoom = 2) {
  shiny::withProgress(
    expr = {
      if (is.null(map_data) || nrow(map_data) == 0) {
        shiny::showNotification(
          "Missing required data. Please try again or check your connection.",
          type = "error"
        )
        return(create_empty_map())
      }
      shiny::incProgress(0.2, detail = "Filtering valid points...")
      valid_points <- subset(map_data, !is.na(map_data$latitude) & !is.na(map_data$longitude))
      if (nrow(valid_points) == 0) {
        shiny::incProgress(0.8, detail = "No valid points found")
        return(create_empty_map() |>
          leaflet::setView(lng = center_lng, lat = center_lat, zoom = zoom))
      }
      shiny::incProgress(0.3, detail = "Grouping plots by location...")
      data_grouped <- valid_points |>
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
      shiny::incProgress(0.3, detail = "Building map...")
      leaflet::leaflet(data_grouped, options = leaflet::leafletOptions(minZoom = 2)) |>
        leaflet::setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85) |>
        leaflet::addTiles() |>
        leaflet::setView(lng = center_lng, lat = center_lat, zoom = zoom) |>
        leaflet::addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~ paste(latitude, longitude, sep = ", "),
          label = ~ author_obs_code_label |> lapply(htmltools::HTML),
          labelOptions = leaflet::labelOptions(
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
          ),
          clusterOptions = leaflet::markerClusterOptions(disableClusteringAtZoom = 17)
        ) |>
        add_zoom_control()
    },
    message = "Processing map data",
    value = 0
  )
}

#' Move the map and show a popup at a given location
#' @param map_proxy A leaflet proxy object
#' @param lng Longitude
#' @param lat Latitude
#' @param label Popup label (HTML)
#' @param zoom Zoom level (default 18)
#' @returns A leaflet map proxy with updated view and popup
#'
#' @importFrom leaflet setView clearPopups addPopups
#' @noRd
update_map_view <- function(map_proxy, lng, lat, label, zoom = 18) {
  map_proxy |>
    leaflet::setView(lng = lng, lat = lat, zoom = zoom) |>
    leaflet::clearPopups() |>
    leaflet::addPopups(
      lng = lng,
      lat = lat,
      popup = label
    )
}

# ---- Helpers below (internal module use only) ----

#' Create an empty leaflet map with a 'Data unavailable' message
#'
#' @returns An empty leaflet map with a message indicating that data is unavailable.
#'
#' @importFrom leaflet leaflet leafletOptions setMaxBounds addTiles addControl
#' @noRd
create_empty_map <- function() {
  leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) |>
    leaflet::setMaxBounds(
      lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85
    ) |>
    leaflet::addTiles() |>
    leaflet::addControl("Data unavailable", position = "topright")
}

#' Create a marker popup HTML for a group of observations
#' @param obs_codes Character vector of author observation codes
#' @param accession_codes Character vector of plot observation accession codes
#' @param count Integer, number of observations for that plot
#' @returns A string containing HTML for the popup label
#' @noRd
create_marker_popup <- function(obs_codes, accession_codes, count) {
  paste0(
    "<strong>",
    ifelse(count == 1, "1 Observation", paste(count, "Observations")),
    "</strong>",
    "<div style='max-height: 15.5rem; overflow-y: auto;' onwheel='event.stopPropagation()'",
    " onmousewheel='event.stopPropagation()' onDOMMouseScroll='event.stopPropagation()'>",
    paste(
      mapply(
        function(obs, acc) {
          sprintf(
            "<a href=\"#\" onclick=\"Shiny.setInputValue('label_link_click',\n '%s', {priority: 'event'})\">%s</a>",
            acc, obs
          )
        },
        obs_codes, accession_codes
      ),
      collapse = "<br>"
    ),
    "</div>"
  )
}

#' Add a custom zoom control to a leaflet map
#' @param map A leaflet map object
#' @returns The leaflet map object with a zoom control added
#'
#' @importFrom htmlwidgets onRender
#' @noRd
add_zoom_control <- function(map) {
  map |> htmlwidgets::onRender("
    function(el, x) {
      var map = this;
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
      var updateTimeout;
      map.on('zoomend', function() {
        document.getElementsByClassName('zoom-control')[0].innerHTML = 'Zoom: ' + map.getZoom();
        clearTimeout(updateTimeout);
        updateTimeout = setTimeout(function() {
          Shiny.setInputValue('map_zoom', map.getZoom(), {priority: 'event'});
        }, 300);
      });
      map.on('moveend', function() {
        var center = map.getCenter();
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
