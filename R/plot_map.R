#' Map Functions Module
#'
#' Provides functions for creating and manipulating leaflet maps.
#'
#' @importFrom leaflet leafletOptions setMaxBounds addTiles addControl addMarkers
#'             setView clearPopups addPopups labelOptions markerClusterOptions
#' @importFrom htmlwidgets onRender
#' @importFrom dplyr arrange group_by summarize n
#' @importFrom htmltools HTML
#' @importFrom ggplot2 .data
#'
#' @noRd

# ---- App-facing functions ----

#' Process map data and return a leaflet map object
#' @param map_data Data frame with map points
#' @param center_lng Longitude for map center
#' @param center_lat Latitude for map center
#' @param zoom Initial zoom level
#' @noRd
process_map_data <- function(map_data, center_lng = -98.5795, center_lat = 39.8283, zoom = 2) {
  progress_handler$with_safe_progress(
    expr = {
      if (is.null(map_data) || nrow(map_data) == 0) {
        progress_handler$show_notification(
          "Missing required data. Please try again or check your connection.",
          type = "error"
        )
        return(create_empty_map())
      }
      progress_handler$inc_progress(0.2, detail = "Filtering valid points...")
      valid_points <- subset(map_data, !is.na(map_data$latitude) & !is.na(map_data$longitude))
      if (nrow(valid_points) == 0) {
        progress_handler$inc_progress(0.8, detail = "No valid points found")
        return(create_empty_map() |>
                 leaflet::setView(lng = center_lng, lat = center_lat, zoom = zoom))
      }
      progress_handler$inc_progress(0.3, detail = "Grouping plots by location...")
      data_grouped <- valid_points |>
        dplyr::arrange(.data$authorobscode) |>
        dplyr::group_by(.data$latitude, .data$longitude) |>
        dplyr::summarize(
          obs_count = dplyr::n(),
          authorobscode_label = create_marker_popup(
            .data$authorobscode,
            .data$obsaccessioncode,
            dplyr::n()
          ),
          .groups = "drop"
        )
      progress_handler$inc_progress(0.3, detail = "Building map...")
      leaflet::leaflet(data_grouped, options = leaflet::leafletOptions(minZoom = 2)) |>
        leaflet::setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85) |>
        leaflet::addTiles() |>
        leaflet::setView(lng = center_lng, lat = center_lat, zoom = zoom) |>
        leaflet::addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~ paste(latitude, longitude, sep = ", "),
          label = ~ authorobscode_label |> lapply(htmltools::HTML),
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
#' @param accession_codes Character vector of accession codes
#' @param count Integer, number of observations
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
