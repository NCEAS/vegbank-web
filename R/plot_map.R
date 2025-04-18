#' Map Functions Module
#'
#' Provides functions for creating and manipulating leaflet maps.
#' @keywords internal

#' @importFrom leaflet leafletOptions setMaxBounds addTiles addControl addMarkers
#'             setView clearPopups addPopups labelOptions markerClusterOptions
#' @importFrom htmlwidgets onRender
#' @importFrom dplyr arrange group_by summarize n
#' @importFrom htmltools HTML
#' @importFrom ggplot2 .data
plot_map <- (function() {
  create_empty_map <- function() {
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) |>
      leaflet::setMaxBounds(
        lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85
      ) |>
      leaflet::addTiles() |>
      leaflet::addControl("Data unavailable", position = "topright")
  }

  create_marker_popup <- function(obs_codes, accession_codes, count) {
    paste0(
      "<strong>",
      ifelse(count == 1, "1 Observation", paste(count, "Observations")),
      "</strong>",
      "<div style='max-height: 15.5rem; overflow-y: auto;'
        onwheel='event.stopPropagation()'
        onmousewheel='event.stopPropagation()'
        onDOMMouseScroll='event.stopPropagation()'>",
      paste(
        mapply(
          function(obs, acc) {
            sprintf(
              "<a href=\"#\" onclick=\"Shiny.setInputValue('label_link_click', 
              '%s', {priority: 'event'})\">%s</a>",
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
        map.on('zoomend', function() {
          document.getElementsByClassName('zoom-control')[0].innerHTML = 'Zoom: ' + map.getZoom();
          Shiny.setInputValue('map_zoom', map.getZoom());
        });
      }
    ")
  }

  process_map_data <- function(map_data) {
    if (is.null(map_data) || nrow(map_data) == 0) {
      return(create_empty_map())
    }

    # Filter valid points and group by location
    valid_points <- subset(map_data, !is.na(map_data$latitude) & !is.na(map_data$longitude))
    message("Total valid pins: ", nrow(valid_points))

    data_grouped <- valid_points |>
      dplyr::arrange(.data$authorobscode) |>
      dplyr::group_by(.data$latitude, .data$longitude) |>
      dplyr::summarize(
        obs_count = dplyr::n(),
        authorobscode_label = create_marker_popup(
          .data$authorobscode,
          .data$accessioncode,
          dplyr::n()
        ),
        .groups = "drop"
      )

    message("Total grouped labels: ", nrow(data_grouped))

    # Create and return the map
    leaflet::leaflet(data_grouped, options = leaflet::leafletOptions(minZoom = 2)) |>
      leaflet::setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85) |>
      leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 2) |>
      leaflet::addTiles() |>
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
        clusterOptions = leaflet::markerClusterOptions()
      ) |>
      add_zoom_control()
  }

  list(
    process_map_data = process_map_data,
    create_empty_map = create_empty_map,
    update_map_view = function(map_proxy, lng, lat, label, zoom = 18) {
      map_proxy |>
        leaflet::setView(lng = lng, lat = lat, zoom = zoom) |>
        leaflet::clearPopups() |>
        leaflet::addPopups(
          lng = lng,
          lat = lat,
          popup = label
        )
    }
  )
})()
