#' Map Functions Module
#'
#' Provides functions for creating and manipulating leaflet maps.

# ---- Constants ----
#' @noRd
MAP_DATA_FETCH_LIMIT <- 1000000L

# ---- Data Fetching ----

#' Fetch plot observations for the map
#'
#' Downloads plot observation data for the leaflet map using the VegBank API.
#' Handles its own progress indicator and error notifications.
#'
#' @param limit Maximum number of records to request (defaults to 1,000,000)
#' @param detail VegBank detail level to request (defaults to "geo")
#' @return Data frame of plot observations or NULL on failure
#' @noRd
fetch_plot_map_data <- function(limit = MAP_DATA_FETCH_LIMIT, detail = "geo") {
  shiny::withProgress(message = "Loading map data...", value = 0.2, {
    error_occurred <- FALSE

    data <- tryCatch(
      vegbankr::vb_get_plot_observations(
        limit = limit,
        detail = detail,
        with_nested = FALSE
      ),
      error = function(err) {
        error_occurred <<- TRUE
        shiny::showNotification(
          paste("Failed to load map data:", conditionMessage(err)),
          type = "error"
        )
        NULL
      }
    )

    shiny::incProgress(1)

    if (isTRUE(error_occurred)) {
      return(NULL)
    }

    if (is.null(data) || nrow(data) == 0) {
      shiny::showNotification(
        "Map data is currently unavailable. Please try again later.",
        type = "warning"
      )
      return(NULL)
    }

    as.data.frame(data)
  })
}

# ---- Data Validation ----

#' Validate map data for rendering
#'
#' Checks whether map data has the required structure and valid points.
#' This is a pure function with no side effects, making it easy to test.
#'
#' @param map_data Data frame to validate
#' @return List with `valid` (logical), `reason` (character if invalid),
#'         and `data` (filtered data frame if valid)
#' @noRd
validate_map_data <- function(map_data) {
  # Check for required structure
  if (is_invalid_map_data(map_data)) {
    return(list(valid = FALSE, reason = "missing_required"))
  }

  # Filter to valid coordinates
  valid_points <- filter_valid_map_points(map_data)

  if (nrow(valid_points) == 0) {
    return(list(valid = FALSE, reason = "no_valid_points"))
  }

  list(valid = TRUE, data = valid_points)
}

#' Check if map data is invalid (missing or malformed)
#'
#' @param map_data Data frame to check
#' @return TRUE if data is invalid, FALSE otherwise
#' @noRd
is_invalid_map_data <- function(map_data) {
  is.null(map_data) ||
    nrow(map_data) == 0 ||
    !all(c("latitude", "longitude", "author_obs_code", "ob_code") %in% colnames(map_data))
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

# ---- Error Notifications ----

#' Show appropriate error notification for map validation failure
#'
#' @param reason Character string indicating the validation failure reason
#' @noRd
show_map_validation_error <- function(reason) {
  message <- switch(reason,
    "missing_required" = "Missing required data. Please try again or check your connection.",
    "no_valid_points" = "No valid map points found in the data.",
    "Unknown map data error."
  )
  shiny::showNotification(message, type = "error")
}

# ---- Map Building ----

#' Process map data and return a leaflet map object
#'
#' Orchestrates validation and map building. Shows appropriate notifications
#' on validation failure and returns an empty map as fallback.
#'
#' @param map_data Data frame with map points. Must contain latitude, longitude,
#'                author_obs_code and ob_code columns.
#' @param center_lng Longitude for map center (numeric)
#' @param center_lat Latitude for map center (numeric)
#' @param zoom Initial zoom level (integer)
#' @param map_options Optional list of map configuration options to override defaults
#' @returns A leaflet map object containing clustered markers for each plot location
#'
#' @importFrom leaflet leaflet leafletOptions setMaxBounds addTiles addMarkers markerClusterOptions
#' @importFrom dplyr arrange group_by summarize n
#' @importFrom htmltools HTML
#' @importFrom shiny withProgress incProgress showNotification
#' @importFrom htmlwidgets onRender
#' @noRd
process_map_data <- function(map_data, center_lng, center_lat, zoom, map_options = NULL) {
  map_defaults <- get_map_defaults()
  if (!is.null(map_options)) {
    map_defaults <- utils::modifyList(map_defaults, map_options)
  }

  # Step 1: Validate
  validation <- validate_map_data(map_data)

  if (!validation$valid) {
    show_map_validation_error(validation$reason)
    return(create_empty_map(map_defaults, center_lng, center_lat, zoom))
  }

  # Step 2: Build map with progress indicator
  shiny::withProgress(message = "Building map...", value = 0, {
    shiny::incProgress(0.3, detail = "Grouping plots by location...")
    data_grouped <- group_map_points(validation$data)

    shiny::incProgress(0.5, detail = "Rendering map...")
    result <- build_leaflet_map(data_grouped, map_defaults, center_lng, center_lat, zoom)

    shiny::incProgress(0.2)
    result
  })
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
    leaflet::addPopups(lng = lng, lat = lat, popup = label)
}

# ---- Data Grouping ----

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
        .data$ob_code,
        dplyr::n()
      ),
      .groups = "drop"
    )
}

# ---- Leaflet Construction ----

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

  map
}

# ---- Marker Content ----

#' Create a marker popup HTML for a group of observations
#'
#' @param author_obs_codes Character vector of author observation codes
#' @param ob_codes Character vector of plot observation VegBank codes
#' @param count Integer, number of observations for that plot
#' @return A string containing HTML for the popup label
#' @noRd
create_marker_popup <- function(author_obs_codes, ob_codes, count) {
  header <- ifelse(count == 1, "1 Observation", paste(count, "Observations"))

  links <- mapply(
    function(author_obs_code, ob_code) {
      # Not using create_detail_link here bc direct string manipulation is slightly faster
      safe_ob <- htmltools::htmlEscape(ob_code)
      safe_author <- htmltools::htmlEscape(author_obs_code)
      sprintf(
        "<a href=\"#\" onclick=\"Shiny.setInputValue('plot_link_click', '%s', {priority: 'event'}); return false;\">%s</a>",
        safe_ob, safe_author
      )
    },
    author_obs_codes, ob_codes
  )

  paste0(
    "<strong>", header, "</strong>",
    "<div style='max-height: 15.5rem; overflow-y: auto;' onwheel='event.stopPropagation()'",
    " onmousewheel='event.stopPropagation()' onDOMMouseScroll='event.stopPropagation()'>",
    paste(links, collapse = "<br>"),
    "</div>"
  )
}

# ---- Zoom Control ----

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
