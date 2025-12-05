test_that("create_empty_map returns a leaflet map", {
  empty_map <- create_empty_map()
  expect_true(inherits(empty_map, "leaflet"))
  expect_true(inherits(empty_map$x, "list"))
  expect_equal(empty_map$x$options$minZoom, 2)
})

test_that("get_map_defaults returns expected structure", {
  defaults <- get_map_defaults()
  expect_type(defaults, "list")
  expect_true(all(c("lat", "lng", "zoom", "detail_zoom", "min_zoom", "max_bounds") %in% names(defaults)))
  expect_type(defaults$max_bounds, "list")
})

test_that("create_marker_popup creates correct HTML", {
  # Single observation
  single_popup <- create_marker_popup("Plot1", "ACC1", 1)
  expect_true(grepl("<strong>1 Observation</strong>", single_popup))
  expect_true(grepl("onclick=\"Shiny.setInputValue\\('plot_link_click',\\s*'ACC1'", single_popup))
  expect_true(grepl(">Plot1</a>", single_popup))

  # Multiple observations
  multi_popup <- create_marker_popup(
    c("Plot1", "Plot2"),
    c("ACC1", "ACC2"),
    2
  )
  expect_true(grepl("<strong>2 Observations</strong>", multi_popup))
  expect_true(grepl(">Plot1</a>", multi_popup))
  expect_true(grepl(">Plot2</a>", multi_popup))
  expect_true(grepl("<br>", multi_popup))
})

# ---- validate_map_data tests ----

test_that("validate_map_data returns invalid for NULL data", {
  result <- validate_map_data(NULL)
  expect_false(result$valid)
  expect_equal(result$reason, "missing_required")
})

test_that("validate_map_data returns invalid for empty data frame", {
  result <- validate_map_data(data.frame())
  expect_false(result$valid)
  expect_equal(result$reason, "missing_required")
})

test_that("validate_map_data returns invalid for missing columns", {
  incomplete_data <- data.frame(latitude = 40, longitude = -74)
  result <- validate_map_data(incomplete_data)
  expect_false(result$valid)
  expect_equal(result$reason, "missing_required")
})

test_that("validate_map_data returns invalid when all coordinates are NA", {
  bad_coords <- data.frame(
    latitude = NA,
    longitude = NA,
    author_obs_code = "CODE",
    ob_code = "ob.1"
  )
  result <- validate_map_data(bad_coords)
  expect_false(result$valid)
  expect_equal(result$reason, "no_valid_points")
})

test_that("validate_map_data returns valid with good data", {
  good_data <- data.frame(
    latitude = 40.7128,
    longitude = -74.0060,
    author_obs_code = "NYC",
    ob_code = "ob.2948"
  )
  result <- validate_map_data(good_data)
  expect_true(result$valid)
  expect_equal(nrow(result$data), 1)
})

test_that("validate_map_data filters out NA coordinates", {
  mixed_data <- data.frame(
    latitude = c(40.7128, NA, 34.0522),
    longitude = c(-74.0060, -100, NA),
    author_obs_code = c("NYC", "BAD1", "BAD2"),
    ob_code = c("ob.1", "ob.2", "ob.3")
  )
  result <- validate_map_data(mixed_data)
  expect_true(result$valid)
  expect_equal(nrow(result$data), 1)
  expect_equal(result$data$author_obs_code, "NYC")
})

# ---- process_map_data tests ----

test_that("process_map_data handles empty input", {
  with_mock_shiny_notifications({
    defaults <- get_map_defaults()

    # Test with NULL data - use default values for coordinates and zoom
    empty_map <- process_map_data(NULL, defaults$lng, defaults$lat, defaults$zoom)
    expect_true(inherits(empty_map, "leaflet"))

    # Test with empty data frame
    empty_map <- process_map_data(data.frame(), defaults$lng, defaults$lat, defaults$zoom)
    expect_true(inherits(empty_map, "leaflet"))
  })
})

test_that("process_map_data creates a map with markers", {
  defaults <- get_map_defaults()

  # Create a small test dataset
  test_data <- data.frame(
    latitude = c(40.7128, 34.0522),
    longitude = c(-74.0060, -118.2437),
    author_obs_code = c("NYC", "LA"),
    ob_code = c("ob.2948", "ob.2949"),
    stringsAsFactors = FALSE
  )

  with_mock_shiny_notifications({
    map <- process_map_data(test_data, defaults$lng, defaults$lat, defaults$zoom)
    # Just check that the map is created successfully
    expect_true(inherits(map, "leaflet"))
    # Check that there's at least one call in the map object
    expect_true(length(map$x$calls) > 0)
  })
})

test_that("process_map_data handles custom center and zoom", {
  test_data <- data.frame(
    latitude = c(40.7128),
    longitude = c(-74.0060),
    author_obs_code = c("NYC"),
    ob_code = c("ob.2948"),
    stringsAsFactors = FALSE
  )

  with_mock_shiny_notifications({
    map <- process_map_data(test_data, center_lat = 35.0, center_lng = -100.0, zoom = 4)
    # Just check that the map is created successfully
    expect_true(inherits(map, "leaflet"))
  })
})

test_that("fetch_plot_map_data returns data when API succeeds", {
  fake_data <- data.frame(
    latitude = 10,
    longitude = 20,
    author_obs_code = "CODE",
    ob_code = "ob.1"
  )

  with_mock_shiny_notifications({
    result <- testthat::with_mocked_bindings(
      fetch_plot_map_data(),
      get_all_plot_observations = function(...) fake_data,
      .package = "vegbankr"
    )

    expect_equal(result, fake_data)
    expect_length(get_mock_notifications(), 0)
  })
})

test_that("fetch_plot_map_data surfaces API errors", {
  with_mock_shiny_notifications({
    result <- testthat::with_mocked_bindings(
      fetch_plot_map_data(),
      get_all_plot_observations = function(...) stop("API offline"),
      .package = "vegbankr"
    )

    expect_null(result)
    last_notification <- get_last_notification()
    expect_match(last_notification$message, "Failed to load map data")
    expect_equal(last_notification$type, "error")
  })
})

test_that("fetch_plot_map_data warns when API returns no rows", {
  with_mock_shiny_notifications({
    result <- testthat::with_mocked_bindings(
      fetch_plot_map_data(),
      get_all_plot_observations = function(...) data.frame(),
      .package = "vegbankr"
    )

    expect_null(result)
    last_notification <- get_last_notification()
    expect_match(last_notification$message, "Map data is currently unavailable")
    expect_equal(last_notification$type, "warning")
  })
})

test_that("add_zoom_control adds onRender function to map", {
  # Create a minimal leaflet map
  map <- leaflet::leaflet()

  # Add zoom control
  map_with_control <- add_zoom_control(map)

  # Check that onRender is added
  expect_true(!is.null(map_with_control$jsHooks$render))

  # Check that the hook contains expected elements
  expect_true(grepl("zoomControl", map_with_control$jsHooks$render[[1]]$code))
  expect_true(grepl("Shiny.setInputValue", map_with_control$jsHooks$render[[1]]$code))
})

test_that("update_map_view creates proper function", {
  # Since update_map_view is a function factory, we can test the function exists
  expect_true(is.function(update_map_view))

  # We can't easily test the proxy function directly without a Shiny session,
  # but we can check the function signature
  fn_args <- formals(update_map_view)
  expect_equal(names(fn_args), c("map_proxy", "lng", "lat", "label", "zoom"))
  expect_null(fn_args$zoom)
})
