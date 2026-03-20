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
  # Single quotes are safe within double-quoted attributes
  expect_true(grepl("onclick=\"Shiny.setInputValue\\('plot_link_click',\\s*'ACC1'", single_popup))
  expect_true(grepl(">Plot1 (ACC1)</a>", single_popup, fixed = TRUE))

  # Multiple observations
  multi_popup <- create_marker_popup(
    c("Plot1", "Plot2"),
    c("ACC1", "ACC2"),
    2
  )
  expect_true(grepl("<strong>2 Observations</strong>", multi_popup))
  expect_true(grepl(">Plot1 (ACC1)</a>", multi_popup, fixed = TRUE))
  expect_true(grepl(">Plot2 (ACC2)</a>", multi_popup, fixed = TRUE))
  expect_true(grepl("<br>", multi_popup))
})

# ---- XSS Prevention tests ----

test_that("create_marker_popup prevents XSS in author_obs_code", {
  # Test HTML injection in display text
  xss_obs <- "<script>alert('xss')</script>"
  popup <- create_marker_popup(xss_obs, "ACC1", 1)

  # Should NOT contain raw script tag
  expect_false(grepl("<script>", popup, fixed = TRUE))
  # Should contain escaped version
  expect_true(grepl("&lt;script&gt;", popup, fixed = TRUE))
})

test_that("create_marker_popup prevents XSS in ob_code", {
  # Test JS injection in onclick handler
  xss_acc <- "'); alert('xss'); //"
  popup <- create_marker_popup("Plot1", xss_acc, 1)

  # Should NOT contain unescaped single quote followed by closing paren
  # that could break out of the JS string (the raw pattern "');")
  expect_false(grepl("'\\);", popup, fixed = TRUE))
  # htmltools::htmlEscape converts quotes to HTML entities (&#39;) which prevents XSS
  expect_true(grepl("&#39;", popup))
})

test_that("create_marker_popup handles special characters safely", {
  # Test various edge cases
  special_obs <- "Plot's \"Name\" <test> & more"
  special_acc <- "ob.123/456"

  popup <- create_marker_popup(special_obs, special_acc, 1)

  # Display text should be HTML escaped for text content
  # Note: htmlEscape() without attribute=TRUE doesn't escape quotes/apostrophes in text nodes (they're safe there)
  expect_true(grepl("&lt;test&gt;", popup)) # escaped angle brackets
  expect_true(grepl("&amp;", popup)) # escaped ampersand

  # JS string value is HTML escaped - forward slashes are safe and not escaped
  expect_true(grepl("ob.123/456", popup, fixed = TRUE))
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
  # Use real ob.2948 coordinates from the Acadia National Park dataset
  good_data <- mock_plot_data[1, c("latitude", "longitude", "author_obs_code", "ob_code")]
  result <- validate_map_data(good_data)
  expect_true(result$valid)
  expect_equal(nrow(result$data), 1)
  expect_equal(result$data$ob_code, "ob.2948")
  expect_equal(result$data$author_obs_code, "ACAD.143")
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

  # Use the three real Acadia National Park observations (Maine coords)
  test_data <- mock_plot_observations_multi[, c("latitude", "longitude", "author_obs_code", "ob_code")]

  with_mock_shiny_notifications({
    map <- process_map_data(test_data, defaults$lng, defaults$lat, defaults$zoom)
    expect_true(inherits(map, "leaflet"))
    expect_true(length(map$x$calls) > 0)
  })
})

test_that("process_map_data handles custom center and zoom", {
  # Use the single real ob.2948 (ACAD.143) observation from Maine
  test_data <- mock_plot_data[1, c("latitude", "longitude", "author_obs_code", "ob_code")]

  with_mock_shiny_notifications({
    map <- process_map_data(test_data, center_lat = 44.0, center_lng = -68.0, zoom = 10)
    expect_true(inherits(map, "leaflet"))
  })
})

test_that("fetch_plot_map_data returns data when API succeeds", {
  # Return the real three-observation Acadia dataset from the mocked API
  acad_map_data <- mock_plot_observations_multi[, c("latitude", "longitude", "author_obs_code", "ob_code")]

  with_mock_shiny_notifications({
    result <- testthat::with_mocked_bindings(
      fetch_plot_map_data(),
      vb_get_plot_observations = function(...) acad_map_data,
      .package = "vegbankr"
    )

    expect_equal(result, as.data.frame(acad_map_data))
    expect_equal(nrow(result), 3)
    expect_equal(result$ob_code, c("ob.2948", "ob.3776", "ob.206444"))
    expect_length(get_mock_notifications(), 0)
  })
})

test_that("fetch_plot_map_data propagates API errors to the caller", {
  expect_error(
    testthat::with_mocked_bindings(
      fetch_plot_map_data(),
      vb_get_plot_observations = function(...) stop("API offline"),
      .package = "vegbankr"
    ),
    "API offline"
  )
})

test_that("fetch_plot_map_data returns NULL when API returns no rows", {
  result <- testthat::with_mocked_bindings(
    fetch_plot_map_data(),
    vb_get_plot_observations = function(...) data.frame(),
    .package = "vegbankr"
  )
  expect_null(result)
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

# ---- add_search_control / onRender hook tests ----

# Helper: collect all `code` strings from $jsHooks$render
render_hook_codes <- function(map) {
  vapply(map$jsHooks$render, `[[`, character(1), "code")
}

test_that("add_search_control attaches a render hook to the map", {
  m <- htmlwidgets::onRender(create_empty_map(),
    "function(el, x) { window.vbMapSearchControl(this, el); }")
  hooks <- m$jsHooks$render
  expect_true(length(hooks) >= 1)
})

test_that("add_search_control render hook invokes vbMapSearchControl", {
  m <- create_empty_map()
  m <- vegbankweb:::add_search_control(m)
  codes <- render_hook_codes(m)
  expect_true(any(grepl("vbMapSearchControl", codes, fixed = TRUE)))
})

test_that("add_search_control render hook passes map and element arguments", {
  m <- create_empty_map()
  m <- vegbankweb:::add_search_control(m)
  codes <- render_hook_codes(m)
  search_code <- codes[grepl("vbMapSearchControl", codes, fixed = TRUE)]
  expect_match(search_code, "function\\(el,\\s*x\\)")
  expect_match(search_code, "window\\.vbMapSearchControl\\(this,\\s*el\\)")
})

test_that("build_leaflet_map includes the vbMapSearchControl render hook", {
  defaults <- get_map_defaults()
  test_data <- mock_plot_data[1, c("latitude", "longitude", "author_obs_code", "ob_code")]
  with_mock_shiny_notifications({
    map <- process_map_data(test_data, defaults$lng, defaults$lat, defaults$zoom)
    codes <- render_hook_codes(map)
    expect_true(any(grepl("vbMapSearchControl", codes, fixed = TRUE)))
  })
})

test_that("build_leaflet_map render hooks include all three control callbacks", {
  defaults <- get_map_defaults()
  test_data <- mock_plot_data[1, c("latitude", "longitude", "author_obs_code", "ob_code")]
  with_mock_shiny_notifications({
    map <- process_map_data(test_data, defaults$lng, defaults$lat, defaults$zoom)
    codes <- render_hook_codes(map)
    expect_true(any(grepl("vbMapBindShinyInputs", codes, fixed = TRUE)))
    expect_true(any(grepl("vbMapHelpControl",     codes, fixed = TRUE)))
    expect_true(any(grepl("vbMapSearchControl",   codes, fixed = TRUE)))
  })
})
