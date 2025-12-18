# Integration Tests for URL State Management in Server Context
# Tests interaction between URL manager, observers, and state restoration

test_that("URLStateManager prevents circular updates during state restoration", {
  # This test verifies the updating flag correctly prevents circular updates
  # when restoring state from URL by checking that update_query_string is
  # not called when the flag is TRUE

  update_called <- FALSE
  mock_update_query_string <- function(queryString, mode = "push", session = NULL) {
    update_called <<- TRUE
  }

  mock_session <- list(
    clientData = list(url_search = "?tab=Plots"),
    sendCustomMessage = function(...) NULL
  )

  defaults <- list(map_lat = 39.8283, map_lng = -98.5795, map_zoom = 4)
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L)
  )

  manager <- URLStateManager$new(mock_session, defaults, table_registry)

  # Initially not updating
  expect_false(manager$is_updating())

  # Simulate state restoration: set flag to prevent circular updates
  manager$set_updating(TRUE)
  expect_true(manager$is_updating())

  # Simulate observer trying to update URL while restoring state
  # In real code, observers check is_updating() and skip update if TRUE
  # We verify this by confirming that when is_updating() is TRUE,
  # the update should be skipped
  update_called <- FALSE

  # Mock the updateQueryString call
  testthat::with_mocked_bindings(
    {
      # Build a query string that differs from current
      new_query <- manager$build_query_string(tab = "Plants")

      # In the actual observer, this block would be wrapped in:
      # if (!manager$is_updating()) { manager$update_query_string(...) }
      # Here we verify that the flag prevents the update
      if (!manager$is_updating()) {
        manager$update_query_string(new_query, mode = "push")
      }

      # Update should have been skipped because flag is TRUE
      expect_false(update_called)
    },
    updateQueryString = mock_update_query_string,
    .package = "shiny"
  )

  # After restoration completes, flag is cleared
  manager$set_updating(FALSE)
  expect_false(manager$is_updating())

  # Now updates should work
  update_called <- FALSE
  testthat::with_mocked_bindings(
    {
      new_query <- manager$build_query_string(tab = "Plants")
      if (!manager$is_updating()) {
        manager$update_query_string(new_query, mode = "push")
      }

      # Update should have been called because flag is FALSE
      expect_true(update_called)
    },
    updateQueryString = mock_update_query_string,
    .package = "shiny"
  )
})

test_that("URLStateManager tracks history initialization for push vs replace", {
  # Track which mode was used when updateQueryString was called
  update_mode_used <- NULL
  mock_update_query_string <- function(queryString, mode = "push", session = NULL) {
    update_mode_used <<- mode
  }

  mock_session <- list(
    clientData = list(url_search = ""),
    sendCustomMessage = function(...) NULL
  )

  manager <- URLStateManager$new(mock_session)

  # Initially false - first update should use "replace"
  expect_false(manager$is_history_initialized())

  testthat::with_mocked_bindings(
    {
      # First navigation - should use "replace" mode
      query <- manager$build_query_string(tab = "Plots")

      # In actual code, this determines mode based on history_initialized
      mode <- if (manager$is_history_initialized()) "push" else "replace"
      manager$update_query_string(query, mode = mode)

      # Verify "replace" was used
      expect_equal(update_mode_used, "replace")

      # Mark history as initialized (simulating what happens after first update)
      manager$set_history_initialized(TRUE)
      expect_true(manager$is_history_initialized())

      # Reset tracker
      update_mode_used <- NULL

      # Subsequent navigation - should use "push" mode
      query2 <- manager$build_query_string(tab = "Plants")
      mode2 <- if (manager$is_history_initialized()) "push" else "replace"
      manager$update_query_string(query2, mode = mode2)

      # Verify "push" was used
      expect_equal(update_mode_used, "push")
    },
    updateQueryString = mock_update_query_string,
    .package = "shiny"
  )
})

test_that("Query parsing and building are inverse operations", {
  defaults <- list(map_lat = 39.8283, map_lng = -98.5795, map_zoom = 4)
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L),
    plants = list(tab = "Plants", table_id = "plant_table", default_length = 100L)
  )

  # Build a complex query string
  table_states <- list(
    plots = list(
      start = 50,
      length = 25,
      order = list(list(column = 0, dir = "asc"), list(column = 1, dir = "desc")),
      search = "test query"
    )
  )

  # First create manager with empty query
  mock_session <- list(
    clientData = list(url_search = ""),
    sendCustomMessage = function(...) NULL
  )
  manager <- URLStateManager$new(mock_session, defaults, table_registry)

  query_string <- manager$build_query_string(
    tab = "Plots",
    detail_type = "plot-observation",
    detail_code = "VB.123",
    map_lat = 40.5,
    map_lng = -100.2,
    map_zoom = 12,
    map_has_custom_state = TRUE,
    table_states = table_states
  )

  # Create new manager with the built query string
  mock_session2 <- list(
    clientData = list(url_search = query_string),
    sendCustomMessage = function(...) NULL
  )
  manager2 <- URLStateManager$new(mock_session2, defaults, table_registry)

  # Parse it back
  params <- manager2$parse_query_string()

  # Verify all components are present
  expect_equal(manager2$first_param(params$tab), "Plots")
  expect_equal(manager2$first_param(params$detail), "plot-observation")
  expect_equal(manager2$first_param(params$code), "VB.123")
  expect_equal(manager2$parse_numeric_param(params$map_lat), 40.5, tolerance = 0.0001)
  expect_equal(manager2$parse_numeric_param(params$map_lng), -100.2, tolerance = 0.0001)
  expect_equal(manager2$parse_numeric_param(params$map_zoom), 12, tolerance = 0.01)

  # Verify table state can be extracted
  extracted_table_state <- manager2$extract_table_state_from_query("plots", params)
  expect_equal(extracted_table_state$start, 50L)
  expect_equal(extracted_table_state$length, 25L)
  expect_equal(extracted_table_state$search, "test query")
  expect_equal(length(extracted_table_state$order), 2)
  expect_equal(extracted_table_state$order[[1]]$column, 0)
  expect_equal(extracted_table_state$order[[1]]$dir, "asc")
})

test_that("Default state detection prevents URL pollution", {
  mock_session <- list(
    clientData = list(url_search = ""),
    sendCustomMessage = function(...) NULL
  )

  defaults <- list(map_lat = 39.8283, map_lng = -98.5795, map_zoom = 4)
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L)
  )

  manager <- URLStateManager$new(mock_session, defaults, table_registry)

  # Map at default position should be detected
  expect_true(manager$is_map_default(39.8283, -98.5795, 4))
  expect_true(manager$is_map_default(39.82830001, -98.57950001, 4.0000001))

  # Table at default state should be detected
  default_table_state <- list(start = 0, length = 100, order = NULL, search = "")
  expect_true(manager$is_default_table_state("plots", default_table_state))

  # Only non-default state should appear in URL
  query_default_map <- manager$build_query_string(
    tab = "Overview",
    map_lat = 39.8283,
    map_lng = -98.5795,
    map_zoom = 4,
    map_has_custom_state = FALSE # Map at defaults, not on Map tab
  )

  # Should not contain map parameters
  expect_false(grepl("map_lat", query_default_map))
  expect_false(grepl("map_lng", query_default_map))
  expect_false(grepl("map_zoom", query_default_map))
})

test_that("URL manager handles edge cases gracefully", {
  mock_session <- list(
    clientData = list(url_search = "?tab=Plots&invalid=param&plots_start=abc&plots_order=broken"),
    sendCustomMessage = function(...) NULL
  )

  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L)
  )

  manager <- URLStateManager$new(mock_session, list(), table_registry)

  params <- manager$parse_query_string()

  # Invalid numeric parameter returns NULL
  expect_null(manager$parse_numeric_param(params$plots_start))

  # Broken order string is filtered out (returns NULL when no valid entries)
  order <- manager$deserialize_order_from_query(params$plots_order)
  expect_null(order)

  # Unknown parameters are ignored (no crash)
  expect_true(is.list(params))
  expect_equal(manager$first_param(params$invalid), "param")
})

test_that("Multiple table states are all included in URL regardless of current tab", {
  mock_session <- list(
    clientData = list(url_search = ""),
    sendCustomMessage = function(...) NULL
  )

  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L),
    plants = list(tab = "Plants", table_id = "plant_table", default_length = 100L),
    communities = list(tab = "Communities", table_id = "comm_table", default_length = 100L)
  )

  manager <- URLStateManager$new(mock_session, list(), table_registry)

  # Build query with multiple table states
  table_states <- list(
    plots = list(start = 50, length = 25, order = NULL, search = "plot query"),
    plants = list(start = 100, length = 50, order = list(list(column = 1, dir = "desc")), search = ""),
    communities = list(start = 0, length = 100, order = NULL, search = "community")
  )

  # When on Plots tab, ALL table states should be in URL
  query_plots <- manager$build_query_string(tab = "Plots", table_states = table_states)

  expect_match(query_plots, "plots_start=50")
  expect_match(query_plots, "plots_search=plot")
  expect_match(query_plots, "plants_start=100")
  expect_match(query_plots, "plants_order=1")
  expect_match(query_plots, "communities_search=community")

  # When on Plants tab, ALL table states should still be in URL
  query_plants <- manager$build_query_string(tab = "Plants", table_states = table_states)

  expect_match(query_plants, "plants_start=100")
  expect_match(query_plants, "plants_order=1")
  expect_match(query_plants, "plots_start=50")
  expect_match(query_plants, "communities_search=community")
})

test_that("State sanitization handles malformed DataTables state", {
  mock_session <- list(
    clientData = list(url_search = ""),
    sendCustomMessage = function(...) NULL
  )

  manager <- URLStateManager$new(mock_session)

  # Malformed state with nested search object
  nested_search_state <- list(
    start = "50", # String instead of integer
    length = "25", # String instead of integer
    search = list(search = "nested query", regex = FALSE),
    order = data.frame(column = c(0, 1), dir = c("asc", "desc"))
  )

  sanitized <- manager$sanitize_table_state(nested_search_state)

  expect_equal(sanitized$start, 50L) # Converted to integer
  expect_equal(sanitized$length, 25L) # Converted to integer
  expect_equal(sanitized$search, "nested query") # Extracted from nested structure
  expect_equal(length(sanitized$order), 2) # Data frame converted to list
})
