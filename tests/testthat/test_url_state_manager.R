# Tests for URL State Manager
# Tests query parsing, encoding, state serialization, and circular update prevention

test_that("URLStateManager initializes correctly", {
  # Mock session
  mock_session <- list(
    clientData = list(url_search = ""),
    sendCustomMessage = function(...) NULL
  )

  defaults <- list(map_lat = 39.8283, map_lng = -98.5795, map_zoom = 4)
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L)
  )

  manager <- URLStateManager$new(mock_session, defaults, table_registry)

  expect_false(manager$is_updating())
  expect_false(manager$is_history_initialized())
})

test_that("first_param extracts first element from array", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_null(manager$first_param(NULL))
  expect_null(manager$first_param(character(0)))
  expect_equal(manager$first_param(c("first", "second")), "first")
  expect_equal(manager$first_param(list("only")), "only")
})

test_that("is_valid_param handles NULL, NA, and empty strings", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_false(manager$is_valid_param(NULL))
  expect_false(manager$is_valid_param(NA))
  expect_false(manager$is_valid_param(""))
  expect_false(manager$is_valid_param(c()))
  expect_true(manager$is_valid_param("value"))
  expect_true(manager$is_valid_param(c("first", "second")))
})

test_that("parse_numeric_param safely converts values", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  # NULL/NA/empty cases
  expect_null(manager$parse_numeric_param(NULL))
  expect_null(manager$parse_numeric_param(NA))
  expect_null(manager$parse_numeric_param(""))
  expect_null(manager$parse_numeric_param(character(0)))

  # Valid numeric strings
  expect_equal(manager$parse_numeric_param("123"), 123)
  expect_equal(manager$parse_numeric_param("45.67"), 45.67)
  expect_equal(manager$parse_numeric_param("-10.5"), -10.5)

  # Invalid strings return NULL (not error)
  expect_null(manager$parse_numeric_param("not_a_number"))

  # Arrays take first element
  expect_equal(manager$parse_numeric_param(c("10", "20")), 10)
})

test_that("format_coord formats coordinates correctly", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_equal(manager$format_coord(39.8283), "39.82830")
  expect_equal(manager$format_coord(-98.579567), "-98.57957")
  expect_equal(manager$format_coord(0), "0.00000")
})

test_that("format_zoom formats zoom levels correctly", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_equal(manager$format_zoom(4), "4")
  expect_equal(manager$format_zoom(12.5), "12.5")
  expect_equal(manager$format_zoom(18.0), "18")
})

test_that("nearly_equal compares with tolerance", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_true(manager$nearly_equal(1.0, 1.0))
  expect_true(manager$nearly_equal(1.0, 1.0000001))
  expect_false(manager$nearly_equal(1.0, 1.01))
  expect_false(manager$nearly_equal(NULL, 1.0))
  expect_false(manager$nearly_equal(1.0, NULL))
})

test_that("is_map_default checks against default coordinates", {
  mock_session <- list(clientData = list(url_search = ""))
  defaults <- list(map_lat = 39.8283, map_lng = -98.5795, map_zoom = 4)
  manager <- URLStateManager$new(mock_session, defaults)

  expect_true(manager$is_map_default(39.8283, -98.5795, 4))
  expect_true(manager$is_map_default(39.8283001, -98.5795001, 4.0000001))
  expect_false(manager$is_map_default(40.0, -98.5795, 4))
  expect_false(manager$is_map_default(39.8283, -99.0, 4))
  expect_false(manager$is_map_default(39.8283, -98.5795, 5))
})

test_that("get_table_key retrieves table key from tab name", {
  mock_session <- list(clientData = list(url_search = ""))
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L),
    plants = list(tab = "Plants", table_id = "plant_table", default_length = 100L)
  )
  manager <- URLStateManager$new(mock_session, list(), table_registry)

  expect_equal(manager$get_table_key("Plots"), "plots")
  expect_equal(manager$get_table_key("Plants"), "plants")
  expect_null(manager$get_table_key("Invalid"))
  expect_null(manager$get_table_key(NULL))
  expect_null(manager$get_table_key(""))
})

test_that("parse_order_entries handles various formats", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  # NULL case
  expect_null(manager$parse_order_entries(NULL))

  # List format
  order_list <- list(list(0, "asc"), list(1, "desc"))
  result <- manager$parse_order_entries(order_list)
  expect_equal(length(result), 2)
  expect_equal(result[[1]]$column, 0)
  expect_equal(result[[1]]$dir, "asc")
  expect_equal(result[[2]]$column, 1)
  expect_equal(result[[2]]$dir, "desc")

  # Data frame format
  order_df <- data.frame(column = c(0, 1), dir = c("asc", "desc"))
  result_df <- manager$parse_order_entries(order_df)
  expect_equal(length(result_df), 2)

  # Invalid entries filtered out
  invalid_list <- list(list(0, "asc"), NULL, list(NA, "desc"))
  result_invalid <- manager$parse_order_entries(invalid_list)
  expect_equal(length(result_invalid), 1)
  expect_equal(result_invalid[[1]]$column, 0)
})

test_that("serialize_order_for_query creates correct format", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_null(manager$serialize_order_for_query(NULL))
  expect_null(manager$serialize_order_for_query(list()))

  order <- list(
    list(column = 0, dir = "asc"),
    list(column = 2, dir = "desc")
  )
  expect_equal(manager$serialize_order_for_query(order), "0:asc,2:desc")

  single_order <- list(list(column = 1, dir = "desc"))
  expect_equal(manager$serialize_order_for_query(single_order), "1:desc")
})

test_that("deserialize_order_from_query parses correct format", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_null(manager$deserialize_order_from_query(NULL))
  expect_null(manager$deserialize_order_from_query(""))

  result <- manager$deserialize_order_from_query("0:asc,2:desc")
  expect_equal(length(result), 2)
  expect_equal(result[[1]]$column, 0)
  expect_equal(result[[1]]$dir, "asc")
  expect_equal(result[[2]]$column, 2)
  expect_equal(result[[2]]$dir, "desc")

  single <- manager$deserialize_order_from_query("1:desc")
  expect_equal(length(single), 1)
  expect_equal(single[[1]]$column, 1)

  # Invalid format filtered out
  invalid <- manager$deserialize_order_from_query("invalid,0:asc")
  expect_equal(length(invalid), 1)
  expect_equal(invalid[[1]]$column, 0)
})

test_that("serialize and deserialize order are inverse operations", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  original_order <- list(
    list(column = 0, dir = "asc"),
    list(column = 1, dir = "desc"),
    list(column = 3, dir = "asc")
  )

  serialized <- manager$serialize_order_for_query(original_order)
  deserialized <- manager$deserialize_order_from_query(serialized)

  expect_equal(length(deserialized), length(original_order))
  for (i in seq_along(original_order)) {
    expect_equal(deserialized[[i]]$column, original_order[[i]]$column)
    expect_equal(deserialized[[i]]$dir, original_order[[i]]$dir)
  }
})

test_that("sanitize_table_state normalizes DataTables state", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_null(manager$sanitize_table_state(NULL))

  raw_state <- list(
    start = 50,
    length = 25,
    order = list(list(0, "asc")),
    search = list(search = "test query")
  )

  result <- manager$sanitize_table_state(raw_state)
  expect_equal(result$start, 50L)
  expect_equal(result$length, 25L)
  expect_equal(result$search, "test query")
  expect_equal(length(result$order), 1)

  # Defaults for missing values
  minimal_state <- list(start = NA, length = -1)
  result_min <- manager$sanitize_table_state(minimal_state)
  expect_equal(result_min$start, 0L)
  expect_equal(result_min$length, 100L)
  expect_equal(result_min$search, "")
})

test_that("extract_table_state_from_query extracts table parameters", {
  mock_session <- list(clientData = list(url_search = ""))
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L)
  )
  manager <- URLStateManager$new(mock_session, list(), table_registry)

  params <- list(
    plots_start = "50",
    plots_length = "25",
    plots_order = "0:asc,1:desc",
    plots_search = "test"
  )

  result <- manager$extract_table_state_from_query("plots", params)
  expect_equal(result$start, 50L)
  expect_equal(result$length, 25L)
  expect_equal(result$search, "test")
  expect_equal(length(result$order), 2)

  # No state present returns NULL
  empty_params <- list()
  expect_null(manager$extract_table_state_from_query("plots", empty_params))
})

test_that("is_default_table_state detects default state", {
  mock_session <- list(clientData = list(url_search = ""))
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L)
  )
  manager <- URLStateManager$new(mock_session, list(), table_registry)

  expect_true(manager$is_default_table_state("plots", NULL))

  default_state <- list(start = 0, length = 100, order = NULL, search = "")
  expect_true(manager$is_default_table_state("plots", default_state))

  custom_state <- list(start = 50, length = 100, order = NULL, search = "")
  expect_false(manager$is_default_table_state("plots", custom_state))

  filtered_state <- list(start = 0, length = 100, order = NULL, search = "query")
  expect_false(manager$is_default_table_state("plots", filtered_state))

  sorted_state <- list(
    start = 0,
    length = 100,
    order = list(list(column = 0, dir = "asc")),
    search = ""
  )
  expect_false(manager$is_default_table_state("plots", sorted_state))
})

test_that("build_query_string constructs valid URLs", {
  mock_session <- list(clientData = list(url_search = ""))
  defaults <- list(map_lat = 39.8283, map_lng = -98.5795, map_zoom = 4)
  table_registry <- list(
    plots = list(tab = "Plots", table_id = "plot_table", default_length = 100L)
  )
  manager <- URLStateManager$new(mock_session, defaults, table_registry)

  # Basic tab only
  query <- manager$build_query_string(tab = "Overview")
  expect_match(query, "^\\?tab=Overview$")

  # Tab with detail overlay
  query_detail <- manager$build_query_string(
    tab = "Plots",
    detail_type = "plot-observation",
    detail_code = "VB.123"
  )
  expect_match(query_detail, "\\?tab=Plots")
  expect_match(query_detail, "detail=plot-observation")
  expect_match(query_detail, "code=VB\\.123")

  # Map tab with coordinates
  query_map <- manager$build_query_string(
    tab = "Map",
    map_lat = 40.5,
    map_lng = -100.2,
    map_zoom = 12
  )
  expect_match(query_map, "map_lat=40\\.50000")
  expect_match(query_map, "map_lng=-100\\.20000")
  expect_match(query_map, "map_zoom=12")

  # With table state
  table_states <- list(
    plots = list(start = 50, length = 25, order = list(list(column = 0, dir = "asc")), search = "test")
  )
  query_table <- manager$build_query_string(tab = "Plots", table_states = table_states)
  expect_match(query_table, "plots_start=50")
  expect_match(query_table, "plots_length=25")
  expect_match(query_table, "plots_order=0%3Aasc")
  expect_match(query_table, "plots_search=test")
})

test_that("build_query_string serializes plot_filter parameters", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  plot_filter <- list(type = "project", code = "pj.340", label = "My Project")
  query <- manager$build_query_string(tab = "Plots", plot_filter = plot_filter)

  # Verify the exact parameter names used in the URL
  expect_match(query, "plot_filter_code=pj\\.340")
  expect_match(query, "plot_filter_type=project")
  expect_match(query, "plot_filter_label=")

  # Ensure old parameter names are NOT used
  expect_no_match(query, "(?<![a-z_])filter_code=", perl = TRUE)
  expect_no_match(query, "(?<![a-z_])filter_type=", perl = TRUE)
  expect_no_match(query, "(?<![a-z_])filter_label=", perl = TRUE)
})

test_that("build_query_string serializes community_filter parameters", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  comm_filter <- list(type = "citation", code = "cc.1234", label = "Citation identifier: VB.CC.1234")
  query <- manager$build_query_string(tab = "Communities", community_filter = comm_filter)

  # Verify the exact parameter names used in the URL
  expect_match(query, "comm_filter_code=cc\\.1234")
  expect_match(query, "comm_filter_type=citation")
  expect_match(query, "comm_filter_label=")

  # Ensure plot filter params are NOT present
  expect_no_match(query, "plot_filter_code=")
})

test_that("build_query_string omits filter params when filters are NULL", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  query <- manager$build_query_string(tab = "Plots", plot_filter = NULL, community_filter = NULL)

  expect_no_match(query, "plot_filter_")
  expect_no_match(query, "comm_filter_")
})

test_that("plot_filter URL params roundtrip through build and parse", {
  # Build a query string with a plot filter
  build_session <- list(clientData = list(url_search = ""))
  builder <- URLStateManager$new(build_session)

  original_filter <- list(type = "party", code = "py.200", label = "Smith, J.")
  query <- builder$build_query_string(tab = "Plots", plot_filter = original_filter)

  # Parse it back using a new manager seeded with the built query
  parse_session <- list(clientData = list(url_search = query))
  parser <- URLStateManager$new(parse_session)
  params <- parser$parse_query_string()

  # Verify the parsed parameter names match what server.R's URL observer reads
  expect_equal(parser$first_param(params$plot_filter_code), "py.200")
  expect_equal(parser$first_param(params$plot_filter_type), "party")
  expect_true(parser$is_valid_param(params$plot_filter_label))
})

test_that("community_filter URL params roundtrip through build and parse", {
  # Build a query string with a community filter
  build_session <- list(clientData = list(url_search = ""))
  builder <- URLStateManager$new(build_session)

  original_filter <- list(type = "citation", code = "cc.5678", label = "Citation identifier: VB.CC.5678")
  query <- builder$build_query_string(tab = "Communities", community_filter = original_filter)

  # Parse it back using a new manager seeded with the built query
  parse_session <- list(clientData = list(url_search = query))
  parser <- URLStateManager$new(parse_session)
  params <- parser$parse_query_string()

  # Verify the parsed parameter names match what server.R's URL observer reads
  expect_equal(parser$first_param(params$comm_filter_code), "cc.5678")
  expect_equal(parser$first_param(params$comm_filter_type), "citation")
  expect_true(parser$is_valid_param(params$comm_filter_label))
})

test_that("both filters can coexist in the same query string", {
  mock_session <- list(clientData = list(url_search = ""))
  builder <- URLStateManager$new(mock_session)

  plot_filter <- list(type = "project", code = "pj.100", label = "Project A")
  comm_filter <- list(type = "citation", code = "cc.200", label = "Cited concept")
  query <- builder$build_query_string(
    tab = "Plots",
    plot_filter = plot_filter,
    community_filter = comm_filter
  )

  # Parse back
  parse_session <- list(clientData = list(url_search = query))
  parser <- URLStateManager$new(parse_session)
  params <- parser$parse_query_string()

  # Both should be independently retrievable
  expect_equal(parser$first_param(params$plot_filter_code), "pj.100")
  expect_equal(parser$first_param(params$comm_filter_code), "cc.200")
  expect_equal(parser$first_param(params$plot_filter_type), "project")
  expect_equal(parser$first_param(params$comm_filter_type), "citation")
})

test_that("parse_query_string extracts parameters", {
  mock_session <- list(
    clientData = list(url_search = "?tab=Plots&detail=plot-observation&code=VB.123")
  )
  manager <- URLStateManager$new(mock_session)

  params <- manager$parse_query_string()
  expect_equal(params$tab, "Plots")
  expect_equal(params$detail, "plot-observation")
  expect_equal(params$code, "VB.123")

  # Empty query string
  mock_session_empty <- list(clientData = list(url_search = ""))
  manager_empty <- URLStateManager$new(mock_session_empty)
  expect_equal(manager_empty$parse_query_string(), list())
})

test_that("updating flag prevents circular updates", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_false(manager$is_updating())

  manager$set_updating(TRUE)
  expect_true(manager$is_updating())

  manager$set_updating(FALSE)
  expect_false(manager$is_updating())
})

test_that("history initialization flag works correctly", {
  mock_session <- list(clientData = list(url_search = ""))
  manager <- URLStateManager$new(mock_session)

  expect_false(manager$is_history_initialized())

  manager$set_history_initialized(TRUE)
  expect_true(manager$is_history_initialized())

  manager$set_history_initialized(FALSE)
  expect_false(manager$is_history_initialized())
})

test_that("update_query_string only updates when different", {
  update_called <- FALSE
  mock_session <- list(
    clientData = list(url_search = "?tab=Overview"),
    sendCustomMessage = function(...) NULL
  )

  # Mock shiny::updateQueryString
  with_mocked_bindings(
    updateQueryString = function(query, mode, session) {
      update_called <<- TRUE
    },
    .package = "shiny",
    {
      manager <- URLStateManager$new(mock_session)

      # Same query - should not update
      update_called <- FALSE
      result <- manager$update_query_string("?tab=Overview", mode = "push")
      expect_false(result)
      expect_false(update_called)

      # Different query - should update
      update_called <- FALSE
      result <- manager$update_query_string("?tab=Plots", mode = "push")
      expect_true(result)
      expect_true(update_called)
    }
  )
})
