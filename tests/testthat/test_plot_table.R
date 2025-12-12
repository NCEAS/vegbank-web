test_that("plot schema template returns correct structure", {
  result <- vegbankweb:::PLOT_TABLE_SCHEMA_TEMPLATE

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), length(PLOT_TABLE_FIELDS))
  expect_equal(names(result), PLOT_TABLE_FIELDS)

  # Check nested list columns
  expect_type(result$top_taxon_observations, "list")
  expect_type(result$top_classifications, "list")
})

test_that("serialize_nested_column converts data frames to JSON strings", {
  nested_data <- list(
    data.frame(to_code = "to.1", scientific_name = "Species A", stringsAsFactors = FALSE),
    data.frame(to_code = "to.2", scientific_name = "Species B", stringsAsFactors = FALSE),
    NULL,
    data.frame()
  )

  result <- serialize_nested_column(nested_data)

  expect_type(result, "character")
  expect_equal(length(result), 4)
  expect_false(result[1] == "[]")
  expect_false(result[2] == "[]")
  expect_equal(result[3], "[]")
  expect_equal(result[4], "[]")

  # Verify JSON can be parsed back
  parsed1 <- jsonlite::fromJSON(result[1])
  expect_equal(parsed1$to_code, "to.1")
  expect_equal(parsed1$scientific_name, "Species A")
})

test_that("serialize_nested_column handles NULL and non-list inputs", {
  result_null <- serialize_nested_column(NULL)
  expect_type(result_null, "character")
  expect_equal(length(result_null), 0)

  # Non-list input should generate a warning and return empty arrays
  expect_warning(
    result_non_list <- serialize_nested_column(c("a", "b")),
    "expected a list"
  )
  expect_type(result_non_list, "character")
  expect_equal(result_non_list, c("[]", "[]"))
})

test_that("serialize_taxa_payload encodes totals and rows", {
  payload <- vegbankweb:::serialize_taxa_payload(
    data.frame(name = "Species A", pc_code = "pc.1", stringsAsFactors = FALSE),
    total_count = 4
  )

  parsed <- jsonlite::fromJSON(payload)
  expect_equal(parsed$total, 4)
  expect_equal(parsed$items$name, "Species A")

  payload_no_total <- vegbankweb:::serialize_taxa_payload(NULL, NA)
  parsed_no_total <- jsonlite::fromJSON(payload_no_total)
  expect_null(parsed_no_total$total)
  empty_items <- parsed_no_total$items
  expect_true(
    is.null(empty_items) ||
      (is.data.frame(empty_items) && nrow(empty_items) == 0) ||
      (is.list(empty_items) && length(empty_items) == 0)
  )
})

test_that("format_location_column combines location, coordinates, and elevation", {
  data <- data.frame(
    state_province = c("Virginia", "Maryland", NA, "Texas"),
    country = c("USA", "USA", "Canada", NA),
    stringsAsFactors = FALSE
  )
  lats <- c(38.5, 39.2, 45.0, NA)
  lngs <- c(-78.2, -77.0, -75.0, NA)
  elevs <- c(500, 250, NA, 120)

  result <- format_location_column(data, lats, lngs, elevs)

  expect_equal(
    result[1],
    "Virginia<br><span class=\"text-muted small\">USA</span><br><span class=\"text-muted small\">38.5000, -78.2000 &bull; 500m</span>"
  )
  expect_equal(
    result[2],
    "Maryland<br><span class=\"text-muted small\">USA</span><br><span class=\"text-muted small\">39.2000, -77.0000 &bull; 250m</span>"
  )
  expect_equal(
    result[3],
    "<span class=\"text-muted small\">Canada</span><br><span class=\"text-muted small\">45.0000, -75.0000</span>"
  )
  expect_equal(
    result[4],
    "Texas<br><span class=\"text-muted small\">120m</span>"
  )
})

test_that("format_location_column handles missing data", {
  data <- data.frame(
    state_province = c(NA, ""),
    country = c(NA, ""),
    stringsAsFactors = FALSE
  )
  lats <- c(NA, NA)
  lngs <- c(NA, NA)
  elevs <- c(NA, NA)

  result <- format_location_column(data, lats, lngs, elevs)

  expect_equal(result[1], "Not provided")
  expect_equal(result[2], "Not provided")
})

test_that("process_plot_data returns correctly formatted display data", {
  plot_data <- data.frame(
    ob_code = c("ob.123", "ob.456"),
    pl_code = c("pl.1", "pl.2"),
    author_plot_code = c("PLOT001", "PLOT002"),
    author_obs_code = c("OBS001", "OBS002"),
    state_province = c("Virginia", "Maryland"),
    country = c("USA", "USA"),
    latitude = c(38.5, 39.2),
    longitude = c(-78.2, -77.0),
    elevation = c(500, 750),
    area = c(100, 200),
    year = c("2020", "2021"),
    taxon_count = c(5L, 3L),
    taxon_count_returned = c(3L, 2L),
    stringsAsFactors = FALSE
  )
  plot_data$top_taxon_observations <- list(
    data.frame(to_code = "to.1", scientific_name = "Species A", stringsAsFactors = FALSE),
    data.frame(to_code = "to.2", scientific_name = "Species B", stringsAsFactors = FALSE)
  )
  plot_data$top_classifications <- list(
    data.frame(cl_code = "cl.1", class_name = "Class A", stringsAsFactors = FALSE),
    data.frame(cl_code = "cl.2", class_name = "Class B", stringsAsFactors = FALSE)
  )

  result <- process_plot_data(plot_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 6)

  expected_cols <- c("Actions", "Author Plot Code", "Location", "Top Taxa", "Communities", "Year")
  expect_equal(names(result), expected_cols)

  # Check action payloads encode detail code plus map metadata
  action_payloads <- lapply(result$Actions, jsonlite::fromJSON)
  expect_equal(
    vapply(action_payloads, function(payload) payload$detail_code, character(1)),
    c("ob.123", "ob.456")
  )
  expect_equal(
    vapply(action_payloads, function(payload) payload$map$code, character(1)),
    c("OBS001", "OBS002")
  )
  expect_equal(
    vapply(action_payloads, function(payload) payload$map$lat, numeric(1)),
    c(38.5, 39.2)
  )
  expect_equal(
    vapply(action_payloads, function(payload) payload$map$lng, numeric(1)),
    c(-78.2, -77.0)
  )

  # Check other data values
  expect_equal(result$`Author Plot Code`, c("OBS001", "OBS002"))
  expect_equal(
    result$Location,
    c(
      "Virginia<br><span class=\"text-muted small\">USA</span><br><span class=\"text-muted small\">38.5000, -78.2000 &bull; 500m</span>",
      "Maryland<br><span class=\"text-muted small\">USA</span><br><span class=\"text-muted small\">39.2000, -77.0000 &bull; 750m</span>"
    )
  )
  expect_equal(result$Year, c("2020", "2021"))

  # Check JSON columns are strings
  expect_type(result$`Top Taxa`, "character")
  expect_type(result$Communities, "character")
  expect_false(result$`Top Taxa`[1] == "[]")
  expect_false(result$Communities[1] == "[]")

  taxa_payloads <- lapply(result$`Top Taxa`, jsonlite::fromJSON)
  expect_equal(
    vapply(taxa_payloads, function(payload) if (is.null(payload$total)) NA_real_ else payload$total, numeric(1)),
    c(5, 3)
  )
  expect_equal(
    vapply(taxa_payloads, function(payload) nrow(payload$items), integer(1)),
    c(1L, 1L)
  )
})

test_that("process_plot_data handles empty data correctly", {
  result <- process_plot_data(NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 6)

  result_empty <- process_plot_data(vegbankweb:::PLOT_TABLE_SCHEMA_TEMPLATE)
  expect_equal(nrow(result_empty), 0)
})

test_that("build_plot_table returns DataTable object", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
      expect_type(table_config, "list")

      # Return mock DataTable
      structure(list(x = list(data = data.frame())), class = "datatables")
    },
    .env = pkg_env,
    {
      result <- build_plot_table()
      expect_s3_class(result, "datatables")
    }
  )
})

test_that("plot table spec produces valid config", {
  config <- build_table_config_from_spec(vegbankweb:::PLOT_TABLE_SPEC)

  expect_type(config, "list")
  expect_true("column_defs" %in% names(config))
  expect_true("initial_data" %in% names(config))
  expect_true("ajax" %in% names(config))

  expect_equal(length(config$column_defs), 6)

  # Check actions column (index 0)
  expect_equal(config$column_defs[[1]]$targets, 0)
  expect_false(config$column_defs[[1]]$orderable)
  expect_false(config$column_defs[[1]]$searchable)
  expect_true(inherits(config$column_defs[[1]]$render, "JS_EVAL"))

  # Check taxon list column (index 3)
  expect_equal(config$column_defs[[4]]$targets, 3)
  expect_false(config$column_defs[[4]]$orderable)
  expect_true(inherits(config$column_defs[[4]]$render, "JS_EVAL"))

  # Check community list column (index 4)
  expect_equal(config$column_defs[[5]]$targets, 4)
  expect_false(config$column_defs[[5]]$orderable)
  expect_true(inherits(config$column_defs[[5]]$render, "JS_EVAL"))

  # Check initial data
  expect_s3_class(config$initial_data, "data.frame")
  expect_equal(nrow(config$initial_data), 0)

  # Check AJAX function and data source spec
  expect_true(is.function(config$ajax))
  ajax_env <- environment(config$ajax)
  spec <- ajax_env$data_source_spec

  expect_equal(spec$table_id, "plot_table")
  expect_equal(spec$resource, "plot-observations")
  expect_equal(spec$detail, "minimal")
  expect_equal(spec$query$with_nested, "TRUE")
})

test_that("create_plot_action_renderer returns JS function", {
  renderer <- create_plot_action_renderer()
  expect_s3_class(renderer, "JS_EVAL")
  expect_type(renderer, "character")
  expect_true(grepl("dt-map-action", renderer))
  expect_true(grepl('data-input-id="plot_link_click"', renderer, fixed = TRUE))
})

test_that("create_taxon_list_renderer returns JS function", {
  renderer <- create_taxon_list_renderer()

  expect_s3_class(renderer, "JS_EVAL")
  expect_type(renderer, "character")
  expect_true(grepl("function\\(data, type, row, meta\\)", renderer))
  expect_true(grepl("plant_link_click", renderer))
  expect_true(grepl("pc_code", renderer))
  expect_true(grepl("<div>", renderer, fixed = TRUE))
})

test_that("create_community_list_renderer returns JS function", {
  renderer <- create_community_list_renderer()

  expect_s3_class(renderer, "JS_EVAL")
  expect_type(renderer, "character")
  expect_true(grepl("function\\(data, type, row, meta\\)", renderer))
  expect_true(grepl("comm_class_link_click", renderer))
  expect_true(grepl("comm_name", renderer))
  expect_true(grepl("comm_code", renderer))
  expect_true(grepl("CEGL", renderer))
})
