test_that("create_empty_plot_df returns correct schema", {
  result <- create_empty_plot_df()
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), length(PLOT_TABLE_FIELDS))
  expect_equal(names(result), PLOT_TABLE_FIELDS)
  
  # Check nested list columns
  expect_type(result$top_taxon_observations, "list")
  expect_type(result$top_classifications, "list")
})

test_that("normalize_plot_data handles NULL and empty inputs", {
  result_null <- normalize_plot_data(NULL)
  expect_s3_class(result_null, "data.frame")
  expect_equal(nrow(result_null), 0)
  expect_equal(names(result_null), PLOT_TABLE_FIELDS)
  
  result_empty <- normalize_plot_data(data.frame())
  expect_s3_class(result_empty, "data.frame")
  expect_equal(nrow(result_empty), 0)
})

test_that("normalize_plot_data ensures all required fields exist", {
  partial_data <- data.frame(
    ob_code = "ob.123",
    pl_code = "pl.456",
    stringsAsFactors = FALSE
  )
  
  result <- normalize_plot_data(partial_data)
  
  expect_equal(names(result), PLOT_TABLE_FIELDS)
  expect_equal(result$ob_code, "ob.123")
  expect_equal(result$pl_code, "pl.456")
  expect_true(is.na(result$state_province))
  expect_type(result$top_taxon_observations, "list")
})

test_that("normalize_plot_data performs type coercion correctly", {
  raw_data <- data.frame(
    ob_code = "ob.123",
    pl_code = "pl.456",
    author_plot_code = "PLOT001",
    author_obs_code = "OBS001",
    state_province = "Virginia",
    country = "USA",
    latitude = "38.5",
    longitude = "-78.2",
    elevation = "500",
    area = "100",
    year = "2020",
    taxon_count = "5",
    taxon_count_returned = "3",
    stringsAsFactors = FALSE
  )
  raw_data$top_taxon_observations <- list(data.frame(to_code = "to.1", scientific_name = "Species A"))
  raw_data$top_classifications <- list(data.frame(cl_code = "cl.1", class_name = "Class A"))
  
  result <- normalize_plot_data(raw_data)
  
  expect_type(result$ob_code, "character")
  expect_type(result$latitude, "double")
  expect_type(result$longitude, "double")
  expect_type(result$elevation, "double")
  expect_equal(result$latitude, 38.5)
  expect_equal(result$longitude, -78.2)
  expect_equal(result$elevation, 500)
  expect_type(result$taxon_count, "integer")
  expect_equal(result$taxon_count, 5L)
})

test_that("coerce_plot_page handles various input types", {
  result_null <- coerce_plot_page(NULL)
  expect_s3_class(result_null, "data.frame")
  expect_equal(nrow(result_null), 0)
  
  df <- data.frame(ob_code = "ob.1", stringsAsFactors = FALSE)
  result_df <- coerce_plot_page(df)
  expect_identical(result_df, df)
  
  list_with_data <- list(data = df)
  result_list <- coerce_plot_page(list_with_data)
  expect_identical(result_list, df)
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

test_that("format_location_column combines state and country", {
  data <- data.frame(
    state_province = c("Virginia", "Maryland", NA, "Texas"),
    country = c("USA", "USA", "Canada", NA),
    stringsAsFactors = FALSE
  )
  
  result <- format_location_column(data)
  
  expect_equal(result[1], "Virginia, USA")
  expect_equal(result[2], "Maryland, USA")
  expect_equal(result[3], "Canada")
  expect_equal(result[4], "Texas")
})

test_that("format_location_column handles missing data", {
  data <- data.frame(
    state_province = c(NA, ""),
    country = c(NA, ""),
    stringsAsFactors = FALSE
  )
  
  result <- format_location_column(data)
  
  expect_equal(result[1], "Not provided")
  expect_equal(result[2], "Not provided")
})

test_that("format_coordinate handles numeric conversion", {
  values <- c("38.5", "-78.2", "invalid", NA, "")
  
  result <- format_coordinate(values)
  
  expect_type(result, "double")
  expect_equal(result[1], 38.5)
  expect_equal(result[2], -78.2)
  expect_true(is.na(result[3]))
  expect_true(is.na(result[4]))
  expect_true(is.na(result[5]))
})

test_that("format_elevation formats values correctly", {
  values <- c("500", "1234.56", "invalid", NA, "")
  
  result <- format_elevation(values)
  
  expect_equal(result[1], "500")
  expect_equal(result[2], "1235")
  expect_equal(result[3], "Not provided")
  expect_equal(result[4], "Not provided")
  expect_equal(result[5], "Not provided")
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
  expect_equal(ncol(result), 9)
  
  expected_cols <- c("Actions", "Author Plot Code", "Location", "Latitude", "Longitude", 
                     "Elevation (m)", "Year", "Top Taxa", "Communities")
  expect_equal(names(result), expected_cols)
  
  # Check action payloads encode detail code plus map metadata
  action_payloads <- lapply(result$Actions, jsonlite::fromJSON)
  expect_equal(vapply(action_payloads, function(payload) payload$detail_code, character(1)),
               c("ob.123", "ob.456"))
  expect_equal(vapply(action_payloads, function(payload) payload$map$code, character(1)),
               c("OBS001", "OBS002"))
  expect_equal(vapply(action_payloads, function(payload) payload$map$lat, numeric(1)),
               c(38.5, 39.2))
  expect_equal(vapply(action_payloads, function(payload) payload$map$lng, numeric(1)),
               c(-78.2, -77.0))

  # Check other data values
  expect_equal(result$`Author Plot Code`, c("PLOT001", "PLOT002"))
  expect_equal(result$Location, c("Virginia, USA", "Maryland, USA"))
  expect_equal(result$Latitude, c(38.5, 39.2))
  expect_equal(result$Longitude, c(-78.2, -77.0))
  expect_equal(result$`Elevation (m)`, c("500", "750"))
  expect_equal(result$Year, c("2020", "2021"))
  
  # Check JSON columns are strings
  expect_type(result$`Top Taxa`, "character")
  expect_type(result$Communities, "character")
  expect_false(result$`Top Taxa`[1] == "[]")
  expect_false(result$Communities[1] == "[]")
})

test_that("process_plot_data handles empty data correctly", {
  result <- process_plot_data(NULL)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 9)
  
  result_empty <- process_plot_data(create_empty_plot_df())
  expect_equal(nrow(result_empty), 0)
})

test_that("build_plot_table returns DataTable object", {
  pkg_env <- asNamespace("vegbankweb")
  
  with_mocked_bindings(
    create_table = function(data_sources, required_sources, process_function, table_config) {
      expect_equal(data_sources, list())
      expect_equal(required_sources, character(0))
      expect_null(process_function)
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

test_that("create_plot_table_config returns valid config", {
  config <- create_plot_table_config()
  
  expect_type(config, "list")
  expect_true("column_defs" %in% names(config))
  expect_true("initial_data" %in% names(config))
  expect_true("ajax" %in% names(config))
  
  expect_equal(length(config$column_defs), 9)
  
  # Check actions column (index 0)
  expect_equal(config$column_defs[[1]]$targets, 0)
  expect_false(config$column_defs[[1]]$orderable)
  expect_false(config$column_defs[[1]]$searchable)
  expect_true(inherits(config$column_defs[[1]]$render, "JS_EVAL"))
  
  # Check taxon list column (index 7)
  expect_equal(config$column_defs[[8]]$targets, 7)
  expect_false(config$column_defs[[8]]$orderable)
  expect_true(inherits(config$column_defs[[8]]$render, "JS_EVAL"))
  
  # Check community list column (index 8)
  expect_equal(config$column_defs[[9]]$targets, 8)
  expect_false(config$column_defs[[9]]$orderable)
  expect_true(inherits(config$column_defs[[9]]$render, "JS_EVAL"))
  
  # Check initial data
  expect_s3_class(config$initial_data, "data.frame")
  expect_equal(nrow(config$initial_data), 0)
  
  # Check AJAX function and data source spec
  expect_true(is.function(config$ajax))
  ajax_env <- environment(config$ajax)
  spec <- ajax_env$data_source_spec
  
  expect_equal(spec$table_id, "plot_table")
  expect_equal(spec$endpoint, "plot-observations")
  expect_equal(spec$detail, "minimal")
  expect_equal(spec$query$with_nested, "TRUE")
})

test_that("create_plot_action_renderer returns JS function", {
  renderer <- create_plot_action_renderer()
  expect_s3_class(renderer, "JS_EVAL")
  expect_type(renderer, "character")
  expect_true(grepl("dt-map-action", renderer))
  expect_true(grepl('data-input-id="see_obs_details"', renderer, fixed = TRUE))
})

test_that("create_taxon_list_renderer returns JS function", {
  renderer <- create_taxon_list_renderer()
  
  expect_s3_class(renderer, "JS_EVAL")
  expect_type(renderer, "character")
  expect_true(grepl("function\\(data, type, row, meta\\)", renderer))
  expect_true(grepl("plant_link_click", renderer))
  expect_true(grepl("pc_code", renderer))
})

test_that("create_community_list_renderer returns JS function", {
  renderer <- create_community_list_renderer()
  
  expect_s3_class(renderer, "JS_EVAL")
  expect_type(renderer, "character")
  expect_true(grepl("function\\(data, type, row, meta\\)", renderer))
  expect_true(grepl("comm_class_link_click", renderer))
  expect_true(grepl("comm_name", renderer))
})
