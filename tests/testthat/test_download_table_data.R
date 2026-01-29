test_that("TABLE_DOWNLOAD_CONFIG has correct structure", {
  expect_true(is.list(TABLE_DOWNLOAD_CONFIG))
  expect_true("plot_table" %in% names(TABLE_DOWNLOAD_CONFIG))

  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  expect_equal(config$resource, "plot-observations")
  expect_true(config$has_nested)
  expect_equal(config$primary_key, "ob_code")
})

test_that("get_table_filter_state extracts search term", {
  # Create a mock state with a function that returns NULL (simulating reactive behavior)
  input <- list(plot_table_search = "oak")
  state <- list(plot_filter = function() NULL)

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_equal(result$search, "oak")
  expect_null(result$filter)
})

test_that("get_table_filter_state extracts cross-resource filter", {
  input <- list(plot_table_search = NULL)
  state <- list(plot_filter = function() list(type = "project", code = "pj.123", label = "Test Project"))

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_null(result$search)
  expect_equal(result$filter$type, "project")
  expect_equal(result$filter$code, "pj.123")
})

test_that("detect_nested_columns identifies nested data frames", {
  data <- data.frame(
    ob_code = c("ob.1", "ob.2"),
    latitude = c(38.5, 39.0),
    top_taxon_observations = I(list(
      data.frame(name = "Oak", cover = 25),
      data.frame(name = "Pine", cover = 30)
    )),
    top_classifications = I(list(
      data.frame(comm_name = "Forest"),
      data.frame(comm_name = "Wetland")
    ))
  )

  result <- detect_nested_columns(data)

  expect_true(is.character(result))
  expect_true("top_taxon_observations" %in% result)
  expect_true("top_classifications" %in% result)
  expect_false("ob_code" %in% result)
  expect_false("latitude" %in% result)
})

test_that("detect_nested_columns returns empty for data without nested columns", {
  data <- data.frame(
    ob_code = c("ob.1", "ob.2"),
    latitude = c(38.5, 39.0)
  )

  result <- detect_nested_columns(data)

  expect_true(is.character(result))
  expect_equal(length(result), 0)
})

test_that("extract_nested_table_with_fk handles empty data", {
  data <- data.frame(ob_code = character(0))

  result <- extract_nested_table_with_fk(data, "top_taxon_observations", "ob_code")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("extract_nested_table_with_fk extracts and adds foreign key", {
  data <- data.frame(
    ob_code = c("ob.1", "ob.2"),
    top_taxon_observations = I(list(
      data.frame(name = c("Oak", "Maple"), cover = c(25, 15)),
      data.frame(name = c("Pine"), cover = c(30))
    ))
  )

  result <- extract_nested_table_with_fk(data, "top_taxon_observations", "ob_code")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("ob_code" %in% names(result))
  expect_equal(result$ob_code, c("ob.1", "ob.1", "ob.2"))
  expect_equal(result$name, c("Oak", "Maple", "Pine"))
})

test_that("prepare_csv_tables splits nested data correctly", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table

  data <- data.frame(
    ob_code = c("ob.1", "ob.2"),
    latitude = c(38.5, 39.0),
    longitude = c(-78.2, -79.0),
    top_taxon_observations = I(list(
      data.frame(name = "Oak", cover = 25),
      data.frame(name = "Pine", cover = 30)
    )),
    top_classifications = I(list(
      data.frame(comm_name = "Forest"),
      data.frame()
    )),
    disturbances = I(list(data.frame(), data.frame())),
    soils = I(list(data.frame(), data.frame()))
  )

  result <- prepare_csv_tables(data, config)

  expect_true(is.list(result))
  expect_true("main" %in% names(result))
  # Check for new friendly names (not old API names)
  expect_true("taxon_observations" %in% names(result))
  expect_true("community_classifications" %in% names(result))

  # Main table should not have nested columns
  expect_false("top_taxon_observations" %in% names(result$main))
  expect_true("latitude" %in% names(result$main))

  # taxon_observations table should have foreign key
  expect_true("ob_code" %in% names(result$taxon_observations))
  expect_equal(nrow(result$taxon_observations), 2)

  # community_classifications table should have data
  expect_true("ob_code" %in% names(result$community_classifications))
  expect_equal(nrow(result$community_classifications), 1)
})

test_that("create_download_readme generates valid content", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = "oak",
    filter = list(type = "project", code = "pj.123", label = "Test Project")
  )
  csv_tables <- list(
    main = data.frame(ob_code = "ob.1"),
    taxon_observations = data.frame(ob_code = "ob.1", name = "Oak")
  )

  readme <- create_download_readme(config, filter_state, 100, csv_tables)

  expect_true(is.character(readme))
  expect_true(grepl("VegBank Data Download", readme))
  expect_true(grepl("Records: 100", readme))
  expect_true(grepl("Search Filter: oak", readme))
  expect_true(grepl("Resource Filter: project = pj.123", readme))
  expect_true(grepl("main.csv", readme))
  expect_true(grepl("taxon_observations.csv", readme))
})

test_that("DOWNLOAD_MAX_RECORDS is set correctly", {
  expect_equal(DOWNLOAD_MAX_RECORDS, 20000L)
})

test_that("get_table_filter_state handles both search and filter", {
  input <- list(plot_table_search = "pine")
  state <- list(plot_filter = function() list(type = "party", code = "pt.456", label = "Test Party"))

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_equal(result$search, "pine")
  expect_equal(result$filter$type, "party")
  expect_equal(result$filter$code, "pt.456")
  expect_equal(result$filter$label, "Test Party")
})

test_that("get_table_filter_state handles whitespace in search", {
  input <- list(plot_table_search = "  oak  ")
  state <- list(plot_filter = function() NULL)

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_equal(result$search, "  oak  ")
})

test_that("get_table_filter_state handles invalid filter info", {
  input <- list(plot_table_search = NULL)
  # Filter missing required fields
  state <- list(plot_filter = function() list(type = "project"))

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_null(result$search)
  expect_null(result$filter)
})

test_that("detect_nested_columns handles NULL data", {
  result <- detect_nested_columns(NULL)
  expect_equal(length(result), 0)
})

test_that("detect_nested_columns handles empty lists", {
  data <- data.frame(
    ob_code = c("ob.1"),
    empty_list = I(list(list()))
  )

  result <- detect_nested_columns(data)

  expect_false("empty_list" %in% result)
})

test_that("detect_nested_columns handles mixed list types", {
  data <- data.frame(
    ob_code = c("ob.1", "ob.2"),
    nested_df = I(list(
      data.frame(x = 1),
      data.frame(x = 2)
    )),
    nested_list = I(list(list(a = 1), list(b = 2))),
    nested_vector = I(list(c(1, 2), c(3, 4)))
  )

  result <- detect_nested_columns(data)

  expect_true("nested_df" %in% result)
  expect_false("nested_list" %in% result)
  expect_false("nested_vector" %in% result)
})

test_that("extract_nested_table_with_fk handles missing column", {
  data <- data.frame(
    ob_code = c("ob.1", "ob.2"),
    latitude = c(38.5, 39.0)
  )

  result <- extract_nested_table_with_fk(data, "nonexistent_column", "ob_code")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("extract_nested_table_with_fk handles NULL values in nested column", {
  data <- data.frame(
    ob_code = c("ob.1", "ob.2", "ob.3"),
    nested = I(list(
      data.frame(val = 1),
      NULL,
      data.frame(val = 2)
    ))
  )

  result <- extract_nested_table_with_fk(data, "nested", "ob_code")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$ob_code, c("ob.1", "ob.3"))
})

test_that("extract_nested_table_with_fk preserves column order", {
  data <- data.frame(
    ob_code = c("ob.1"),
    nested = I(list(
      data.frame(z = 3, y = 2, x = 1)
    ))
  )

  result <- extract_nested_table_with_fk(data, "nested", "ob_code")

  # Foreign key should be first
  expect_equal(names(result)[1], "ob_code")
  # Other columns should follow
  expect_true(all(c("z", "y", "x") %in% names(result)))
})

test_that("prepare_csv_tables handles data without nested columns", {
  config <- list(
    has_nested = FALSE,
    primary_key = "id"
  )

  data <- data.frame(
    id = c(1, 2),
    value = c("a", "b")
  )

  result <- prepare_csv_tables(data, config)

  expect_true("main" %in% names(result))
  expect_equal(length(result), 1)
  expect_equal(nrow(result$main), 2)
})

test_that("prepare_csv_tables removes all list columns from main table", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table

  data <- data.frame(
    ob_code = c("ob.1"),
    latitude = 38.5,
    nested1 = I(list(data.frame(x = 1))),
    nested2 = I(list(data.frame(y = 2))),
    regular = "value"
  )

  result <- prepare_csv_tables(data, config)

  expect_false("nested1" %in% names(result$main))
  expect_false("nested2" %in% names(result$main))
  expect_true("latitude" %in% names(result$main))
  expect_true("regular" %in% names(result$main))
})

test_that("prepare_csv_tables skips empty nested tables", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table

  data <- data.frame(
    ob_code = c("ob.1", "ob.2"),
    nested_empty = I(list(data.frame(), data.frame())),
    nested_full = I(list(data.frame(x = 1), data.frame(x = 2)))
  )

  result <- prepare_csv_tables(data, config)

  expect_false("nested_empty" %in% names(result))
  expect_true("nested_full" %in% names(result))
})

test_that("create_download_readme handles no filters", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = NULL, filter = NULL)
  csv_tables <- list(main = data.frame(ob_code = "ob.1"))

  readme <- create_download_readme(config, filter_state, 50, csv_tables)

  expect_true(grepl("Records: 50", readme))
  expect_false(grepl("Search Filter:", readme))
  expect_false(grepl("Resource Filter:", readme))
})

test_that("create_download_readme includes filter label", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "project", code = "pj.789", label = "My Project")
  )
  csv_tables <- list(main = data.frame(ob_code = "ob.1"))

  readme <- create_download_readme(config, filter_state, 10, csv_tables)

  expect_true(grepl("My Project", readme))
})

test_that("create_download_readme handles filter without label", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "project", code = "pj.789")
  )
  csv_tables <- list(main = data.frame(ob_code = "ob.1"))

  readme <- create_download_readme(config, filter_state, 10, csv_tables)

  expect_true(grepl("project = pj.789", readme))
  expect_false(grepl("\\(\\)", readme)) # Should not have empty parentheses
})

test_that("create_download_readme includes nested table descriptions", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = NULL, filter = NULL)
  csv_tables <- list(
    main = data.frame(ob_code = "ob.1"),
    taxon_observations = data.frame(ob_code = "ob.1", name = "Oak"),
    community_classifications = data.frame(ob_code = "ob.1", comm = "Forest")
  )

  readme <- create_download_readme(config, filter_state, 1, csv_tables)

  expect_true(grepl("taxon_observations.csv", readme))
  expect_true(grepl("community_classifications.csv", readme))
  expect_true(grepl("linked by ob_code", readme))
  expect_true(grepl("Example \\(R\\)", readme))
  expect_true(grepl("merge\\(", readme))
})

test_that("create_download_readme handles single main table", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = NULL, filter = NULL)
  csv_tables <- list(main = data.frame(ob_code = "ob.1"))

  readme <- create_download_readme(config, filter_state, 1, csv_tables)

  expect_true(grepl("main.csv", readme))
  expect_false(grepl("Example \\(R\\)", readme))
})

