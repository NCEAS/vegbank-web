load_table_module_env <- function() {
  candidates <- c(
    testthat::test_path("..", "..", "R", "table.R"),
    file.path(system.file("tests", package = "vegbankweb"), "..", "vegbankweb", "R", "table.R"),
    file.path(system.file(package = "vegbankweb"), "R", "table.R")
  )

  candidates <- unique(normalizePath(candidates[file.exists(candidates)], mustWork = FALSE))

  if (!length(candidates)) {
    testthat::skip("table.R source not available for create_table tests")
  }

  table_env <- new.env(parent = baseenv())
  sys.source(candidates[[1]], envir = table_env)
  table_env
}

test_that("create_table uses provided initial data", {
  skip_if_not_installed("DT")

  table_env <- load_table_module_env()
  create_table_fn <- table_env$create_table

  test_df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)

  widget <- create_table_fn(table_config = list(initial_data = test_df))

  expect_s3_class(widget, "datatables")
  expect_equal(widget$x$data, test_df)
})

test_that("create_table falls back to empty data when initial_data missing", {
  skip_if_not_installed("DT")

  table_env <- load_table_module_env()
  create_table_fn <- table_env$create_table

  widget <- create_table_fn(table_config = list())

  expect_s3_class(widget, "datatables")
  expect_equal(nrow(widget$x$data), 0)
})

test_that("create_action_buttons generates HTML buttons correctly", {
  # Test data
  code_values <- c("id1", "id2")
  input_id <- "test_click"
  button_label <- "Test"
  # Test the function
  result <- create_action_buttons(input_id, button_label, code_values)
  expect_type(result, "character")
})

test_that("clean_column_data handles all cases correctly", {
  # Test data with various data types and edge cases
  test_data <- data.frame(
    string_col = c("value", NA_character_, "", "  ", "normal"),
    num_col = c(1, NA_real_, 3, 4, 5),
    factor_col = factor(c("a", NA, "c", "d", "e")),
    logical_col = c(TRUE, FALSE, NA, TRUE, FALSE),
    date_col = as.Date(c("2023-01-01", NA, "2023-03-01", "2023-04-01", "2023-05-01")),
    stringsAsFactors = FALSE
  )

  # 1. Test with string column - NA and empty strings should be replaced
  result1 <- clean_column_data(test_data, "string_col")
  expect_type(result1, "character")
  expect_equal(result1, c("Value", "Unspecified", "Unspecified", "  ", "Normal"))

  # 2. Test with numeric column - NA should be replaced, numbers converted to character
  result2 <- clean_column_data(test_data, "num_col")
  expect_type(result2, "character")
  expect_equal(result2, c("1", "Unspecified", "3", "4", "5"))

  # 3. Test with factor column - NA should be replaced, factor converted to character
  result3 <- clean_column_data(test_data, "factor_col")
  expect_type(result3, "character")
  expect_equal(result3, c("A", "Unspecified", "C", "D", "E"))

  # 4. Test with logical column - NA should be replaced, logical converted to character
  result4 <- clean_column_data(test_data, "logical_col")
  expect_type(result4, "character")
  expect_equal(result4, c("TRUE", "FALSE", "Unspecified", "TRUE", "FALSE"))

  # 5. Test with date column - NA should be replaced, dates converted to character
  result5 <- clean_column_data(test_data, "date_col")
  expect_type(result5, "character")
  expect_equal(result5, c("2023-01-01", "Unspecified", "2023-03-01", "2023-04-01", "2023-05-01"))

  # 6. Test with non-existent column - should return vector of default values
  missing_col_result <- clean_column_data(test_data, "nonexistent_col")
  expect_type(missing_col_result, "character")
  expect_equal(missing_col_result, rep("Unspecified", nrow(test_data)))

  # 7. Test with custom default value
  custom_result <- clean_column_data(test_data, "string_col", "MISSING")
  expect_equal(custom_result, c("Value", "MISSING", "MISSING", "  ", "Normal"))

  # 8. Test with empty data frame
  empty_df <- data.frame(x = character(0))
  empty_result <- clean_column_data(empty_df, "x")
  expect_equal(length(empty_result), 0)
  expect_type(empty_result, "character")

  # 9. Test with data frame that has a column but all values are NA
  all_na_df <- data.frame(all_na = c(NA, NA, NA), stringsAsFactors = FALSE)
  all_na_result <- clean_column_data(all_na_df, "all_na")
  expect_equal(all_na_result, c("Unspecified", "Unspecified", "Unspecified"))

  # 10. Test with data frame that has a column but all values are empty strings
  all_empty_df <- data.frame(all_empty = c("", "", ""), stringsAsFactors = FALSE)
  all_empty_result <- clean_column_data(all_empty_df, "all_empty")
  expect_equal(all_empty_result, c("Unspecified", "Unspecified", "Unspecified"))

  # 11. Test that whitespace is preserved
  whitespace_df <- data.frame(
    ws = c(" leading", "trailing ", "  both  ", "none"),
    stringsAsFactors = FALSE
  )
  whitespace_result <- clean_column_data(whitespace_df, "ws")
  expect_equal(whitespace_result, c(" leading", "Trailing ", "  both  ", "None"))

  # 12. Test with a single row data frame
  single_row <- data.frame(x = NA, y = "value", stringsAsFactors = FALSE)
  expect_equal(clean_column_data(single_row, "x"), "Unspecified")
  expect_equal(clean_column_data(single_row, "y"), "Value")
  expect_equal(clean_column_data(single_row, "z"), "Unspecified")
})

test_that("create_table wires DataTables state load from URL", {
  skip_if_not_installed("DT")
  table_env <- load_table_module_env()
  create_table_fn <- table_env$create_table

  widget <- create_table_fn(table_config = list(initial_data = data.frame(a = 1)))
  expect_s3_class(widget, "datatables")

  options <- widget$x$options
  expect_true(isTRUE(options$stateSave))
  expect_identical(options$stateDuration, 0)

  load_cb <- options$stateLoadCallback
  expect_s3_class(load_cb, "JS_EVAL")
  expect_match(as.character(load_cb), "window\\.vegbankLoadTableState")
})

test_that("create_table registers DT state save callback for handshake", {
  skip_if_not_installed("DT")
  table_env <- load_table_module_env()
  create_table_fn <- table_env$create_table

  widget <- create_table_fn(table_config = list(initial_data = data.frame(a = 1)))
  options <- widget$x$options

  save_cb <- options$stateSaveCallback
  expect_s3_class(save_cb, "JS_EVAL")
  expect_match(as.character(save_cb), "window\\.vegbankSaveTableState")
})

# Factory function tests
test_that("create_coercer produces working coercion functions", {
  table_env <- load_table_module_env()
  create_coercer <- table_env$create_coercer
  coerce_api_response <- table_env$coerce_api_response

  schema <- data.frame(a = integer(), b = character(), stringsAsFactors = FALSE)
  coercer <- create_coercer(schema)

  # Test NULL handling
  expect_equal(coercer(NULL), schema)

  # Test data frame pass-through
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  expect_equal(coercer(df), df)

  # Test nested list unwrapping
  nested <- list(data = df)
  expect_equal(coercer(nested), df)

  # Test single-element list unwrapping
  wrapped <- list(df)
  expect_s3_class(coercer(wrapped), "data.frame")
})

test_that("create_normalizer produces working normalization functions", {
  table_env <- load_table_module_env()
  create_normalizer <- table_env$create_normalizer

  schema <- data.frame(
    id = integer(),
    name = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )

  # Test basic normalization
  normalizer <- create_normalizer(schema)
  raw <- data.frame(id = 1, name = "test", count = 5, stringsAsFactors = FALSE)
  result <- normalizer(raw)

  expect_equal(names(result), c("id", "name", "count"))
  expect_type(result$id, "integer")
  expect_type(result$name, "character")

  # Test NA-to-zero conversion
  normalizer_with_zero <- create_normalizer(schema, na_to_zero_fields = "count")
  raw_with_na <- data.frame(id = 1, name = "test", count = NA_integer_, stringsAsFactors = FALSE)
  result_zero <- normalizer_with_zero(raw_with_na)

  expect_equal(result_zero$count, 0L)

  # Test custom transforms
  add_uppercase <- function(df) {
    df$name <- toupper(df$name)
    df
  }
  normalizer_custom <- create_normalizer(schema, custom_transforms = list(add_uppercase))
  result_custom <- normalizer_custom(raw)

  expect_equal(result_custom$name, "TEST")
})

test_that("create_normalizer handles multiple NA-to-zero fields", {
  table_env <- load_table_module_env()
  create_normalizer <- table_env$create_normalizer

  schema <- data.frame(
    field1 = integer(),
    field2 = integer(),
    field3 = character(),
    stringsAsFactors = FALSE
  )

  normalizer <- create_normalizer(schema, na_to_zero_fields = c("field1", "field2"))
  raw <- data.frame(
    field1 = NA_integer_,
    field2 = NA_integer_,
    field3 = "test",
    stringsAsFactors = FALSE
  )

  result <- normalizer(raw)
  expect_equal(result$field1, 0L)
  expect_equal(result$field2, 0L)
  expect_equal(result$field3, "test")
})

test_that("create_normalizer preserves existing non-NA values", {
  table_env <- load_table_module_env()
  create_normalizer <- table_env$create_normalizer

  schema <- data.frame(count = integer(), stringsAsFactors = FALSE)
  normalizer <- create_normalizer(schema, na_to_zero_fields = "count")

  raw <- data.frame(count = 42L, stringsAsFactors = FALSE)
  result <- normalizer(raw)

  expect_equal(result$count, 42L)
})
