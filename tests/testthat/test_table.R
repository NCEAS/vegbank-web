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

test_that("create_table returns empty widget when required data missing", {
  skip_if_not_installed("DT")

  table_env <- load_table_module_env()
  create_table_fn <- table_env$create_table

  result <- with_mock_shiny_notifications({
    create_table_fn(
      data_sources = list(existing = data.frame()),
      required_sources = c("existing", "missing"),
      process_function = function(x) x
    )
  })

  expect_s3_class(result, "datatables")
  expect_match(result$x$data[[1]], "Please try again")
})

test_that("create_table processes data correctly when all sources available", {
  skip_if_not_installed("DT")

  table_env <- load_table_module_env()
  create_table_fn <- table_env$create_table

  test_df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)

  widget <- create_table_fn(
    data_sources = list(test_data = test_df),
    required_sources = "test_data",
    process_function = function(sources) sources$test_data
  )

  expect_s3_class(widget, "datatables")
  expect_equal(widget$x$data, test_df)
})

test_that("create_action_buttons generates HTML buttons correctly", {
  # Test data
  test_data <- data.frame(
    id = c("id1", "id2"),
    name = c("name1", "name2"),
    stringsAsFactors = FALSE
  )

  # Actions configuration
  actions <- list(
    list(input_id = "view_click", input_value = "id", label = "View", class = "btn-primary"),
    list(input_id = "edit_click", input_value = "id", label = "Edit", class = "btn-secondary")
  )

  # Test the function
  result <- create_action_buttons(test_data, actions)

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
  expect_equal(result1, c("Value", "Not provided", "Not provided", "  ", "Normal"))

  # 2. Test with numeric column - NA should be replaced, numbers converted to character
  result2 <- clean_column_data(test_data, "num_col")
  expect_type(result2, "character")
  expect_equal(result2, c("1", "Not provided", "3", "4", "5"))

  # 3. Test with factor column - NA should be replaced, factor converted to character
  result3 <- clean_column_data(test_data, "factor_col")
  expect_type(result3, "character")
  expect_equal(result3, c("A", "Not provided", "C", "D", "E"))

  # 4. Test with logical column - NA should be replaced, logical converted to character
  result4 <- clean_column_data(test_data, "logical_col")
  expect_type(result4, "character")
  expect_equal(result4, c("TRUE", "FALSE", "Not provided", "TRUE", "FALSE"))

  # 5. Test with date column - NA should be replaced, dates converted to character
  result5 <- clean_column_data(test_data, "date_col")
  expect_type(result5, "character")
  expect_equal(result5, c("2023-01-01", "Not provided", "2023-03-01", "2023-04-01", "2023-05-01"))

  # 6. Test with non-existent column - should return vector of default values
  missing_col_result <- clean_column_data(test_data, "nonexistent_col")
  expect_type(missing_col_result, "character")
  expect_equal(missing_col_result, rep("Not provided", nrow(test_data)))

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
  expect_equal(all_na_result, c("Not provided", "Not provided", "Not provided"))

  # 10. Test with data frame that has a column but all values are empty strings
  all_empty_df <- data.frame(all_empty = c("", "", ""), stringsAsFactors = FALSE)
  all_empty_result <- clean_column_data(all_empty_df, "all_empty")
  expect_equal(all_empty_result, c("Not provided", "Not provided", "Not provided"))

  # 11. Test that whitespace is preserved
  whitespace_df <- data.frame(
    ws = c(" leading", "trailing ", "  both  ", "none"),
    stringsAsFactors = FALSE
  )
  whitespace_result <- clean_column_data(whitespace_df, "ws")
  expect_equal(whitespace_result, c(" leading", "Trailing ", "  both  ", "None"))

  # 12. Test with a single row data frame
  single_row <- data.frame(x = NA, y = "value", stringsAsFactors = FALSE)
  expect_equal(clean_column_data(single_row, "x"), "Not provided")
  expect_equal(clean_column_data(single_row, "y"), "Value")
  expect_equal(clean_column_data(single_row, "z"), "Not provided")
})

test_that("create_table wires DataTables state load from URL", {
  skip_if_not_installed("DT")
  table_env <- load_table_module_env()
  create_table_fn <- table_env$create_table

  data_sources <- list(test = data.frame(a = 1))
  process_fn <- function(sources) sources$test

  widget <- create_table_fn(data_sources, required_sources = NULL, process_function = process_fn)
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

  data_sources <- list(test = data.frame(a = 1))
  process_fn <- function(sources) sources$test

  widget <- create_table_fn(data_sources, required_sources = NULL, process_function = process_fn)
  options <- widget$x$options

  save_cb <- options$stateSaveCallback
  expect_s3_class(save_cb, "JS_EVAL")
  expect_match(as.character(save_cb), "window\\.vegbankSaveTableState")
})
