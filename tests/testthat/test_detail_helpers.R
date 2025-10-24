# Tests for detail view helper functions

test_that("coalesce function (%|||%) works correctly", {
  # Test with various input types
  expect_equal(NULL %|||% "default", "default")
  expect_equal(NA %|||% "default", "default")
  expect_equal("" %|||% "default", "default")
  expect_equal("NA" %|||% "default", "default")
  expect_equal("valid" %|||% "default", "valid")
  expect_equal(0 %|||% "default", 0)
  expect_equal(FALSE %|||% "default", FALSE)

  # Test with vectors
  expect_equal(c(NA, "valid") %|||% "default", "default") # Takes first element
  expect_equal(c("valid", "other") %|||% "default", "valid") # Takes first element
})

test_that("create_detail_table correctly formats data into HTML table", {
  # Create test data
  test_details <- list(
    field1 = "Value 1",
    field2 = "Value 2",
    field3 = "Value 3"
  )

  display_names <- c(field1 = "Display Name 1", field2 = "Display Name 2")

  # Test function
  result <- create_detail_table(test_details, display_names)

  # Verify structure
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "table")
  expect_equal(result$attribs$class, "table table-sm table-striped table-hover")

  # Convert to character to check content
  result_text <- as.character(result)

  # Check for display names and values
  expect_true(grepl("Display Name 1", result_text))
  expect_true(grepl("Display Name 2", result_text))
  expect_true(grepl("field3", result_text)) # This should use the original name
  expect_true(grepl("Value 1", result_text))
  expect_true(grepl("Value 2", result_text))
  expect_true(grepl("Value 3", result_text))
})

test_that("safe_render_details handles valid and invalid fields gracefully", {
  # Create test data with some fields
  test_data <- data.frame(
    field1 = "Value 1",
    field2 = "Value 2",
    field3 = NA,
    stringsAsFactors = FALSE
  )

  # Mock get_field_display_names
  with_mocked_bindings(
    get_field_display_names = function() {
      c(field1 = "Display Name 1", field2 = "Display Name 2", field3 = "Display Name 3")
    },
    {
      # Test with valid fields
      valid_result <- safe_render_details(c("field1", "field2"), test_data)
      expect_s3_class(valid_result, "shiny.render.function")

      # Test with invalid fields
      invalid_result <- safe_render_details(c("non_existent_field"), test_data)
      expect_s3_class(invalid_result, "shiny.render.function")

      # Test with mixed valid and invalid fields
      mixed_result <- safe_render_details(c("field1", "non_existent_field"), test_data)
      expect_s3_class(mixed_result, "shiny.render.function")

      # Test with NA values
      na_result <- safe_render_details(c("field3"), test_data)
      expect_s3_class(na_result, "shiny.render.function")
    }
  )
})
