# Tests for detail view helper functions

test_that("coalesce function (%|||%) works correctly", {
  # Test with various input types
  expect_equal(NULL %|||% "default", "default")
  expect_equal(NA %|||% "default", "default")
  expect_equal("" %|||% "default", "default")  # Empty string now returns default
  expect_equal("NA" %|||% "default", "NA")
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

test_that("render_detail_table handles valid and invalid fields gracefully", {
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
      valid_result <- render_detail_table(c("field1", "field2"), test_data)
      expect_s3_class(valid_result, "shiny.render.function")

      # Test with invalid fields
      invalid_result <- render_detail_table(c("non_existent_field"), test_data)
      expect_s3_class(invalid_result, "shiny.render.function")

      # Test with mixed valid and invalid fields
      mixed_result <- render_detail_table(c("field1", "non_existent_field"), test_data)
      expect_s3_class(mixed_result, "shiny.render.function")

      # Test with NA values
      na_result <- render_detail_table(c("field3"), test_data)
      expect_s3_class(na_result, "shiny.render.function")
    }
  )
})

# ---- XSS Prevention tests ----

test_that("create_detail_link escapes code_value to prevent XSS", {
  # Test JS injection in code_value
  xss_code <- "'); alert('xss'); //"
  link <- create_detail_link("test_click", xss_code, "Display Text")
  link_html <- as.character(link)

  # Should NOT contain unescaped single quote that could break JS string
  # The raw pattern "'); would allow JS injection
  expect_false(grepl("'\\);", link_html, fixed = TRUE))
  # htmltools::htmlEscape converts quotes to HTML entities in the onclick attribute
  # Single quotes become &#39; which is safe in HTML attributes
  expect_true(grepl("&#39;", link_html))
})

test_that("create_detail_link escapes display_text to prevent XSS", {
  # Test HTML injection in display_text
  xss_display <- "<script>alert('xss')</script>"
  link <- create_detail_link("test_click", "code123", xss_display)
  link_html <- as.character(link)

  # Should NOT contain raw script tag
  expect_false(grepl("<script>", link_html, fixed = TRUE))
  # Should contain escaped version
  expect_true(grepl("&lt;script&gt;", link_html, fixed = TRUE))
})

test_that("create_detail_link handles special characters safely", {
  # Test various edge cases
  special_code <- "ob.123/456"
  special_display <- "Plot's \"Name\" <test> & more"

  link <- create_detail_link("test_click", special_code, special_display)
  link_html <- as.character(link)

  # Code value in onclick will be HTML escaped - forward slashes are safe and not escaped
  expect_true(grepl("ob.123/456", link_html, fixed = TRUE))

  # Display text should be HTML escaped for text content
  # Note: htmlEscape() without attribute=TRUE doesn't escape quotes/apostrophes in text nodes (they're safe there)
  expect_true(grepl("&lt;test&gt;", link_html)) # escaped angle brackets
  expect_true(grepl("&amp;", link_html)) # escaped ampersand
})
