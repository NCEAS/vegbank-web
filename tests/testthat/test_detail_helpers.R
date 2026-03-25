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

# ---- sanitize_description_html tests ----

test_that("sanitize_description_html preserves allowed inline tags without attributes", {
  expect_equal(sanitize_description_html("plain text"), "plain text")
  expect_equal(sanitize_description_html("a <i>italic</i> word"), "a <i>italic</i> word")
  expect_equal(sanitize_description_html("a <b>bold</b> word"), "a <b>bold</b> word")
  expect_equal(sanitize_description_html("<em>emphasis</em>"), "<em>emphasis</em>")
  expect_equal(sanitize_description_html("<strong>strong</strong>"), "<strong>strong</strong>")
  expect_equal(sanitize_description_html("line one<br>line two"), "line one<br>line two")
  expect_equal(sanitize_description_html("line one<br/>line two"), "line one<br/>line two")
  # Mixed allowlist tags
  expect_equal(
    sanitize_description_html("<b>bold</b> and <i>italic</i>"),
    "<b>bold</b> and <i>italic</i>"
  )
})

test_that("sanitize_description_html preserves allowed tags case-insensitively", {
  expect_equal(sanitize_description_html("<I>italic</I>"), "<I>italic</I>")
  expect_equal(sanitize_description_html("<B>bold</B>"), "<B>bold</B>")
  expect_equal(sanitize_description_html("<EM>em</EM>"), "<EM>em</EM>")
  expect_equal(sanitize_description_html("<STRONG>strong</STRONG>"), "<STRONG>strong</STRONG>")
  expect_equal(sanitize_description_html("<BR>"), "<BR>")
})

test_that("sanitize_description_html escapes disallowed tags", {
  # <script> must never appear in output
  result <- sanitize_description_html("<script>alert('xss')</script>")
  expect_false(grepl("<script>", result, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", result, fixed = TRUE))

  # <img> is not on the allowlist
  result <- sanitize_description_html('<img src="x" onerror="alert(1)">')
  expect_false(grepl("<img", result, fixed = TRUE))
  expect_true(grepl("&lt;img", result, fixed = TRUE))

  # <a> is not on the allowlist
  result <- sanitize_description_html('<a href="http://evil.example">click</a>')
  expect_false(grepl("<a ", result, fixed = TRUE))
  expect_true(grepl("&lt;a ", result, fixed = TRUE))

  # <div> is not on the allowlist
  result <- sanitize_description_html("<div>block</div>")
  expect_false(grepl("<div>", result, fixed = TRUE))
  expect_true(grepl("&lt;div&gt;", result, fixed = TRUE))
})

test_that("sanitize_description_html does not restore allowed tags that carry attributes", {
  # <b> with an event handler must stay escaped
  result <- sanitize_description_html('<b onclick="alert(1)">bold</b>')
  expect_false(grepl('<b onclick', result, fixed = TRUE))
  expect_true(grepl("&lt;b onclick", result, fixed = TRUE))

  # <i> with a style attribute must stay escaped
  result <- sanitize_description_html('<i style="color:red">italic</i>')
  expect_false(grepl('<i style', result, fixed = TRUE))
  expect_true(grepl("&lt;i style", result, fixed = TRUE))

  # <em> with any attribute must stay escaped
  result <- sanitize_description_html('<em class="x">em</em>')
  expect_false(grepl('<em class', result, fixed = TRUE))
  expect_true(grepl("&lt;em class", result, fixed = TRUE))

  # <br> with an attribute must stay escaped
  result <- sanitize_description_html('<br id="x">')
  expect_false(grepl('<br id', result, fixed = TRUE))
  expect_true(grepl("&lt;br id", result, fixed = TRUE))
})

test_that("sanitize_description_html handles edge cases gracefully", {
  expect_equal(sanitize_description_html(NULL), NULL)
  expect_true(is.na(sanitize_description_html(NA)))
  expect_equal(sanitize_description_html(""), "")
  expect_equal(sanitize_description_html("   "), "   ")
  # Ampersands and quotes in plain text are still escaped
  result <- sanitize_description_html("cats & dogs")
  expect_true(grepl("&amp;", result, fixed = TRUE))
})

test_that("append_units renders slope_aspect sentinel values without degree symbol", {
  expect_equal(append_units("slope_aspect", -1),   "-1 (Too flat)")
  expect_equal(append_units("slope_aspect", -2),   "-2 (Too irregular)")
  expect_equal(append_units("slope_aspect", "-1"),  "-1 (Too flat)")
  expect_equal(append_units("slope_aspect", "-2"),  "-2 (Too irregular)")
})

test_that("append_units appends degree symbol to normal slope_aspect values", {
  expect_equal(append_units("slope_aspect", 180),  "180\u00B0")
  expect_equal(append_units("slope_aspect", 0),    "0\u00B0")
  expect_equal(append_units("slope_aspect", 360),  "360\u00B0")
})

test_that("append_units sentinel check does not affect min_slope_aspect or max_slope_aspect", {
  expect_equal(append_units("min_slope_aspect", -1),  "-1\u00B0")
  expect_equal(append_units("max_slope_aspect", -2),  "-2\u00B0")
})

test_that("append_units skips sentinel check for NA or Unspecified slope_aspect", {
  expect_equal(append_units("slope_aspect", NA),           NA)
  expect_equal(append_units("slope_aspect", "Unspecified"), "Unspecified")
})

test_that("append_units renders slope_gradient sentinel values without degree symbol", {
  expect_equal(append_units("slope_gradient", -1),   "-1 (Too irregular)")
  expect_equal(append_units("slope_gradient", "-1"), "-1 (Too irregular)")
})

test_that("append_units treats non-sentinel negative slope_gradient values as degree values", {
  expect_equal(append_units("slope_gradient", -2),   "-2\u00B0")
  expect_equal(append_units("slope_gradient", "-2"), "-2\u00B0")
})

test_that("append_units appends degree symbol to normal slope_gradient values", {
  expect_equal(append_units("slope_gradient", 0),   "0\u00B0")
  expect_equal(append_units("slope_gradient", 15),  "15\u00B0")
  expect_equal(append_units("slope_gradient", 90),  "90\u00B0")
})

test_that("append_units sentinel check does not affect min/max slope_gradient", {
  expect_equal(append_units("min_slope_gradient", -1),  "-1\u00B0")
  expect_equal(append_units("max_slope_gradient", -2),  "-2\u00B0")
})

test_that("append_units renders area sentinel values without square meter units", {
  expect_equal(append_units("area", -1),                    "-1 (No known boundaries)")
  expect_equal(append_units("taxon_observation_area", -1),  "-1 (No known boundaries)")
  expect_equal(append_units("taxon_inference_area", -1),    "-1 (No known boundaries)")
  expect_equal(append_units("stem_observation_area", -1),   "-1 (No known boundaries)")
  expect_equal(append_units("inference_area", -1),          "-1 (No known boundaries)")
  expect_equal(append_units("stem_taxon_area", -1),         "-1 (No known boundaries)")
})

test_that("append_units keeps normal units for area fields when not sentinel", {
  expect_equal(append_units("area", 12),                   "12 m\u00B2")
  expect_equal(append_units("taxon_observation_area", 8),  "8 m\u00B2")
  expect_equal(append_units("taxon_inference_area", 3),    "3 m\u00B2")
  expect_equal(append_units("stem_observation_area", 5),   "5 m\u00B2")
  expect_equal(append_units("inference_area", 4),          "4 m\u00B2")
})

test_that("append_units returns raw value for stem_taxon_area when not sentinel", {
  expect_equal(append_units("stem_taxon_area", 9), 9)
})

test_that("create_copy_permalink_button builds a copy control with cite URL", {
  tag <- create_citation_button("ob.2948")
  expect_s3_class(tag, "shiny.tag")
  html <- as.character(tag)

  expect_true(grepl("vb-copy-permalink", html, fixed = TRUE))
  expect_true(grepl("Copy citation", html, fixed = TRUE))
  expect_false(grepl("Copy citation link", html, fixed = TRUE))
  expect_true(grepl("vegbank.org/cite/ob.2948", html, fixed = TRUE))
})

test_that("create_copy_permalink_button returns NULL for missing vb_code", {
  expect_null(create_citation_button(NULL))
  expect_null(create_citation_button(NA_character_))
  expect_null(create_citation_button(""))
  expect_null(create_citation_button("   "))
})

test_that("attach_copy_button_to_last_header_row appends copy control to final row", {
  rows <- list(
    htmltools::tags$h5("Header 1"),
    htmltools::tags$p("Header 2")
  )

  out <- add_citation_button_to_last_row(rows, "ob.2948")
  expect_length(out, 2)

  html <- as.character(out[[2]])
  expect_true(grepl("vb-copy-inline-row", html, fixed = TRUE))
  expect_true(grepl("Header 2", html, fixed = TRUE))
  expect_true(grepl("vb-copy-permalink", html, fixed = TRUE))
})

test_that("attach_copy_button_to_last_header_row ignores NULL rows", {
  rows <- list(
    htmltools::tags$h5("Header 1"),
    NULL,
    htmltools::tags$p("Header 3")
  )

  out <- add_citation_button_to_last_row(rows, "ob.2948")
  expect_length(out, 2)

  html <- as.character(out[[2]])
  expect_true(grepl("Header 3", html, fixed = TRUE))
  expect_true(grepl("vb-copy-permalink", html, fixed = TRUE))
})
