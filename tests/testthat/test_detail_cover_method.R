# Tests for cover method detail view

# Create mock cover method data
mock_cover_method_with_indexes <- data.frame(
  cm_code = "cm.471",
  cover_type = "Braun-blanquet (MN)",
  cover_estimation_method = "canopy cover",
  rf_code = "rf.28",
  rf_label = "Braun-Blanquet",
  stringsAsFactors = FALSE
)

# Add nested cover_indexes as a list column
mock_cover_method_with_indexes$cover_indexes <- list(data.frame(
  cover_code = c("1", "2", "3"),
  lower_limit = c("0", "5", "25"),
  upper_limit = c("5", "25", "50"),
  cover_percent = c("2.5", "15", "37.5"),
  index_description = c("Rare", "Occasional", "Common"),
  stringsAsFactors = FALSE
))

mock_cover_method_no_indexes <- data.frame(
  cm_code = "cm.1631",
  cover_type = "NC Botanical Garden",
  cover_estimation_method = NA,
  rf_code = NA,
  rf_label = NA,
  stringsAsFactors = FALSE
)

mock_cover_method_no_indexes$cover_indexes <- list(data.frame())

test_that("build_cover_method_details_view handles NULL data gracefully", {
  result <- build_cover_method_details_view(NULL)

  expect_type(result, "list")
  expect_named(result, c(
    "cover_method_header",
    "cover_method_details",
    "cover_method_indexes"
  ))

  expect_s3_class(result$cover_method_header, "shiny.render.function")
  mock_session <- shiny::MockShinySession$new()
  html <- htmltools::renderTags(result$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("Cover method details not available", html))
})

test_that("build_cover_method_details_view handles empty dataframe", {
  result <- build_cover_method_details_view(data.frame())

  expect_type(result, "list")
  expect_named(result, c(
    "cover_method_header",
    "cover_method_details",
    "cover_method_indexes"
  ))

  mock_session <- shiny::MockShinySession$new()
  html <- htmltools::renderTags(result$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("Cover method details not available", html))
})

test_that("build_cover_method_details_view formats cover method with indexes and reference", {
  details <- build_cover_method_details_view(mock_cover_method_with_indexes)

  expect_type(details, "list")
  expect_named(details, c(
    "cover_method_header",
    "cover_method_details",
    "cover_method_indexes"
  ))

  expect_s3_class(details$cover_method_header, "shiny.render.function")
  expect_s3_class(details$cover_method_details, "shiny.render.function")
  expect_s3_class(details$cover_method_indexes, "shiny.render.function")

  mock_session <- shiny::MockShinySession$new()

  # Test header
  header_html <- htmltools::renderTags(details$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("Braun-blanquet \\(MN\\)", header_html))
  expect_true(grepl("cm.471", header_html))

  # Test details (should have cover estimation method and reference link)
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("canopy cover", details_html))
  expect_true(grepl("Braun-Blanquet", details_html))
  expect_true(grepl("ref_link_click", details_html))
  expect_true(grepl("rf.28", details_html))

  # Test indexes table
  indexes_html <- htmltools::renderTags(details$cover_method_indexes(shinysession = mock_session))$html
  expect_true(grepl("Cover Code", indexes_html))
  expect_true(grepl("Lower Limit %", indexes_html))
  expect_true(grepl("Upper Limit %", indexes_html))
  expect_true(grepl("Cover %", indexes_html))
  expect_true(grepl("Index Desc", indexes_html))
  expect_true(grepl("Rare", indexes_html))
  expect_true(grepl("Occasional", indexes_html))
  expect_true(grepl("Common", indexes_html))

  # Test toggle checkbox
  expect_true(grepl("toggle_cover_index_description", indexes_html))
  expect_true(grepl("Show Index Description Column", indexes_html))
})

test_that("build_cover_method_details_view handles cover method without reference", {
  details <- build_cover_method_details_view(mock_cover_method_no_indexes)

  mock_session <- shiny::MockShinySession$new()

  # Test header
  header_html <- htmltools::renderTags(details$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("NC Botanical Garden", header_html))
  expect_true(grepl("cm.1631", header_html))

  # Test details (should show "Not provided" for reference)
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("Not provided", details_html))
  expect_true(grepl("Not recorded", details_html)) # cover_estimation_method is NA

  # Test indexes (should show no indexes message)
  indexes_html <- htmltools::renderTags(details$cover_method_indexes(shinysession = mock_session))$html
  expect_true(grepl("No cover indexes recorded", indexes_html))
})

test_that("build_cover_method_details_view handles missing fields gracefully", {
  minimal_data <- data.frame(
    cm_code = "cm.999",
    cover_type = NA,
    cover_estimation_method = NA,
    rf_code = NA,
    rf_label = NA,
    stringsAsFactors = FALSE
  )
  minimal_data$cover_indexes <- list(data.frame())

  details <- build_cover_method_details_view(minimal_data)

  mock_session <- shiny::MockShinySession$new()

  # Test header with missing cover_type
  header_html <- htmltools::renderTags(details$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("Not recorded", header_html))
  expect_true(grepl("cm.999", header_html))

  # Test details with all missing fields
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("Not recorded", details_html))
  expect_true(grepl("Not provided", details_html))
})

test_that("build_cover_method_details_view creates clickable reference link", {
  details <- build_cover_method_details_view(mock_cover_method_with_indexes)

  mock_session <- shiny::MockShinySession$new()
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html

  # Verify the reference link has the correct structure (uses onclick with Shiny.setInputValue)
  expect_true(grepl("Shiny.setInputValue", details_html, fixed = TRUE))
  expect_true(grepl("ref_link_click", details_html, fixed = TRUE))
  expect_true(grepl("rf.28", details_html))
  expect_true(grepl("Braun-Blanquet", details_html))
  expect_true(grepl('href="#"', details_html, fixed = TRUE))
})

test_that("cover indexes table has correct column visibility toggle", {
  details <- build_cover_method_details_view(mock_cover_method_with_indexes)

  mock_session <- shiny::MockShinySession$new()
  indexes_html <- htmltools::renderTags(details$cover_method_indexes(shinysession = mock_session))$html

  # Check for toggle checkbox
  expect_true(grepl('<input type="checkbox"', indexes_html))
  expect_true(grepl('id="toggle_cover_index_description"', indexes_html))

  # Check that description column and header are hidden by default
  expect_true(grepl('class="cover-index-description-header"', indexes_html))
  expect_true(grepl('style="display: none;"', indexes_html))
  expect_true(grepl('class="cover-index-description"', indexes_html))
})
