# Tests for cover method detail view
# Real mocks (mock_cover_method_cm79/cm1630/cm1) are loaded from mocks/cover_methods.R

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
  # cm.1: has rf.27/CVS Protocol, no estimation method, 3 cover indexes
  details <- build_cover_method_details_view(mock_cover_method_cm1)

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
  expect_true(grepl("Carolina Vegetation Survey", header_html))
  expect_true(grepl("cm.1", header_html))

  # Test details: no estimation method → Unspecified; has rf link
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("Unspecified", details_html))   # cover_estimation_method is NA
  expect_true(grepl("CVS Protocol", details_html))
  expect_true(grepl("ref_link_click", details_html))
  expect_true(grepl("rf.27", details_html))

  # Test indexes table structure
  indexes_html <- htmltools::renderTags(details$cover_method_indexes(shinysession = mock_session))$html
  expect_true(grepl("Cover Code", indexes_html))
  expect_true(grepl("Lower Limit %", indexes_html))
  expect_true(grepl("Upper Limit %", indexes_html))
  expect_true(grepl("Cover %", indexes_html))
  expect_true(grepl("Index Desc", indexes_html))
  expect_true(grepl("toggle_cover_index_description", indexes_html))
  expect_true(grepl("Show Index Description Column", indexes_html))
})

test_that("build_cover_method_details_view formats cm.1630 (Domin, estimation method, UNKNOWN ref)", {
  details <- build_cover_method_details_view(mock_cover_method_cm1630)
  mock_session <- shiny::MockShinySession$new()

  header_html <- htmltools::renderTags(details$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("Domin Cover Scale", header_html))
  expect_true(grepl("cm.1630", header_html))

  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("canopy cover", details_html))
  expect_true(grepl("UNKNOWN", details_html))
  expect_true(grepl("ref_link_click", details_html))
  expect_true(grepl("rf.35243", details_html))

  indexes_html <- htmltools::renderTags(details$cover_method_indexes(shinysession = mock_session))$html
  expect_true(grepl("trace", indexes_html))
  expect_true(grepl("0-1 percent", indexes_html))
})

test_that("build_cover_method_details_view formats cm.79 (NPS, no ref, no estimation method)", {
  details <- build_cover_method_details_view(mock_cover_method_cm79)
  mock_session <- shiny::MockShinySession$new()

  header_html <- htmltools::renderTags(details$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("NPS CoverMethod", header_html))
  expect_true(grepl("cm.79", header_html))

  # No rf_code and no estimation method → both Unspecified
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("Unspecified", details_html))
  expect_false(grepl("ref_link_click", details_html))

  indexes_html <- htmltools::renderTags(details$cover_method_indexes(shinysession = mock_session))$html
  expect_true(grepl("01", indexes_html))  # cover_code
  expect_true(grepl("Cover Code", indexes_html))
})

test_that("build_cover_method_details_view handles cover method without reference", {
  details <- build_cover_method_details_view(mock_cover_method_no_indexes)

  mock_session <- shiny::MockShinySession$new()

  # Test header
  header_html <- htmltools::renderTags(details$cover_method_header(shinysession = mock_session))$html
  expect_true(grepl("NC Botanical Garden", header_html))
  expect_true(grepl("cm.1631", header_html))

  # Test details (should show "Unspecified" for reference)
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("Unspecified", details_html))
  expect_true(grepl("Unspecified", details_html)) # cover_estimation_method is NA

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
  expect_true(grepl("Unnamed Cover Method", header_html))
  expect_true(grepl("cm.999", header_html))

  # Test details with all missing fields
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html
  expect_true(grepl("Unspecified", details_html))
  expect_true(grepl("Unspecified", details_html))
})

test_that("build_cover_method_details_view creates clickable reference link", {
  # cm.1 has rf.27/CVS Protocol
  details <- build_cover_method_details_view(mock_cover_method_cm1)

  mock_session <- shiny::MockShinySession$new()
  details_html <- htmltools::renderTags(details$cover_method_details(shinysession = mock_session))$html

  expect_true(grepl("Shiny.setInputValue", details_html, fixed = TRUE))
  expect_true(grepl("ref_link_click", details_html, fixed = TRUE))
  expect_true(grepl("rf.27", details_html))
  expect_true(grepl("CVS Protocol", details_html))
  expect_true(grepl('href="#"', details_html, fixed = TRUE))
})

test_that("cover indexes table has correct column visibility toggle", {
  details <- build_cover_method_details_view(mock_cover_method_cm1)

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
