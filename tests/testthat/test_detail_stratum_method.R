# Tests for stratum method detail view
# Real mocks (mock_stratum_method_sm1/sm5/sm329) are loaded from mocks/stratum_methods.R

mock_stratum_method_no_types <- data.frame(
  sm_code = "sm.999",
  stratum_method_name = "Test Method",
  stratum_method_description = NA,
  stratum_assignment = NA,
  rf_code = NA,
  rf_label = NA,
  stringsAsFactors = FALSE
)

mock_stratum_method_no_types$stratum_types <- list(data.frame())

test_that("build_stratum_method_details_view handles NULL data gracefully", {
  result <- build_stratum_method_details_view(NULL)

  expect_type(result, "list")
  expect_named(result, c(
    "stratum_method_header",
    "stratum_method_details",
    "stratum_types"
  ))

  expect_s3_class(result$stratum_method_header, "shiny.render.function")
  mock_session <- shiny::MockShinySession$new()
  html <- htmltools::renderTags(result$stratum_method_header(shinysession = mock_session))$html
  expect_true(grepl("Stratum method details not available", html))
})

test_that("build_stratum_method_details_view handles empty dataframe", {
  result <- build_stratum_method_details_view(data.frame())

  expect_type(result, "list")
  expect_named(result, c(
    "stratum_method_header",
    "stratum_method_details",
    "stratum_types"
  ))

  mock_session <- shiny::MockShinySession$new()
  html <- htmltools::renderTags(result$stratum_method_header(shinysession = mock_session))$html
  expect_true(grepl("Stratum method details not available", html))
})

test_that("build_stratum_method_details_view formats stratum method with types and reference", {
  details <- build_stratum_method_details_view(mock_stratum_method_sm1)

  expect_type(details, "list")
  expect_named(details, c(
    "stratum_method_header",
    "stratum_method_details",
    "stratum_types"
  ))

  expect_s3_class(details$stratum_method_header, "shiny.render.function")
  expect_s3_class(details$stratum_method_details, "shiny.render.function")
  expect_s3_class(details$stratum_types, "shiny.render.function")

  mock_session <- shiny::MockShinySession$new()

  # Test header
  header_html <- htmltools::renderTags(details$stratum_method_header(shinysession = mock_session))$html
  expect_true(grepl("Carolina Vegetation Survey", header_html))
  expect_true(grepl("sm.1", header_html))

  # Test details (should have description and reference link)
  details_html <- htmltools::renderTags(details$stratum_method_details(shinysession = mock_session))$html
  expect_true(grepl("Stratum heights are constructed", details_html))
  expect_true(grepl("CVS Protocol", details_html))
  expect_true(grepl("ref_link_click", details_html))
  expect_true(grepl("rf.27", details_html))

  # Test stratum types table
  types_html <- htmltools::renderTags(details$stratum_types(shinysession = mock_session))$html
  expect_true(grepl("Stratum Index", types_html))
  expect_true(grepl("Stratum Name", types_html))
  expect_true(grepl("Stratum Description", types_html))
  expect_true(grepl("Emergent", types_html))
  expect_true(grepl("Herb", types_html))
  expect_true(grepl("Shrub", types_html))
  expect_true(grepl("greater than 35m high", types_html))
})

test_that("build_stratum_method_details_view formats sm.5 (NPS, NA stratum descriptions)", {
  details <- build_stratum_method_details_view(mock_stratum_method_sm5)
  mock_session <- shiny::MockShinySession$new()

  header_html <- htmltools::renderTags(details$stratum_method_header(shinysession = mock_session))$html
  expect_true(grepl("National Park Service", header_html))
  expect_true(grepl("sm.5", header_html))

  details_html <- htmltools::renderTags(details$stratum_method_details(shinysession = mock_session))$html
  expect_true(grepl("NPS Methodology", details_html))
  expect_true(grepl("rf.29", details_html))
  expect_true(grepl("ref_link_click", details_html))

  types_html <- htmltools::renderTags(details$stratum_types(shinysession = mock_session))$html
  expect_true(grepl("T1", types_html))       # stratum_index
  expect_true(grepl("T2", types_html))
  expect_true(grepl("Canopy", types_html))   # stratum_name
  expect_true(grepl("Sub-Canopy", types_html))
  expect_true(grepl("Stratum Index", types_html))
})

test_that("build_stratum_method_details_view formats sm.329 (No Strata, no reference)", {
  details <- build_stratum_method_details_view(mock_stratum_method_sm329)
  mock_session <- shiny::MockShinySession$new()

  header_html <- htmltools::renderTags(details$stratum_method_header(shinysession = mock_session))$html
  expect_true(grepl("No Strata", header_html))
  expect_true(grepl("sm.329", header_html))

  # No rf_code → reference shows Unspecified
  details_html <- htmltools::renderTags(details$stratum_method_details(shinysession = mock_session))$html
  expect_true(grepl("No Strata were used", details_html))
  expect_true(grepl("Unspecified", details_html))  # reference

  # Single module stratum type
  types_html <- htmltools::renderTags(details$stratum_types(shinysession = mock_session))$html
  expect_true(grepl("mod", types_html))       # stratum_index
  expect_true(grepl("module", types_html))    # stratum_name
  expect_true(grepl("No vertical splitting", types_html))
})

test_that("build_stratum_method_details_view handles stratum method without reference", {
  details <- build_stratum_method_details_view(mock_stratum_method_no_types)

  mock_session <- shiny::MockShinySession$new()

  # Test header
  header_html <- htmltools::renderTags(details$stratum_method_header(shinysession = mock_session))$html
  expect_true(grepl("Test Method", header_html))
  expect_true(grepl("sm.999", header_html))

  # Test details (should show "Unspecified" for reference)
  details_html <- htmltools::renderTags(details$stratum_method_details(shinysession = mock_session))$html
  expect_true(grepl("Unspecified", details_html))
  expect_true(grepl("Unspecified", details_html)) # stratum_method_description is NA

  # Test types (should show no types message)
  types_html <- htmltools::renderTags(details$stratum_types(shinysession = mock_session))$html
  expect_true(grepl("No stratum types recorded", types_html))
})

test_that("build_stratum_method_details_view handles missing fields gracefully", {
  minimal_data <- data.frame(
    sm_code = "sm.888",
    stratum_method_name = NA,
    stratum_method_description = NA,
    stratum_assignment = NA,
    rf_code = NA,
    rf_label = NA,
    stringsAsFactors = FALSE
  )
  minimal_data$stratum_types <- list(data.frame())

  details <- build_stratum_method_details_view(minimal_data)

  mock_session <- shiny::MockShinySession$new()

  # Test header with missing stratum_method_name
  header_html <- htmltools::renderTags(details$stratum_method_header(shinysession = mock_session))$html
  expect_true(grepl("Unnamed Stratum Method", header_html))
  expect_true(grepl("sm.888", header_html))

  # Test details with all missing fields
  details_html <- htmltools::renderTags(details$stratum_method_details(shinysession = mock_session))$html
  expect_true(grepl("Unspecified", details_html))
  expect_true(grepl("Unspecified", details_html))
})

test_that("build_stratum_method_details_view creates clickable reference link", {
  details <- build_stratum_method_details_view(mock_stratum_method_sm1)

  mock_session <- shiny::MockShinySession$new()
  details_html <- htmltools::renderTags(details$stratum_method_details(shinysession = mock_session))$html

  # Verify the reference link has the correct structure (uses onclick with Shiny.setInputValue)
  expect_true(grepl("Shiny.setInputValue", details_html, fixed = TRUE))
  expect_true(grepl("ref_link_click", details_html, fixed = TRUE))
  expect_true(grepl("rf.27", details_html))
  expect_true(grepl("CVS Protocol", details_html))
  expect_true(grepl('href="#"', details_html, fixed = TRUE))
})

test_that("stratum types table handles NULL stratum_description gracefully", {
  stratum_method_null_desc <- mock_stratum_method_sm1
  stratum_method_null_desc$stratum_types <- list(data.frame(
    sy_code = c("sy.1", "sy.2"),
    stratum_index = c("A", "F"),
    stratum_name = c("Aquatic submerged", "Floating"),
    stratum_description = c(NA, NA),
    stringsAsFactors = FALSE
  ))

  details <- build_stratum_method_details_view(stratum_method_null_desc)

  mock_session <- shiny::MockShinySession$new()
  types_html <- htmltools::renderTags(details$stratum_types(shinysession = mock_session))$html

  # Table should still render with names
  expect_true(grepl("Aquatic submerged", types_html))
  expect_true(grepl("Floating", types_html))
  expect_true(grepl("Stratum Index", types_html))
})
