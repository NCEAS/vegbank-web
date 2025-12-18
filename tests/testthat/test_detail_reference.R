# Tests for reference detail view

test_that("build_reference_details_view handles NULL data gracefully", {
  result <- build_reference_details_view(NULL)

  expect_type(result, "list")
  expect_named(result, c(
    "reference_header",
    "reference_identifiers",
    "reference_publication"
  ))

  expect_s3_class(result$reference_header, "shiny.render.function")
  mock_session <- shiny::MockShinySession$new()
  html <- htmltools::renderTags(result$reference_header(shinysession = mock_session))$html
  expect_true(grepl("Reference details not available", html))
})

test_that("build_reference_details_view formats reference data", {
  details <- build_reference_details_view(mock_reference_data)

  expect_type(details, "list")
  expect_named(details, c(
    "reference_header",
    "reference_identifiers",
    "reference_publication"
  ))

  expect_s3_class(details$reference_header, "shiny.render.function")
  mock_session <- shiny::MockShinySession$new()
  summary_html <- htmltools::renderTags(details$reference_header(shinysession = mock_session))$html
  expect_true(grepl("Example Author. 2020.", summary_html, fixed = TRUE))
  expect_true(grepl("Book", summary_html, fixed = TRUE))

  identifiers_html <- htmltools::renderTags(details$reference_identifiers(shinysession = mock_session))$html
  expect_true(grepl("No DOI, ISBN, or URL provided", identifiers_html))

  publication_html <- htmltools::renderTags(details$reference_publication(shinysession = mock_session))$html
  expect_true(grepl("2020-01-01", publication_html, fixed = TRUE))
  expect_true(grepl("Example Title", publication_html, fixed = TRUE))
})
