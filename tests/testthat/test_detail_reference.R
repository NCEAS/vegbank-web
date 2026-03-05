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

test_that("build_reference_details_view formats reference data — all three mocks", {
  for (mock in list(mock_reference_rf36675, mock_reference_rf37, mock_reference_rf35243)) {
    details <- build_reference_details_view(mock)
    expect_type(details, "list")
    expect_named(details, c("reference_header", "reference_identifiers", "reference_publication"))
    expect_s3_class(details$reference_header, "shiny.render.function")
    expect_s3_class(details$reference_identifiers, "shiny.render.function")
    expect_s3_class(details$reference_publication, "shiny.render.function")
  }
})

test_that("build_reference_details_view renders correct header content", {
  mock_session <- shiny::MockShinySession$new()

  # rf.36675: Report type, label present, has publication date
  details_36675 <- build_reference_details_view(mock_reference_rf36675)
  html <- htmltools::renderTags(details_36675$reference_header(shinysession = mock_session))$html
  expect_true(grepl("NYNHP Field Form Instructions", html, fixed = TRUE))
  expect_true(grepl("rf.36675", html, fixed = TRUE))
  expect_true(grepl("Report", html, fixed = TRUE))
  expect_true(grepl("1905-06-22", html, fixed = TRUE))

  # rf.37: no reference_type, label present, has publication date
  details_37 <- build_reference_details_view(mock_reference_rf37)
  html37 <- htmltools::renderTags(details_37$reference_header(shinysession = mock_session))$html
  expect_true(grepl("USDA Plants 2011", html37, fixed = TRUE))
  expect_true(grepl("rf.37", html37, fixed = TRUE))
  expect_true(grepl("2011-08-25", html37, fixed = TRUE))

  # rf.35243: no type, no publication date, label is UNKNOWN
  details_35243 <- build_reference_details_view(mock_reference_rf35243)
  html35243 <- htmltools::renderTags(details_35243$reference_header(shinysession = mock_session))$html
  expect_true(grepl("UNKNOWN", html35243, fixed = TRUE))
  expect_true(grepl("rf.35243", html35243, fixed = TRUE))
  expect_false(grepl("Published:", html35243, fixed = TRUE))  # no date
})

test_that("build_reference_details_view renders correct identifiers content", {
  mock_session <- shiny::MockShinySession$new()

  # rf.36675: has URL → rendered as clickable link
  details_36675 <- build_reference_details_view(mock_reference_rf36675)
  id_html <- htmltools::renderTags(details_36675$reference_identifiers(shinysession = mock_session))$html
  expect_true(grepl("whiteoak.natureserve.org", id_html, fixed = TRUE))
  expect_true(grepl("<a ", id_html, fixed = TRUE))

  # rf.37: no doi/isbn/url → fallback message
  details_37 <- build_reference_details_view(mock_reference_rf37)
  id_html37 <- htmltools::renderTags(details_37$reference_identifiers(shinysession = mock_session))$html
  expect_true(grepl("No DOI, ISBN, or URL recorded", id_html37))

  # rf.35243: all NA → fallback message
  details_35243 <- build_reference_details_view(mock_reference_rf35243)
  id_html35243 <- htmltools::renderTags(details_35243$reference_identifiers(shinysession = mock_session))$html
  expect_true(grepl("No DOI, ISBN, or URL recorded", id_html35243))
})

test_that("build_reference_details_view renders correct publication content", {
  mock_session <- shiny::MockShinySession$new()

  # rf.36675: full citation, title, publisher, publication place, 31 pages, date
  details_36675 <- build_reference_details_view(mock_reference_rf36675)
  pub_html <- htmltools::renderTags(details_36675$reference_publication(shinysession = mock_session))$html
  expect_true(grepl("Edinger et al. 2000", pub_html, fixed = TRUE))
  expect_true(grepl("Community Field Form Instructions", pub_html, fixed = TRUE))
  expect_true(grepl("New York Natural Heritage Program", pub_html, fixed = TRUE))
  expect_true(grepl("1905-06-22", pub_html, fixed = TRUE))

  # rf.37: full citation includes USDA, title present, no total_pages
  details_37 <- build_reference_details_view(mock_reference_rf37)
  pub_html37 <- htmltools::renderTags(details_37$reference_publication(shinysession = mock_session))$html
  expect_true(grepl("USDA, NRCS. 2011", pub_html37, fixed = TRUE))
  expect_true(grepl("The Plants Database", pub_html37, fixed = TRUE))

  # rf.35243: citation is UNKNOWN, no other fields
  details_35243 <- build_reference_details_view(mock_reference_rf35243)
  pub_html35243 <- htmltools::renderTags(details_35243$reference_publication(shinysession = mock_session))$html
  expect_true(grepl("UNKNOWN", pub_html35243, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# URL scheme whitelist / stored-XSS mitigation
# ---------------------------------------------------------------------------

test_that("build_reference_identifiers_ui renders http and https URLs as clickable links", {
  mock_session <- shiny::MockShinySession$new()

  for (safe_url in c("http://example.com/page", "https://example.com/page")) {
    ref <- mock_reference_rf36675
    ref$url <- safe_url

    details <- build_reference_details_view(ref)
    id_html <- htmltools::renderTags(details$reference_identifiers(shinysession = mock_session))$html

    expect_true(grepl("<a ", id_html, fixed = TRUE),
      info = paste("Expected <a> tag for safe URL:", safe_url))
    expect_true(grepl(safe_url, id_html, fixed = TRUE),
      info = paste("Expected URL text present:", safe_url))
  }
})

test_that("build_reference_identifiers_ui renders disallowed URL schemes as plain text, not links", {
  mock_session <- shiny::MockShinySession$new()

  dangerous_urls <- c(
    "javascript:alert('xss')",
    "javascript:void(0)",
    "data:text/html,<script>alert(1)</script>",
    "vbscript:msgbox('xss')",
    "file:///etc/passwd"
  )

  for (bad_url in dangerous_urls) {
    ref <- mock_reference_rf36675
    ref$url <- bad_url

    details <- build_reference_details_view(ref)
    id_html <- htmltools::renderTags(details$reference_identifiers(shinysession = mock_session))$html

    expect_false(grepl("<a ", id_html, fixed = TRUE),
      info = paste("Expected NO <a> tag for dangerous URL scheme:", bad_url))
  }
})
