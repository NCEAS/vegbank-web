# Tests for dataset detail view

# ==== parse_dataset_author_label() ====

test_that("parse_dataset_author_label converts Last, First to First Last", {
  expect_equal(parse_dataset_author_label("Palmquist, Kyle"),    "Kyle Palmquist")
  expect_equal(parse_dataset_author_label("Albers, Gayle"),     "Gayle Albers")
  expect_equal(parse_dataset_author_label("Lee, Michael"),      "Michael Lee")
})

test_that("parse_dataset_author_label returns original string when no comma", {
  expect_equal(parse_dataset_author_label("MononymousAuthor"), "MononymousAuthor")
})

test_that("parse_dataset_author_label handles NULL / NA / empty", {
  expect_equal(parse_dataset_author_label(NULL),         "Unknown Author")
  expect_equal(parse_dataset_author_label(NA_character_), "Unknown Author")
  expect_equal(parse_dataset_author_label(""),           "Unknown Author")
  expect_equal(parse_dataset_author_label("   "),        "Unknown Author")
})

# ==== build_dataset_citation_html() ====

test_that("build_dataset_citation_html builds correct citation — start only", {
  text <- build_dataset_citation_html(
    mock_dataset_ds201120,
    author_name = "Kyle Palmquist",
    start_year  = "2017"
  )
  expect_true(grepl("Kyle Palmquist", text, fixed = TRUE))
  expect_true(grepl("(2017)", text, fixed = TRUE))
  expect_true(grepl("VegBank plot observations", text, fixed = TRUE))
  expect_true(grepl("CEGL007141.v2", text, fixed = TRUE))
  expect_true(grepl("VegBank. Dataset.", text, fixed = TRUE))
  expect_true(grepl("VB.ds.201120.DWLFOT", text, fixed = TRUE))
})

test_that("build_dataset_citation_html sanitizes dataset name", {
  ds_xss <- mock_dataset_ds201120
  ds_xss$name <- '<script>alert("xss")</script>'
  text <- build_dataset_citation_html(
    ds_xss,
    author_name = "Kyle Palmquist",
    start_year  = "2017"
  )
  expect_false(grepl("<script>", text, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", text, fixed = TRUE))
})

# ==== build_dataset_details_view() ====

test_that("build_dataset_details_view handles NULL data gracefully", {
  result <- build_dataset_details_view(NULL)

  expect_type(result, "list")
  expect_named(result, c("dataset_header", "dataset_details", "dataset_citation"))
  expect_s3_class(result$dataset_header,   "shiny.render.function")
  expect_s3_class(result$dataset_details,  "shiny.render.function")
  expect_s3_class(result$dataset_citation, "shiny.render.function")

  mock_session <- shiny::MockShinySession$new()
  html <- htmltools::renderTags(result$dataset_header(shinysession = mock_session))$html
  expect_true(grepl("Dataset details not available", html))
})

test_that("build_dataset_details_view returns correct output names for all three mocks", {
  for (mock in list(mock_dataset_ds201120, mock_dataset_ds201398, mock_dataset_ds201907)) {
    result <- build_dataset_details_view(mock)
    expect_named(result, c("dataset_header", "dataset_details", "dataset_citation"))
    expect_s3_class(result$dataset_header,   "shiny.render.function")
    expect_s3_class(result$dataset_details,  "shiny.render.function")
    expect_s3_class(result$dataset_citation, "shiny.render.function")
  }
})

test_that("build_dataset_details_view header contains ds_code and name", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201120)
  html <- htmltools::renderTags(details$dataset_header(shinysession = mock_session))$html
  expect_true(grepl("ds.201120",      html, fixed = TRUE))
  expect_true(grepl("CEGL007141.v2",  html, fixed = TRUE))

  details2 <- build_dataset_details_view(mock_dataset_ds201398)
  html2 <- htmltools::renderTags(details2$dataset_header(shinysession = mock_session))$html
  expect_true(grepl("ds.201398",       html2, fixed = TRUE))
  expect_true(grepl("unnamed dataset", html2, fixed = TRUE))
})

test_that("build_dataset_details_view header includes copy permalink button — VegBank accession uses identifiers.org", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201120)
  html <- htmltools::renderTags(details$dataset_header(shinysession = mock_session))$html

  expect_true(grepl("vb-copy-permalink", html, fixed = TRUE))
  expect_true(grepl("Copy permalink", html, fixed = TRUE))
  expect_true(grepl("https://identifiers.org/vegbank:VB.ds.201120.DWLFOT", html, fixed = TRUE))
  expect_false(grepl("vegbank.org/cite", html, fixed = TRUE))
})

test_that("build_dataset_details_view header uses identifiers.org permalink for unnamed VegBank dataset", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201398)
  html <- htmltools::renderTags(details$dataset_header(shinysession = mock_session))$html

  expect_true(grepl("vb-copy-permalink", html, fixed = TRUE))
  expect_true(grepl("https://identifiers.org/vegbank:VB.ds.201398.UNNAMEDDATASET", html, fixed = TRUE))
  expect_false(grepl("vegbank.org/cite", html, fixed = TRUE))
})

test_that("build_dataset_details_view header uses doi.org permalink for DOI accession", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201910)
  html <- htmltools::renderTags(details$dataset_header(shinysession = mock_session))$html

  expect_true(grepl("vb-copy-permalink", html, fixed = TRUE))
  expect_true(grepl("https://doi.org/10.5072/FK26D61D4V", html, fixed = TRUE))
  expect_false(grepl("vegbank.org/cite", html, fixed = TRUE))
})

test_that("build_dataset_details_view details card contains accession, author, plot count link", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201120)
  html <- htmltools::renderTags(details$dataset_details(shinysession = mock_session))$html

  expect_true(grepl("VB.ds.201120.DWLFOT", html, fixed = TRUE))  # accession code
  expect_true(grepl("Palmquist, Kyle",     html, fixed = TRUE))  # owner_label as-is
  expect_true(grepl("obs-count-link",      html, fixed = TRUE))  # plot obs link present
  expect_true(grepl("6",                   html, fixed = TRUE))  # obs_count
})

test_that("build_dataset_details_view details card omits description section when blank", {
  mock_session <- shiny::MockShinySession$new()

  # ds201120 has description = "" → treated as invalid by has_valid_field_value
  details <- build_dataset_details_view(mock_dataset_ds201120)
  html <- htmltools::renderTags(details$dataset_details(shinysession = mock_session))$html
  expect_false(grepl("Description", html, fixed = TRUE))

  # ds201398 has description = NA → also invalid
  details2 <- build_dataset_details_view(mock_dataset_ds201398)
  html2 <- htmltools::renderTags(details2$dataset_details(shinysession = mock_session))$html
  expect_false(grepl("Description", html2, fixed = TRUE))
})

test_that("build_dataset_details_view details card shows description when present", {
  mock_session <- shiny::MockShinySession$new()

  ds_with_desc <- mock_dataset_ds201120
  ds_with_desc$description <- "A <b>test</b> description"
  details <- build_dataset_details_view(ds_with_desc)
  html <- htmltools::renderTags(details$dataset_details(shinysession = mock_session))$html
  expect_true(grepl("Description",  html, fixed = TRUE))
  expect_true(grepl("A ",           html, fixed = TRUE))
  expect_true(grepl("<b>test</b>",  html, fixed = TRUE))   # safe inline tag preserved
})

test_that("build_dataset_details_view citation card contains expected parts", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201120)
  html <- htmltools::renderTags(details$dataset_citation(shinysession = mock_session))$html

  expect_true(grepl("Kyle Palmquist",         html, fixed = TRUE))
  expect_true(grepl("VegBank plot observations", html, fixed = TRUE))
  expect_true(grepl("VegBank. Dataset.",       html, fixed = TRUE))
  expect_true(grepl("VB.ds.201120.DWLFOT",    html, fixed = TRUE))
  expect_true(grepl("2017",                   html, fixed = TRUE))
})

test_that("build_dataset_details_view citation for ds201398 has correct author", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201398)
  html <- htmltools::renderTags(details$dataset_citation(shinysession = mock_session))$html
  expect_true(grepl("Gayle Albers",               html, fixed = TRUE))
  expect_true(grepl("VB.ds.201398.UNNAMEDDATASET", html, fixed = TRUE))
})

test_that("build_dataset_details_view details plot count link has correct ds_code", {
  mock_session <- shiny::MockShinySession$new()

  details <- build_dataset_details_view(mock_dataset_ds201907)
  html <- htmltools::renderTags(details$dataset_details(shinysession = mock_session))$html
  expect_true(grepl("ds.201907",  html, fixed = TRUE))
  expect_true(grepl("1901",       html, fixed = TRUE))
  expect_true(grepl("obs-count-link", html, fixed = TRUE))
})

# ==== build_dataset_citation_html() accession/DOI link rendering ====

test_that("build_dataset_citation_html renders VegBank accession as correct link and label", {
  tag <- build_dataset_citation_html(
    mock_dataset_ds201120,
    author_name = "Kyle Palmquist",
    start_year  = "2017"
  )
  html <- as.character(tag)
  expect_true(grepl(">vegbank:VB.ds.201120.DWLFOT<", html, fixed = TRUE))
  expect_true(grepl("href=\"https://identifiers.org/vegbank:VB.ds.201120.DWLFOT\"", html, fixed = TRUE))
})

test_that("build_dataset_citation_html renders DOI as correct link and label", {
  tag <- build_dataset_citation_html(
    mock_dataset_ds201910,
    author_name = "Rushirah Nenuji",
    start_year  = "2026"
  )
  html <- as.character(tag)
  expect_true(grepl(">doi:10.5072/FK26D61D4V<", html, fixed = TRUE))
  expect_true(grepl("href=\"https://doi.org/10.5072/FK26D61D4V\"", html, fixed = TRUE))
})

test_that("build_dataset_citation_html renders doi:-prefixed accession as link", {
  ds <- mock_dataset_ds201910
  ds$accession_code <- "doi:10.82902/J17P4J"
  tag <- build_dataset_citation_html(ds, author_name = "Matthew Jones", start_year = "2026")
  html <- as.character(tag)
  expect_true(grepl(">doi:10.82902/J17P4J<", html, fixed = TRUE))
  expect_true(grepl("href=\"https://doi.org/10.82902/J17P4J\"", html, fixed = TRUE))
})
