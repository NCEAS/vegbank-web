# Tests for project detail view

test_that("build_project_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_project_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_setequal(names(result), c(
    "project_header", "project_description",
    "project_dates", "project_contributors", "project_observations"
  ))

  # Each component should be a render function
  expect_true(inherits(result$project_header, "shiny.render.function"))
  expect_true(inherits(result$project_description, "shiny.render.function"))
  expect_true(inherits(result$project_dates, "shiny.render.function"))
  expect_true(inherits(result$project_contributors, "shiny.render.function"))
  expect_true(inherits(result$project_observations, "shiny.render.function"))
})

test_that("build_project_details_view formats project data correctly — all three mocks", {
  for (mock in list(mock_project_pj339, mock_project_pj10559, mock_project_pj11008)) {
    result <- build_project_details_view(mock)
    expect_type(result, "list")
    expect_setequal(names(result), c(
      "project_header", "project_observations", "project_description",
      "project_contributors", "project_dates"
    ))
    expect_true(inherits(result$project_header, "shiny.render.function"))
    expect_true(inherits(result$project_description, "shiny.render.function"))
    expect_true(inherits(result$project_observations, "shiny.render.function"))
    expect_true(inherits(result$project_dates, "shiny.render.function"))
  }
})

test_that("build_project_details_view renders correct header content", {
  # pj.339: no start/stop → no date range paragraph
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj339)$project_header
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Composition and function of vegetation alliances", html$html))
      expect_true(grepl("pj.339", html$html))
      expect_false(grepl("From ", html$html))  # no date range
    }
  )

  # pj.10559: start=2013-05-01, stop=NA → "From 2013-05-01"
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj10559)$project_header
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Ecological Site Descriptions", html$html))
      expect_true(grepl("pj.10559", html$html))
      expect_true(grepl("From 2013-05-01", html$html))
    }
  )

  # pj.11008: no dates → no date range paragraph
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj11008)$project_header
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Carolina Vegetation Survey", html$html))
      expect_true(grepl("pj.11008", html$html))
      expect_false(grepl("From ", html$html))
    }
  )
})

test_that("build_project_details_view renders correct observations content", {
  # pj.339: 12962 → clickable link
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj339)$project_observations
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("12962", html$html))
      expect_true(grepl("obs-count-link", html$html))
    }
  )

  # pj.10559: 24 → clickable link
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj10559)$project_observations
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("24", html$html))
      expect_true(grepl("obs-count-link", html$html))
    }
  )

  # pj.11008: 48 → clickable link
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj11008)$project_observations
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("48", html$html))
      expect_true(grepl("obs-count-link", html$html))
    }
  )
})

test_that("build_project_details_view renders correct description content", {
  # pj.339: has description
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj339)$project_description
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Improved policy and management", html$html))
    }
  )

  # pj.10559: has description
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj10559)$project_description
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("USDA Natural Resource Conservation Service", html$html))
    }
  )

  # pj.11008: NA description → fallback
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_project_details_view(mock_project_pj11008)$project_description
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No description recorded", html$html))
    }
  )
})
