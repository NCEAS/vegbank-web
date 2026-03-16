# Tests for taxon observation detail view

test_that("build_taxon_obs_details_view handles NULL data gracefully", {
  result <- build_taxon_obs_details_view(NULL, NULL)

  expect_type(result, "list")
  expect_named(result, c(
    "taxon_obs_header",
    "taxon_obs_interpretations",
    "taxon_obs_details",
    "taxon_obs_importance"
  ))
  expect_true(inherits(result$taxon_obs_header, "shiny.render.function"))
  expect_true(inherits(result$taxon_obs_interpretations, "shiny.render.function"))
  expect_true(inherits(result$taxon_obs_details, "shiny.render.function"))
  expect_true(inherits(result$taxon_obs_importance, "shiny.render.function"))
})

test_that("build_taxon_obs_details_view handles empty data gracefully", {
  result <- build_taxon_obs_details_view(data.frame(), data.frame())

  expect_type(result, "list")
  expect_setequal(names(result), taxon_obs_detail_output_names)
})

test_that("build_taxon_obs_details_view returns render functions for valid data", {
  result <- build_taxon_obs_details_view(mock_taxon_obs_to2178147, mock_taxon_interps_to2178147)

  expect_type(result, "list")
  expect_setequal(names(result), taxon_obs_detail_output_names)
  expect_true(inherits(result$taxon_obs_header, "shiny.render.function"))
  expect_true(inherits(result$taxon_obs_interpretations, "shiny.render.function"))
  expect_true(inherits(result$taxon_obs_details, "shiny.render.function"))
  expect_true(inherits(result$taxon_obs_importance, "shiny.render.function"))
})

test_that("create_taxon_obs_header_ui renders plant name, to_code, and ob_code link", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_header_ui(mock_taxon_obs_to2178147)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Bromus sp. #5", html$html))
      expect_true(grepl("to.2178147", html$html))
      expect_true(grepl("ob.120762", html$html))
      # ob_code should be a clickable link
      expect_true(grepl("plot_link_click", html$html))
    }
  )

  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_header_ui(mock_taxon_obs_minimal)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Unknown sp.", html$html))
      expect_true(grepl("to.999", html$html))
    }
  )
})

test_that("create_taxon_obs_interpretations_ui renders interpretations in reverse chronological order", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_interpretations_ui(mock_taxon_interps_to2178147)
    },
    {
      html <- session$getOutput("test")
      # Most recent (Lee, Michael / Taxonomic revision / 2015) appears before oldest (Wagner, Gail / 2011)
      expect_true(regexpr("Lee, Michael", html$html) < regexpr("Wagner, Gail", html$html))
      expect_true(grepl("Bromus pubescens \\[Weakley 2015\\]", html$html))
      # Party labels present
      expect_true(grepl("Wagner, Gail", html$html))
      expect_true(grepl("LeBlond, Richard", html$html))
      expect_true(grepl("Lee, Michael", html$html))
      # Boolean flags shown as badges (not "Yes"/"No")
      expect_true(grepl("Original", html$html))  # is_orig=TRUE for first interp
      expect_true(grepl("Current", html$html))   # is_curr=TRUE for last interp
      # Prose subtitle: type + party + date
      expect_true(grepl("Authored", html$html))
      expect_true(grepl("Finer resolution", html$html))
      expect_true(grepl("Taxonomic revision", html$html))
      expect_true(grepl("by", html$html))
      # Fit and confidence
      expect_true(grepl("Absolutely correct", html$html))
      expect_true(grepl("High", html$html))
      # Links
      expect_true(grepl("plant_link_click", html$html))
      expect_true(grepl("party_link_click", html$html))
    }
  )
})

test_that("create_taxon_obs_interpretations_ui handles empty interpretations", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_interpretations_ui(data.frame())
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No taxon interpretations recorded", html$html))
    }
  )

  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_interpretations_ui(NULL)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No taxon interpretations recorded", html$html))
    }
  )
})

test_that("create_taxon_obs_details_ui renders inference area, orig/curr links, and reference", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_details_ui(mock_taxon_obs_to2178147, mock_taxon_interps_to2178147)
    },
    {
      html <- session$getOutput("test")
      # Inference area with unit
      expect_true(grepl("100", html$html))
      expect_true(grepl("m\u00B2", html$html))
      # Original interpretation: Bromus [Weakley Jan 1, 2006] (is_orig=TRUE, pc.203088)
      expect_true(grepl("Bromus \\[Weakley Jan 1, 2006\\]", html$html))
      # Current interpretation: Bromus pubescens [Weakley 2015] (is_curr=TRUE, pc.391162)
      expect_true(grepl("Bromus pubescens \\[Weakley 2015\\]", html$html))
      # Reference label
      expect_true(grepl("Weakley Jan 1, 2006", html$html))
      # Reference link uses ref_link_click
      expect_true(grepl("ref_link_click", html$html))
      # Headers
      expect_true(grepl("Taxon Inference Area", html$html))
      expect_true(grepl("Original Interpretation", html$html))
      expect_true(grepl("Current Interpretation", html$html))
      expect_true(grepl("Reference", html$html))
    }
  )
})

test_that("create_taxon_obs_details_ui shows Unspecified for missing fields", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_details_ui(mock_taxon_obs_minimal, data.frame())
    },
    {
      html <- session$getOutput("test")
      # All fields missing → Unspecified
      expect_true(grepl("Unspecified", html$html))
    }
  )
})

test_that("create_taxon_obs_importance_ui renders importance table", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_importance_ui(mock_taxon_obs_to2178147)
    },
    {
      html <- session$getOutput("test")
      # Stratum names
      expect_true(grepl("Herb", html$html))
      expect_true(grepl("module", html$html))
      # Cover formatted with %
      expect_true(grepl("0.51%", html$html))
      # Cover code
      expect_true(grepl(">2<", html$html))
      # Column headers
      expect_true(grepl("Stratum", html$html))
      expect_true(grepl("Cover", html$html))
      expect_true(grepl("Basal Area", html$html))
      expect_true(grepl("Bio-mass", html$html))
    }
  )
})

test_that("create_taxon_obs_importance_ui handles empty importance data", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_taxon_obs_importance_ui(mock_taxon_obs_minimal)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No taxon importance data recorded", html$html))
    }
  )
})

test_that("taxon_obs_detail_output_names contains all expected output names", {
  expect_equal(taxon_obs_detail_output_names, c(
    "taxon_obs_header",
    "taxon_obs_interpretations",
    "taxon_obs_details",
    "taxon_obs_importance"
  ))
})
