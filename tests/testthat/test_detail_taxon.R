# Tests for taxon observation detail view

test_that("build_taxon_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_taxon_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")

  # Use expect_true to check that all required components exist
  expect_true(all(
    c("taxon_name", "taxon_coverage", "taxon_aliases", "taxon_identifiers") %in% names(result)
  ))

  # Each component should be a render function
  expect_true(inherits(result$taxon_name, "shiny.render.function"))
  expect_true(inherits(result$taxon_coverage, "shiny.render.function"))
  expect_true(inherits(result$taxon_aliases, "shiny.render.function"))
  expect_true(inherits(result$taxon_identifiers, "shiny.render.function"))
})

test_that("build_taxon_details_view formats taxon data correctly", {
  result <- build_taxon_details_view(mock_taxon_data)

  # Test structure and types
  expect_type(result, "list")

  # Check that all expected keys are present (order doesn't matter)
  expect_true(all(
    c("taxon_name", "taxon_coverage", "taxon_aliases", "taxon_identifiers") %in% names(result)
  ))

  # Each component should be a render function
  expect_true(inherits(result$taxon_name, "shiny.render.function"))
  expect_true(inherits(result$taxon_coverage, "shiny.render.function"))
})
