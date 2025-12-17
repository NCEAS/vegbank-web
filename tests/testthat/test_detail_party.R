# Tests for party detail view

test_that("build_party_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_party_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_setequal(names(result), c(
    "party_header", "party_organization", "party_contact", "party_contributions"
  ))

  # Each component should be a render function
  expect_true(inherits(result$party_header, "shiny.render.function"))
  expect_true(inherits(result$party_organization, "shiny.render.function"))
  expect_true(inherits(result$party_contact, "shiny.render.function"))
  expect_true(inherits(result$party_contributions, "shiny.render.function"))
})

test_that("build_party_details_view formats party data correctly", {
  result <- build_party_details_view(mock_party_data)

  # Test structure and types
  expect_type(result, "list")
  # Verify names in any order
  expect_setequal(names(result), c(
    "party_header", "party_organization", "party_contact", "party_contributions"
  ))

  # Each component should be a render function
  expect_true(inherits(result$party_header, "shiny.render.function"))
  expect_true(inherits(result$party_organization, "shiny.render.function"))
  expect_true(inherits(result$party_contact, "shiny.render.function"))
  expect_true(inherits(result$party_contributions, "shiny.render.function"))
})
