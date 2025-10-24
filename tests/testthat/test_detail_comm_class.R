# Tests for community classification detail view

test_that("build_comm_class_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_comm_class_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c(
    "observation_details", "community_interpretation"
  ))

  # Each component should be a render function
  expect_true(inherits(result$observation_details, "shiny.render.function"))
  expect_true(inherits(result$community_interpretation, "shiny.render.function"))
})

test_that("build_comm_class_details_view formats classification data correctly", {
  result <- build_comm_class_details_view(mock_comm_class_data)

  # Test structure and types
  expect_type(result, "list")
  # Verify names in any order
  expect_setequal(names(result), c(
    "observation_details", "community_interpretation"
  ))

  # Each component should be a render function
  expect_true(inherits(result$observation_details, "shiny.render.function"))
  expect_true(inherits(result$community_interpretation, "shiny.render.function"))
})
