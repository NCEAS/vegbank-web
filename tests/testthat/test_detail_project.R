# Tests for project detail view

test_that("build_project_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_project_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_setequal(names(result), c(
    "project_name", "project_description",
    "project_dates", "project_contributors", "project_observations"
  ))

  # Each component should be a render function
  expect_true(inherits(result$project_name, "shiny.render.function"))
  expect_true(inherits(result$project_description, "shiny.render.function"))
  expect_true(inherits(result$project_dates, "shiny.render.function"))
  expect_true(inherits(result$project_contributors, "shiny.render.function"))
  expect_true(inherits(result$project_observations, "shiny.render.function"))
})

test_that("build_project_details_view formats project data correctly", {
  result <- build_project_details_view(mock_project_data)

  # Test structure and types
  expect_type(result, "list")
  # Verify names in any order
  expect_setequal(names(result), c(
    "project_name", "project_observations", "project_description",
    "project_contributors", "project_dates"
  ))

  # Each component should be a render function
  expect_true(inherits(result$project_name, "shiny.render.function"))
  expect_true(inherits(result$project_description, "shiny.render.function"))
  expect_true(inherits(result$project_observations, "shiny.render.function"))
  expect_true(inherits(result$project_dates, "shiny.render.function"))
})
