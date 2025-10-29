# Tests for plot observation detail view

test_that("build_plot_obs_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_plot_obs_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c(
    "plot_id_details", "location_details", "layout_details",
    "environmental_details", "methods_details", "plot_quality_details",
    "taxa_details", "communities_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
})

test_that("build_plot_obs_details_view formats plot data correctly", {
  result <- build_plot_obs_details_view(mock_plot_data)

  # Test structure and types
  expect_type(result, "list")
  expect_named(result, c(
    "plot_id_details", "location_details", "layout_details",
    "environmental_details", "methods_details", "plot_quality_details",
    "taxa_details", "communities_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
})
