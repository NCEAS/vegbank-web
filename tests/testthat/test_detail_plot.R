# Tests for plot observation detail view

test_that("build_plot_obs_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_plot_obs_details_view(NULL)

  # It should return a list with placeholder components (no plot_header for empty data)
  expect_type(result, "list")
  expect_named(result, c(
    "plot_header", "plot_id_details", "location_details", "layout_details",
    "environmental_details", "methods_details", "plot_quality_details",
    "taxa_details", "communities_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
})

test_that("build_plot_obs_details_view formats plot data correctly", {
  result <- build_plot_obs_details_view(mock_plot_data)

  # Test structure and types (includes plot_header when data is present)
  expect_type(result, "list")
  expect_named(result, c(
    "plot_header", "plot_id_details", "location_details", "layout_details",
    "environmental_details", "methods_details", "plot_quality_details",
    "taxa_details", "communities_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
})

test_that("normalize_plot_obs_result handles dataframe payload format", {
  payload <- data.frame(
    author_obs_code = "ACAD.206",
    author_plot_code = "ACAD.206",
    latitude = 44.343921905,
    longitude = -68.046130404,
    plot_validation_level_descr = "Validated",
    stringsAsFactors = FALSE
  )

  payload$top_classifications <- I(list(data.frame(
    cl_code = "cl.47305",
    comm_name = "Picea mariana / Kalmia angustifolia Woodland",
    stringsAsFactors = FALSE
  )))

  payload$top_taxon_observations <- I(list(data.frame(
    pc_code = c("pc.8790", "pc.10653"),
    plant_name = c("Amelanchier stolonifera", "Aronia melanocarpa"),
    stratum_name = c("Shrub", "-all-"),
    cover = c(0.5625, 0.0625),
    stringsAsFactors = FALSE
  )))

  normalized <- vegbankweb:::normalize_plot_obs_result(payload)

  expect_true(normalized$has_data)
  expect_equal(normalized$plot_observation$author_obs_code, "ACAD.206")
  expect_false("top_taxon_observations" %in% names(normalized$plot_observation))
  expect_equal(nrow(normalized$top_taxon_observations), 2)
  expect_equal(normalized$top_taxon_observations$pc_code[1], "pc.8790")
  expect_equal(nrow(normalized$communities), 1)
  expect_equal(normalized$communities$cl_code[1], "cl.47305")
})

test_that("prepare_taxa_display groups by stratum and sorts cover", {
  taxa <- data.frame(
    plant_name = c("A", "B", "C", "D"),
    stratum_name = c("Canopy", "Shrub", NA, "Shrub"),
    cover = c(25.4, 60.2, 40.8, 20.1),
    stringsAsFactors = FALSE
  )

  prepared <- vegbankweb:::prepare_taxa_display(taxa)

  expect_equal(prepared$stratum_label, c("Canopy", "Shrub", "Shrub", "Unspecified"))
  expect_true(prepared$cover_numeric[2] >= prepared$cover_numeric[4])
  expect_equal(tail(prepared$stratum_label, 1), "Unspecified")
  expect_equal(prepared$cover_display[1], "25.40%")
  expect_equal(prepared$cover_display[2], "60.20%")
})
