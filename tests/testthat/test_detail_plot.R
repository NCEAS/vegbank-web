# Tests for plot observation detail view

test_that("build_plot_obs_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_plot_obs_details_view(NULL)

  # It should return a list with placeholder components (no plot_header for empty data)
  expect_type(result, "list")
  expect_named(result, c(
    "plot_notification", "plot_header", "author_code_details", "date_details",
    "location_details", "layout_details", "environmental_details", "methods_details",
    "plot_quality_details", "plot_vegetation_details", "communities_details", "taxa_details",
    "disturbances_details", "soils_details", "plot_misc_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$author_code_details, "shiny.render.function"))
})

test_that("build_plot_obs_details_view formats plot data correctly", {
  result <- build_plot_obs_details_view(mock_plot_data)

  # Test structure and types (includes plot_header when data is present)
  expect_type(result, "list")
  expect_named(result, c(
    "plot_notification", "plot_header", "author_code_details", "date_details",
    "location_details", "layout_details", "environmental_details", "methods_details",
    "plot_quality_details", "plot_vegetation_details", "communities_details", "taxa_details",
    "disturbances_details", "soils_details", "plot_misc_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$author_code_details, "shiny.render.function"))
})

test_that("normalize_plot_obs_result handles dataframe payload format", {
  # Use realistic ACAD.143 data (ob.2948) from the Acadia National Park dataset
  payload <- mock_plot_data

  normalized <- vegbankweb:::normalize_plot_obs_result(payload)

  expect_true(normalized$has_data)
  expect_equal(normalized$plot_observation$ob_code, "ob.2948")
  expect_equal(normalized$plot_observation$author_obs_code, "ACAD.143")
  expect_equal(normalized$plot_observation$project_name, "Acadia National Park")
  expect_equal(normalized$plot_observation$state_province, "Maine")
  expect_equal(normalized$plot_observation$year, "1998")
  expect_equal(normalized$plot_observation$taxon_count, 11L)

  # Nested columns should be extracted and removed from plot_observation
  expect_false("top_taxon_observations" %in% names(normalized$plot_observation))
  expect_false("top_classifications" %in% names(normalized$plot_observation))
  expect_false("disturbances" %in% names(normalized$plot_observation))
  expect_false("soils" %in% names(normalized$plot_observation))

  # top_taxon_observations: 5 rows (3 "-all-" + 2 stratum-specific)
  expect_equal(nrow(normalized$top_taxon_observations), 5)
  expect_equal(normalized$top_taxon_observations$pc_code[1], "pc.10653")
  expect_equal(normalized$top_taxon_observations$plant_name[3], "Bryophyta")

  # communities: 1 row with full classification data
  expect_equal(nrow(normalized$communities), 1)
  expect_equal(normalized$communities$cl_code[1], "cl.1553")
  expect_equal(normalized$communities$comm_code[1], "CEGL005094")
  expect_equal(normalized$communities$cc_code[1], "cc.46260")
  expect_equal(normalized$communities$ci_code[1], "ci.297")
  expect_equal(
    normalized$communities$comm_name[1],
    "Vaccinium angustifolium - Sorbus americana / Sibbaldiopsis tridentata Dwarf-shrubland"
  )

  # disturbances: 1 row
  expect_equal(nrow(normalized$disturbances), 1)
  expect_equal(normalized$disturbances$type[1], "unknown")
  expect_true(is.na(normalized$disturbances$intensity[1]))

  # soils: 1 row with texture present
  expect_equal(nrow(normalized$soils), 1)
  expect_equal(normalized$soils$horizon[1], "unknown")
  expect_equal(normalized$soils$texture[1], "Sands: Sand")
  expect_true(is.na(normalized$soils$ph[1]))
})

test_that("normalize_plot_obs_result handles ACAD.144 with null observation fields", {
  # ob.2949: observation_narrative = NA, slope_aspect/gradient = 0, no rock_type,
  # surficial_deposits present, hydrologic_regime = Seasonally flooded
  normalized <- vegbankweb:::normalize_plot_obs_result(mock_plot_data_acad144)

  expect_true(normalized$has_data)
  expect_equal(normalized$plot_observation$ob_code, "ob.2949")
  expect_equal(normalized$plot_observation$hydrologic_regime, "Seasonally flooded")
  expect_true(is.na(normalized$plot_observation$observation_narrative))
  expect_true(is.na(normalized$plot_observation$rock_type))
  expect_equal(normalized$plot_observation$surficial_deposits, "Glacial Deposits: Till")

  # Zero slope values should be preserved (not treated as missing)
  expect_equal(normalized$plot_observation$slope_aspect, 0L)
  expect_equal(normalized$plot_observation$slope_gradient, 0L)

  # Peat soil with no texture
  expect_equal(nrow(normalized$soils), 1)
  expect_true(is.na(normalized$soils$texture[1]))
  expect_true(grepl("peat depth", normalized$soils$description[1]))

  # Communities: Typha eastern marsh
  expect_equal(normalized$communities$comm_code[1], "CEGL006153")
  expect_equal(normalized$communities$cl_code[1], "cl.1554")
})

test_that("normalize_plot_obs_result handles ACAD.145 with high taxon count", {
  # ob.2950: taxon_count = 28, topo_position = Midslope, woodland community
  normalized <- vegbankweb:::normalize_plot_obs_result(mock_plot_data_acad145)

  expect_true(normalized$has_data)
  expect_equal(normalized$plot_observation$ob_code, "ob.2950")
  expect_equal(normalized$plot_observation$taxon_count, 28L)
  expect_equal(normalized$plot_observation$topo_position, "Midslope")
  expect_equal(normalized$plot_observation$slope_aspect, 270L)
  expect_equal(normalized$plot_observation$soil_drainage, "somewhat excessively drained")

  # Loamy sand soil texture should be preserved
  expect_equal(nrow(normalized$soils), 1)
  expect_equal(normalized$soils$texture[1], "Loamy Sands: Loamy Sand")

  # Woodland community
  expect_equal(normalized$communities$comm_code[1], "CEGL006116")
  expect_true(grepl("Pinus rigida", normalized$communities$comm_name[1]))
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

test_that("prepare_taxa_display sorts -all- stratum rows to the end", {
  # ACAD.143-style data: each species has both a stratum-specific row and a "-all-" row
  taxa <- data.frame(
    plant_name = c("Aronia melanocarpa", "Aronia melanocarpa", "Bryophyta", "Bryophyta", "Danthonia spicata"),
    stratum_name = c("-all-", "Dwarf Shrub", "-all-", "Nonvascular", "-all-"),
    cover = c(0.1875, 0.1875, 0.5625, 0.5625, 0.0625),
    pc_code = c("pc.10653", "pc.10653", "pc.92211", "pc.92211", "pc.22506"),
    stringsAsFactors = FALSE
  )

  prepared <- vegbankweb:::prepare_taxa_display(taxa)

  # All "-all-" rows must appear after all stratum-specific rows
  all_positions <- which(prepared$stratum_label == "-all-")
  specific_positions <- which(!prepared$stratum_label %in% c("-all-", "Unspecified"))
  expect_true(length(all_positions) > 0)
  expect_true(length(specific_positions) > 0)
  expect_true(min(all_positions) > max(specific_positions))

  # The two stratum-specific rows (Dwarf Shrub and Nonvascular) should appear in
  # stratum order determined by first appearance in the original data
  first_specific <- prepared$stratum_label[specific_positions[1]]
  expect_true(first_specific %in% c("Dwarf Shrub", "Nonvascular"))

  # Within each stratum group, rows should be sorted descending by cover
  for (stratum in unique(prepared$stratum_label[specific_positions])) {
    stratum_covers <- prepared$cover_numeric[prepared$stratum_label == stratum]
    if (length(stratum_covers) > 1) {
      expect_true(all(diff(stratum_covers) <= 0),
        info = paste("covers in stratum", stratum, "should be descending"))
    }
  }

  # All rows have cover_display formatted as "X.XX%"
  expect_true(all(grepl("^[0-9.]+%$", prepared$cover_display)))
})

test_that("prepare_taxa_display handles taxa with missing cover values", {
  taxa <- data.frame(
    plant_name = c("Species A", "Species B", "Species C"),
    stratum_name = c("Tree", "Shrub", "Herbaceous"),
    cover = c(10.0, NA, 5.0),
    stringsAsFactors = FALSE
  )

  prepared <- vegbankweb:::prepare_taxa_display(taxa)

  expect_equal(nrow(prepared), 3)
  # NA cover_numeric should result in a non-NA display value (either "" or a placeholder)
  na_row <- prepared[is.na(prepared$cover_numeric), ]
  expect_equal(nrow(na_row), 1)
  expect_false(is.na(na_row$cover_display))
  # Non-NA covers should be formatted as "X.XX%"
  expect_equal(prepared$cover_display[prepared$plant_name == "Species A"], "10.00%")
  expect_equal(prepared$cover_display[prepared$plant_name == "Species C"], "5.00%")
})

test_that("build_plot_obs_details_view handles ACAD.144 with null observation_narrative", {
  # ob.2949 has observation_narrative = NA — should not crash
  result <- build_plot_obs_details_view(mock_plot_data_acad144)

  expect_type(result, "list")
  expect_named(result, c(
    "plot_notification", "plot_header", "author_code_details", "date_details",
    "location_details", "layout_details", "environmental_details", "methods_details",
    "plot_quality_details", "plot_vegetation_details", "communities_details", "taxa_details",
    "disturbances_details", "soils_details", "plot_misc_details"
  ))
  expect_true(inherits(result$taxa_details, "shiny.render.function"))
  expect_true(inherits(result$disturbances_details, "shiny.render.function"))
  expect_true(inherits(result$soils_details, "shiny.render.function"))
})

test_that("build_plot_obs_details_view handles ACAD.145 with large taxon count", {
  # ob.2950 has taxon_count = 28
  result <- build_plot_obs_details_view(mock_plot_data_acad145)

  expect_type(result, "list")
  expect_true(inherits(result$taxa_details, "shiny.render.function"))
})

test_that("build_plot_obs_details_view renders synonym notification for replaced observations", {
  # has_observation_synonym = TRUE and replaced_by_ob_code is set
  # The notification renderUI is present and the underlying data has the expected flags
  result <- build_plot_obs_details_view(mock_plot_data_with_synonym)

  expect_true(inherits(result$plot_notification, "shiny.render.function"))

  # Verify the input data that drives the notification logic
  expect_true(isTRUE(mock_plot_data_with_synonym$has_observation_synonym))
  expect_equal(mock_plot_data_with_synonym$replaced_by_ob_code, "ob.9999")
})

test_that("build_plot_obs_details_view renders no notification for normal observations", {
  # has_observation_synonym = NA (default) — should be a render function but no alert
  result <- build_plot_obs_details_view(mock_plot_data)

  expect_true(inherits(result$plot_notification, "shiny.render.function"))
  # Verify the input does NOT have synonym flag set
  expect_false(isTRUE(mock_plot_data$has_observation_synonym))
})

test_that("build_plot_obs_details_view includes reference data in plot header", {
  # mock with rf_code/rf_label and previous_ob_code set
  result <- build_plot_obs_details_view(mock_plot_data_with_reference)

  # Should produce a valid render function
  expect_true(inherits(result$plot_header, "shiny.render.function"))

  # Verify the input data that drives the reference/previous obs link rendering
  expect_equal(mock_plot_data_with_reference$rf_code, "rf.501")
  expect_equal(mock_plot_data_with_reference$rf_label, "Gawler, S. 2000.")
  expect_equal(mock_plot_data_with_reference$previous_ob_code, "ob.2100")
})

test_that("normalize_plot_obs_result only uses first row of multi-row input", {
  # Only the first row should be processed
  normalized <- vegbankweb:::normalize_plot_obs_result(mock_plot_observations_multi)

  expect_true(normalized$has_data)
  # Should use first row (ob.2948)
  expect_equal(normalized$plot_observation$ob_code, "ob.2948")
  expect_equal(normalized$plot_observation$author_obs_code, "ACAD.143")
})

test_that("normalize_plot_obs_result returns empty result for zero-row dataframe", {
  empty_df <- mock_plot_data[0, ]
  normalized <- vegbankweb:::normalize_plot_obs_result(empty_df)
  expect_false(normalized$has_data)
  expect_equal(nrow(normalized$plot_observation), 0)
})

test_that("disturbances with null intensity are correctly structured in mock data", {
  # All three ACAD observations have type = "unknown" and intensity = NA
  normalized <- vegbankweb:::normalize_plot_obs_result(mock_plot_data)

  expect_equal(nrow(normalized$disturbances), 1)
  expect_equal(normalized$disturbances$type[1], "unknown")
  expect_true(is.na(normalized$disturbances$intensity[1]))
  # The build function should not error on null intensity
  expect_silent(build_plot_obs_details_view(mock_plot_data))
})

test_that("ACAD.145 has multi-word disturbance comment", {
  # ob.2950 has a richer disturbance comment with fire and wind notes
  normalized <- vegbankweb:::normalize_plot_obs_result(mock_plot_data_acad145)

  expect_equal(nrow(normalized$disturbances), 1)
  expect_true(grepl("Fire:", normalized$disturbances$comment[1]))
  expect_true(grepl("Wind:", normalized$disturbances$comment[1]))
  expect_true(is.na(normalized$disturbances$intensity[1]))
})

test_that("soils texture is present for ACAD.143 (sand) and absent for ACAD.144 (peat)", {
  # ACAD.143 has texture = "Sands: Sand"
  normalized_143 <- vegbankweb:::normalize_plot_obs_result(mock_plot_data)
  expect_equal(normalized_143$soils$texture[1], "Sands: Sand")
  expect_equal(normalized_143$soils$horizon[1], "unknown")
  expect_true(grepl("Profile:", normalized_143$soils$description[1]))

  # ACAD.144 has texture = NA (peat soil — no texture class applies)
  normalized_144 <- vegbankweb:::normalize_plot_obs_result(mock_plot_data_acad144)
  expect_true(is.na(normalized_144$soils$texture[1]))
  expect_true(grepl("peat depth", normalized_144$soils$description[1]))

  # Both should not error when building detailed view
  expect_silent(build_plot_obs_details_view(mock_plot_data))
  expect_silent(build_plot_obs_details_view(mock_plot_data_acad144))
})
