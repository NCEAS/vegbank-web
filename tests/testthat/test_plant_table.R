test_that("build_plant_table delegates to concept builder with plant config", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
      expect_equal(length(table_config$column_defs), 8)
      expect_equal(table_config$column_defs[[2]]$targets, 1) # Vegbank Code
      expect_equal(table_config$column_defs[[2]]$width, "12%")
      expect_equal(table_config$column_defs[[3]]$targets, 2) # Name
      expect_true(is.function(table_config$ajax))
      structure(list(options = list()), class = "datatables")
    },
    .env = pkg_env,
    {
      with_mock_shiny_notifications({
        result <- build_plant_table()
        expect_s3_class(result, "datatables")
      })
    }
  )
})

test_that("plant concept data includes plant-specific values", {
  cols <- c("pc_code", "plant_name", "plant_level", "current_accepted",
            "concept_rf_code", "concept_rf_label", "obs_count", "plant_description")
  plant_data <- rbind(
    mock_plant_concept_acru[, cols],
    mock_plant_concept_vaccinium[, cols],
    mock_plant_concept_psme[, cols]
  )

  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(plant_data = plant_data), concept_type = "plant")

    expect_equal(colnames(result), c(
      "Actions", "Vegbank Code", "Plant Concept", "Status",
      "Level", "Reference Source", "Plots", "Description"
    ))

    expect_true(all(grepl("<button", result$Actions)))
    expect_equal(
      result$`Vegbank Code`,
      vapply(c("pc.111478", "pc.389660", "pc.47659"), htmltools::htmlEscape, character(1), USE.NAMES = FALSE)
    )
    # Plant names
    expect_equal(result$`Plant Concept`[1], "Acer rubrum L.")
    expect_true(grepl("Vaccinium stamineum", result$`Plant Concept`[2]))
    expect_true(grepl("Pseudotsuga menziesii", result$`Plant Concept`[3]))
    # Status: ACRU=Accepted (TRUE), Vaccinium=No Status (NA), PSME=Not Current (FALSE)
    expect_true(grepl("Accepted",   result$Status[1]))
    expect_true(grepl("No Status",  result$Status[2]))
    expect_true(grepl("Not Current", result$Status[3]))
    # Level
    expect_equal(result$Level, c("Species", "Unspecified", "Species"))
    # Reference source: all three have valid concept_rf_code → all links
    expect_true(all(grepl("<a ", result$`Reference Source`)))
    # Observations: ACRU=0→"0", Vaccinium=828→link, PSME=7475→link
    expect_equal(result$Plots[1], "0")
    expect_true(grepl(">828</a>",  result$Plots[2]))
    expect_true(grepl("obs-count-link", result$Plots[2]))
    expect_true(grepl(">7475</a>", result$Plots[3]))
    expect_true(grepl("obs-count-link", result$Plots[3]))
    # Descriptions: all plant_description=NA → "Unspecified" in container
    expect_true(grepl("Unspecified", result$Description[1]) && grepl('data-value="pc.111478"', result$Description[1]))
    expect_true(grepl("Unspecified", result$Description[2]) && grepl('data-value="pc.389660"', result$Description[2]))
    expect_true(grepl("Unspecified", result$Description[3]) && grepl('data-value="pc.47659"',  result$Description[3]))
  })
})
