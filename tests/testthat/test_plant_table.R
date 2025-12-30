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
  plant_data <- data.frame(
    pc_code = c("pc.101", "pc.102"),
    plant_name = c("Oak", "Maple"),
    plant_level = c("Species", NA),
    current_accepted = c(TRUE, FALSE),
    concept_rf_code = c("rf.9", ""),
    concept_rf_label = c("Oak Ref", "Not provided"),
    obs_count = c("15", "7"),
    plant_description = c("Deciduous tree", "Deciduous tree"),
    stringsAsFactors = FALSE
  )

  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(plant_data = plant_data), concept_type = "plant")

    expect_equal(colnames(result), c(
      "Actions", "Vegbank Code", "Plant Concept", "Status",
      "Level", "Reference Source", "Observations", "Description"
    ))

    expect_true(all(grepl("<button", result$Actions)))
    expect_equal(result$`Vegbank Code`, plant_data$pc_code)
    expect_equal(result$`Plant Concept`, c("Oak", "Maple"))
    expect_equal(result$Status, plant_data$current_accepted)
    expect_equal(result$Level, c("Species", "Not provided"))
    expect_true(grepl("<a ", result$`Reference Source`[1]))
    expect_equal(result$`Reference Source`[2], "Not provided")
    expect_equal(result$Observations, c(15, 7))
    expect_equal(result$Description, plant_data$plant_description)
  })
})
