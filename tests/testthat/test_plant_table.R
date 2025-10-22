test_that("build_plant_table delegates to concept builder with plant config", {
  sample_data <- data.frame(
    pc_code = "pc.001",
    plant_name = "Sample Plant",
    stringsAsFactors = FALSE
  )

  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(data_sources, required_sources, process_function, table_config) {
      expect_equal(names(data_sources), "plant_data")
      expect_equal(data_sources$plant_data, sample_data)
      expect_equal(required_sources, "plant_data")
      expect_equal(length(table_config$column_defs), 8)
      expect_equal(table_config$column_defs[[2]]$targets, 1)
      expect_equal(table_config$column_defs[[2]]$width, "25%")
      expect_equal(table_config$column_defs[[3]]$targets, 2)
      expect_equal(table_config$column_defs[[4]]$visible, FALSE)
      expect_equal(table_config$column_defs[[6]]$visible, FALSE)
      structure(list(options = list()), class = "datatables")
    },
    .env = pkg_env,
    {
      with_mock_shiny_notifications({
        result <- build_plant_table(sample_data)
        expect_s3_class(result, "datatables")
      })
    }
  )
})

test_that("plant concept data includes plant-specific values", {
  plant_data <- data.frame(
    pc_code = c("pc.101", "pc.102"),
    plant_name = c("Oak", "Maple"),
    current_accepted = c(TRUE, FALSE),
    concept_rf_code = c("rf.9", ""),
    concept_rf_name = c("Oak Ref", "Not Provided"),
    obs_count = c("15", "7"),
    plant_description = c("Deciduous tree", "Deciduous tree"),
    stringsAsFactors = FALSE
  )

  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(plant_data = plant_data), concept_type = "plant")

    expect_equal(colnames(result), c(
      "Actions", "Plant Name", "Status", "status_sort",
      "Reference Source", "ref_sort", "Observations", "Description"
    ))

    expect_equal(result$Actions, plant_data$pc_code)
    expect_equal(result$`Plant Name`, plant_data$plant_name)
    expect_equal(result$Status, plant_data$current_accepted)
    expect_equal(result$status_sort, c(0, 1))
    expect_equal(result$`Reference Source`, plant_data$concept_rf_code)
    expect_equal(result$ref_sort, plant_data$concept_rf_name)
    expect_equal(result$Observations, c(15, 7))
    expect_equal(result$Description, plant_data$plant_description)
  })
})
