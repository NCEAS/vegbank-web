test_that("build_concept_table configures datatable with hidden sort columns", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(data_sources, required_sources, process_function, table_config) {
      expect_equal(length(data_sources), 0)
      expect_length(required_sources, 0)
      expect_null(process_function)

      expect_equal(length(table_config$column_defs), 9)

      status_col <- table_config$column_defs[[3]]
      expect_equal(status_col$orderData, 3)

      status_sort_col <- table_config$column_defs[[4]]
      expect_false(status_sort_col$visible)
      expect_false(status_sort_col$searchable)

      level_col <- table_config$column_defs[[5]]
      expect_equal(level_col$targets, 4)

      ref_col <- table_config$column_defs[[6]]
      expect_equal(ref_col$orderData, 6)

      ref_sort_col <- table_config$column_defs[[7]]
      expect_false(ref_sort_col$visible)
      expect_false(ref_sort_col$searchable)

      expect_false(is.null(table_config$initial_data))
      expect_s3_class(table_config$initial_data, "data.frame")
      expect_true(is.function(table_config$ajax))

      structure(list(options = list()), class = "datatables")
    },
    .env = pkg_env,
    {
      with_mock_shiny_notifications({
        result <- build_concept_table(concept_type = "plant")
        expect_s3_class(result, "datatables")
      })
    }
  )
})

plant_test_data <- data.frame(
  pc_code = c("pc.101", "pc.102"),
  plant_name = c("Oak", NA),
  plant_level = c("Species", NA),
  current_accepted = c(TRUE, NA),
  concept_rf_code = c("rf.9", ""),
  concept_rf_name = c("Oak Ref", NA),
  obs_count = c("15", ""),
  plant_description = c("Deciduous tree", NA),
  stringsAsFactors = FALSE
)

community_test_data <- data.frame(
  cc_code = c("cc.201", "cc.202"),
  comm_name = c("Prairie", NA),
  comm_level = c("Alliance", NA),
  current_accepted = c(FALSE, NA),
  concept_rf_code = c("cr.5", NA),
  concept_rf_name = c("Prairie Ref", ""),
  obs_count = c("8", ""),
  comm_description = c("Grassland", NA),
  stringsAsFactors = FALSE
)

test_that("process_concept_data formats plant concepts", {
  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(plant_data = plant_test_data), concept_type = "plant")

    expect_equal(colnames(result), c(
      "Actions", "Plant Name", "Status", "status_sort",
      "Level", "Reference Source", "ref_sort", "Observations", "Description"
    ))

    expect_equal(result$Actions, plant_test_data$pc_code)
    expect_equal(result$`Plant Name`, c("Oak", "Not provided"))
    expect_equal(result$Status, plant_test_data$current_accepted)
    expect_equal(result$status_sort, c(0, 2))
    expect_equal(result$Level, c("Species", "Not provided"))
    expect_equal(result$`Reference Source`, c("rf.9", ""))
    expect_equal(result$ref_sort, c("Oak Ref", "Not provided"))
    expect_equal(result$Observations, c(15, 0))
    expect_equal(result$Description, c("Deciduous tree", "Not provided"))
  })
})

test_that("process_concept_data formats community concepts", {
  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(community_data = community_test_data),
                                                concept_type = "community")

    expect_equal(colnames(result), c(
      "Actions", "Community Name", "Status", "status_sort",
      "Level", "Reference Source", "ref_sort", "Observations", "Description"
    ))

    expect_equal(result$Actions, community_test_data$cc_code)
    expect_equal(result$`Community Name`, c("Prairie", "Not provided"))
    expect_equal(result$Status, community_test_data$current_accepted)
    expect_equal(result$status_sort, c(1, 2))
    expect_equal(result$Level, c("Alliance", "Not provided"))
    expect_equal(result$`Reference Source`, c("cr.5", NA_character_))
    expect_equal(result$ref_sort, c("Prairie Ref", "Not provided"))
    expect_equal(result$Observations, c(8, 0))
    expect_equal(result$Description, c("Grassland", "Not provided"))
  })
})
