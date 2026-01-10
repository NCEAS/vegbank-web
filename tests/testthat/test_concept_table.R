test_that("build_concept_table configures datatable with hidden sort columns", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
      expect_equal(length(table_config$column_defs), 8)

      # Vegbank Code column
      vegbank_col <- table_config$column_defs[[2]]
      expect_equal(vegbank_col$targets, 1)
      expect_equal(vegbank_col$width, "12%")

      # Name column
      name_col <- table_config$column_defs[[3]]
      expect_equal(name_col$targets, 2)
      expect_equal(name_col$width, "23%")

      # Status column
      status_col <- table_config$column_defs[[4]]
      expect_equal(status_col$targets, 3)
      expect_equal(status_col$className, "dt-center")

      # Level column
      level_col <- table_config$column_defs[[5]]
      expect_equal(level_col$targets, 4)

      # Reference Source column
      ref_col <- table_config$column_defs[[6]]
      expect_equal(ref_col$targets, 5)

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
  concept_rf_label = c("Oak Ref", NA),
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
  concept_rf_label = c("Prairie Ref", ""),
  obs_count = c("8", ""),
  comm_description = c("Grassland", NA),
  stringsAsFactors = FALSE
)

test_that("process_concept_data formats plant concepts", {
  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(plant_data = plant_test_data), concept_type = "plant")

    expect_equal(colnames(result), c(
      "Actions", "Vegbank Code", "Plant Concept", "Status",
      "Level", "Reference Source", "Observations", "Description"
    ))

    expect_true(all(grepl("<button", result$Actions)))
    expect_equal(result$`Vegbank Code`, vapply(plant_test_data$pc_code, htmltools::htmlEscape, character(1), USE.NAMES = FALSE))
    expect_equal(result$`Plant Concept`, c("Oak", "Not provided"))
    expect_true(grepl("Accepted", result$Status[1]))
    expect_true(grepl("No Status", result$Status[2]))
    expect_equal(result$Level, c("Species", "Not provided"))
    expect_true(grepl("<a ", result$`Reference Source`[1]))
    expect_equal(result$`Reference Source`[2], "Not provided")
    expect_true(grepl(">15</a>", result$Observations[1]))
    expect_true(grepl("obs-count-link", result$Observations[1]))
    expect_equal(result$Observations[2], "0")
    expect_equal(result$Description, c("Deciduous tree", "Not provided"))
  })
})

test_that("process_concept_data formats community concepts", {
  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(community_data = community_test_data),
                                                concept_type = "community")

    expect_equal(colnames(result), c(
      "Actions", "Vegbank Code", "Community Concept", "Status",
      "Level", "Reference Source", "Observations", "Description"
    ))

    expect_true(all(grepl("<button", result$Actions)))
    expect_equal(result$`Vegbank Code`, vapply(community_test_data$cc_code, htmltools::htmlEscape, character(1), USE.NAMES = FALSE))
    expect_equal(result$`Community Concept`, c("Prairie", "Not provided"))
    expect_true(grepl("Not Current", result$Status[1]))
    expect_true(grepl("No Status", result$Status[2]))
    expect_equal(result$Level, c("Alliance", "Not provided"))
    expect_true(grepl("<a ", result$`Reference Source`[1]))
    expect_equal(result$`Reference Source`[2], "Not provided")
    expect_true(grepl(">8</a>", result$Observations[1]))
    expect_true(grepl("obs-count-link", result$Observations[1]))
    expect_equal(result$Observations[2], "0")
    expect_equal(result$Description, c("Grassland", "Not provided"))
  })
})
