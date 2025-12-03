test_that("build_community_table delegates to concept builder with community config", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
      expect_equal(length(table_config$column_defs), 9)
      expect_equal(table_config$column_defs[[2]]$targets, 1)
      expect_equal(table_config$column_defs[[2]]$width, "25%")
      expect_equal(table_config$column_defs[[3]]$targets, 2)
      expect_equal(table_config$column_defs[[4]]$visible, FALSE)
      expect_equal(table_config$column_defs[[7]]$visible, FALSE)
      expect_true(is.function(table_config$ajax))
      structure(list(options = list()), class = "datatables")
    },
    .env = pkg_env,
    {
      with_mock_shiny_notifications({
        result <- build_community_table()
        expect_s3_class(result, "datatables")
      })
    }
  )
})

test_that("community concept data includes community-specific values", {
  community_data <- data.frame(
    cc_code = c("cc.101", "cc.102"),
    comm_name = c("Prairie", "Wetland"),
    comm_level = c("Alliance", NA),
    current_accepted = c(FALSE, NA),
    concept_rf_code = c("cr.5", ""),
    concept_rf_name = c("Prairie Ref", "Not provided"),
    obs_count = c("8", "0"),
    comm_description = c("Grassland", "Marsh"),
    stringsAsFactors = FALSE
  )

  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(community_data = community_data),
                                                concept_type = "community")

    expect_equal(colnames(result), c(
      "Actions", "Community Name", "Status", "status_sort",
      "Level", "Reference Source", "ref_sort", "Observations", "Description"
    ))

    expect_equal(result$Actions, community_data$cc_code)
    expect_equal(result$`Community Name`, community_data$comm_name)
    expect_equal(result$Status, community_data$current_accepted)
    expect_equal(result$status_sort, c(1, 2))
    expect_equal(result$Level, c("Alliance", "Not provided"))
    expect_equal(result$`Reference Source`, community_data$concept_rf_code)
    expect_equal(result$ref_sort, community_data$concept_rf_name)
    expect_equal(result$Observations, c(8, 0))
    expect_equal(result$Description, community_data$comm_description)
  })
})
