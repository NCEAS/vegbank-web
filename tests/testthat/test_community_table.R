test_that("build_community_table delegates to concept builder with community config", {
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
        result <- build_community_table()
        expect_s3_class(result, "datatables")
      })
    }
  )
})

test_that("community concept data includes community-specific values", {
  cols <- c("cc_code", "comm_name", "comm_level", "current_accepted",
            "concept_rf_code", "concept_rf_label", "obs_count", "comm_description")
  community_data <- rbind(
    mock_comm_concept_cegl007230[, cols],
    mock_comm_concept_brachypodium[, cols],
    mock_comm_concept_vii[, cols]
  )

  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(community_data = community_data),
                                                concept_type = "community")

    expect_equal(colnames(result), c(
      "Actions", "Vegbank Code", "Community Concept", "Status",
      "Level", "Reference Source", "Plots", "Description"
    ))

    expect_true(all(grepl("<button", result$Actions)))
    expect_equal(
      result$`Vegbank Code`,
      vapply(c("cc.47882", "cc.38611", "cc.133"), htmltools::htmlEscape, character(1), USE.NAMES = FALSE)
    )
    # Community names
    expect_true(grepl("Quercus alba",  result$`Community Concept`[1]))
    expect_true(grepl("Brachypodium",  result$`Community Concept`[2]))
    expect_equal(result$`Community Concept`[3], "VII")
    # Status: CEGL=Accepted (TRUE), Brachypodium=Not Current (FALSE), VII=Not Current (FALSE)
    expect_true(grepl("Accepted",    result$Status[1]))
    expect_true(grepl("Not Current", result$Status[2]))
    expect_true(grepl("Not Current", result$Status[3]))
    # Level
    expect_equal(result$Level, c("Association", "Association", "Class"))
    # Reference source: all three have valid concept_rf_code → all links
    expect_true(all(grepl("<a ", result$`Reference Source`)))
    # Observations: CEGL=294→link, Brachypodium=17→link, VII=0→"0"
    expect_true(grepl(">294</a>", result$Plots[1]))
    expect_true(grepl("obs-count-link", result$Plots[1]))
    expect_true(grepl(">17</a>",  result$Plots[2]))
    expect_true(grepl("obs-count-link", result$Plots[2]))
    expect_equal(result$Plots[3], "0")
    # Descriptions
    expect_true(grepl("forests occur", result$Description[1]) && grepl('data-value="cc.47882"', result$Description[1]))
    expect_true(grepl("Unspecified",   result$Description[2]) && grepl('data-value="cc.38611"', result$Description[2]))
    expect_true(grepl("Unspecified",   result$Description[3]) && grepl('data-value="cc.133"',   result$Description[3]))
  })
})
