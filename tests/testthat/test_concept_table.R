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

# Real API data: ACRU (accepted, 0 obs), Vaccinium (no status, 828 obs), PSME (not current, 7475 obs)
.pc_fields <- c("pc_code", "plant_name", "current_accepted", "plant_level",
                "concept_rf_code", "concept_rf_label", "obs_count", "plant_description")
plant_test_data <- rbind(
  mock_plant_concept_acru[, .pc_fields],
  mock_plant_concept_vaccinium[, .pc_fields],
  mock_plant_concept_psme[, .pc_fields]
)
rm(.pc_fields)

# Real API data: CEGL007230 (accepted, 294 obs, has description),
#                Brachypodium (not current/undetermined, 17 obs, no description),
#                VII (not current, 0 obs, no description)
.cc_fields <- c("cc_code", "comm_name", "current_accepted", "comm_level",
                "concept_rf_code", "concept_rf_label", "obs_count", "comm_description")
community_test_data <- rbind(
  mock_comm_concept_cegl007230[, .cc_fields],
  mock_comm_concept_brachypodium[, .cc_fields],
  mock_comm_concept_vii[, .cc_fields]
)
rm(.cc_fields)

test_that("process_concept_data formats plant concepts", {
  with_mock_shiny_notifications({
    result <- vegbankweb:::process_concept_data(list(plant_data = plant_test_data), concept_type = "plant")

    expect_equal(colnames(result), c(
      "Actions", "Vegbank Code", "Plant Concept", "Status",
      "Level", "Reference Source", "Observations", "Description"
    ))

    expect_true(all(grepl("<button", result$Actions)))
    expect_equal(result$`Vegbank Code`, vapply(plant_test_data$pc_code, htmltools::htmlEscape, character(1), USE.NAMES = FALSE))
    expect_equal(result$`Plant Concept`,
      c("Acer rubrum L.", "Vaccinium stamineum", "Pseudotsuga menziesii (Mirbel) Franco"))
    expect_true(grepl("Accepted",    result$Status[1]))  # ACRU: current_accepted=TRUE
    expect_true(grepl("No Status",   result$Status[2]))  # Vaccinium: current_accepted=NA
    expect_true(grepl("Not Current", result$Status[3]))  # PSME: current_accepted=FALSE
    expect_equal(result$Level, c("Species", "Unspecified", "Species"))
    expect_true(all(grepl("<a ", result$`Reference Source`)))  # all three have valid ref codes
    expect_equal(result$Observations[1], "0")                  # ACRU: obs_count=0 (no link)
    expect_true(grepl(">828</a>",  result$Observations[2]))    # Vaccinium: 828
    expect_true(grepl("obs-count-link", result$Observations[2]))
    expect_true(grepl(">7475</a>", result$Observations[3]))    # PSME: 7475
    expect_true(grepl("obs-count-link", result$Observations[3]))
    # All three have NA descriptions → all show "Unspecified" with data-value attribute
    expect_true(grepl("Unspecified", result$Description[1]) && grepl('data-value="pc.111478"', result$Description[1]))
    expect_true(grepl("Unspecified", result$Description[2]) && grepl('data-value="pc.389660"', result$Description[2]))
    expect_true(grepl("Unspecified", result$Description[3]) && grepl('data-value="pc.47659"',  result$Description[3]))
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
    expect_equal(result$`Community Concept`, c(
      "Quercus alba - Quercus (rubra, montana) / Rhododendron calendulaceum - (Gaylussacia ursina) Forest",
      "Brachypodium distachyon \u2013 Bromus diandrus / Quercus douglasii Semi-natural Association",
      "VII"
    ))
    expect_true(grepl("Accepted",    result$Status[1]))  # CEGL007230: current_accepted=TRUE
    expect_true(grepl("Not Current", result$Status[2]))  # Brachypodium: current_accepted=FALSE
    expect_true(grepl("Not Current", result$Status[3]))  # VII: current_accepted=FALSE
    expect_equal(result$Level, c("Association", "Association", "Class"))
    expect_true(all(grepl("<a ", result$`Reference Source`)))  # all three have valid ref codes
    expect_true(grepl(">294</a>",  result$Observations[1]))   # CEGL007230: 294
    expect_true(grepl("obs-count-link", result$Observations[1]))
    expect_true(grepl(">17</a>",   result$Observations[2]))   # Brachypodium: 17
    expect_true(grepl("obs-count-link", result$Observations[2]))
    expect_equal(result$Observations[3], "0")                  # VII: obs_count=0 (no link)
    # CEGL007230 has description; Brachypodium and VII have NA → "Unspecified"
    expect_true(grepl("These forests occur", result$Description[1]))
    expect_true(grepl('data-value="cc.47882"', result$Description[1]))
    expect_true(grepl("Unspecified", result$Description[2]) && grepl('data-value="cc.38611"', result$Description[2]))
    expect_true(grepl("Unspecified", result$Description[3]) && grepl('data-value="cc.133"',   result$Description[3]))
  })
})
