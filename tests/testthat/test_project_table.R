test_that("build_project_table configures a remote datatable", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
      expect_true(is.list(table_config))
      structure(list(options = list()), class = "datatables")
    },
    .env = pkg_env,
    {
      result <- build_project_table()
      expect_s3_class(result, "datatables")
    }
  )
})

test_that("project table spec wires AJAX data source", {
  config <- build_table_config_from_spec(vegbankweb:::PROJECT_TABLE_SPEC)

  expect_true(is.list(config))
  expect_equal(length(config$column_defs), 8)
  expect_equal(config$column_defs[[1]]$targets, 0)
  expect_s3_class(config$initial_data, "data.frame")
  expect_equal(nrow(config$initial_data), 0)
  expect_equal(colnames(config$initial_data), c("Actions", "VegBank Code", "Project", "Plots", "Started", "Ended", "Last Plot Added", "Description"))
  expect_true(is.function(config$ajax))
  ajax_env <- environment(config$ajax)
  spec <- ajax_env$data_source_spec
  expect_equal(spec$table_id, "proj_table")
  expect_equal(spec$resource, "projects")
  expect_equal(spec$detail, "full")
  expect_false(spec$clean_names)
  expect_true(is.function(spec$normalize_fn))
  expect_true(is.function(spec$display_fn))
  expect_equal(colnames(spec$empty_factory()), PROJECT_TABLE_FIELDS)
  expect_match(config$options$language$processing, "projects", ignore.case = TRUE)
})

test_that("process_project_data formats normalized data", {
  cols <- c("pj_code", "project_name", "project_description", "obs_count",
            "start_date", "stop_date", "last_plot_added_date")
  test_data <- rbind(
    mock_project_pj339[, cols],
    mock_project_pj10559[, cols],
    mock_project_pj11008[, cols]
  )

  result <- process_project_data(test_data)

  expect_equal(nrow(result), 3)
  expect_true(all(grepl("<button", result$Actions)))
  expect_equal(
    result$`VegBank Code`,
    vapply(c("pj.339", "pj.10559", "pj.11008"), htmltools::htmlEscape, character(1), USE.NAMES = FALSE)
  )
  # Project names (clean_column_data capitalises first letter)
  expect_equal(result$Project[1], "Composition and function of vegetation alliances in the Interior Northwest, USA")
  expect_equal(result$Project[2], "Ecological Site Descriptions")
  expect_equal(result$Project[3], "Carolina Vegetation Survey (121): Pulse 2010B = Mountain lacunae")
  # Plots (obs_count links)
  expect_true(grepl(">12962</a>", result$Plots[1]))
  expect_true(grepl("obs-count-link", result$Plots[1]))
  expect_true(grepl(">24</a>",    result$Plots[2]))
  expect_true(grepl("obs-count-link", result$Plots[2]))
  expect_true(grepl(">48</a>",    result$Plots[3]))
  expect_true(grepl("obs-count-link", result$Plots[3]))
  # Dates — pj.339: no start/stop; pj.10559: start only; pj.11008: no start/stop
  expect_equal(result$Started,          c("Unspecified", "2013-05-01", "Unspecified"))
  expect_equal(result$Ended,            c("Unspecified", "Unspecified", "Unspecified"))
  expect_equal(result$`Last Plot Added`, c("2004-11-19",  "2015-03-17", "2015-12-11"))
  # Descriptions
  expect_true(grepl("Improved policy and management",             result$Description[1]))
  expect_true(grepl('data-value="pj.339"',                        result$Description[1]))
  expect_true(grepl("USDA Natural Resource Conservation Service", result$Description[2]))
  expect_true(grepl('data-value="pj.10559"',                      result$Description[2]))
  expect_true(grepl("Unspecified",                                result$Description[3]))
  expect_true(grepl('data-value="pj.11008"',                      result$Description[3]))
})

test_that("process_project_data handles empty input", {
  result <- process_project_data(vegbankweb:::PROJECT_TABLE_SCHEMA_TEMPLATE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("Actions", "VegBank Code", "Project", "Plots", "Started", "Ended", "Last Plot Added", "Description"))
})
