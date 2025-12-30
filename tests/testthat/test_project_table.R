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
  expect_equal(colnames(config$initial_data), c("Actions", "Vegbank Code", "Project", "Plots", "Started", "Ended", "Last Plot Added", "Description"))
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
  expect_match(config$options$language$processing, "project records", ignore.case = TRUE)
})

test_that("process_project_data formats normalized data", {
  test_data <- data.frame(
    pj_code = c("pj.1", "pj.2", "pj.314"),
    project_name = c("alpha", "Beta", "Walker et al. 2010"),
    project_description = c(NA, "second", "Angelina National Forest.  Sampling work conducted between 1998 and 2002 supported the application of the United States National Vegetation Classification (USNVC) standard to Region 8 Forests. This resulted in a basic list of vegetation units (alliances and community associations) for the National Forests in this region.\nThe vegetation classification produced through this agreement, and the NatureServe Ecological Systems Classification which is based on it, form the foundation for continuing use of the USNVC on U.S. Forest Service lands in Region 8 for natural resource planning and management.\nReports on the US National Vegetation Classification for various National Forests or groups of forests are found at http://www.natureserve.org/publications/library.jsp#techrpts"),
    obs_count = c(NA, 12, 149),
    start_date = c("", "Thu, 21 Aug 1997 07:00:00 GMT", "2022-01-01"),
    stop_date = c("2020-12-31", NA, "Sat, 23 Aug 1997 07:00:00 GMT"),
    last_plot_added_date = c("2021-05-01", "2022-02-02", NA),
    stringsAsFactors = FALSE
  )

  result <- process_project_data(test_data)

  expect_equal(nrow(result), 3)
  expect_true(all(grepl("<button", result$Actions)))
  expect_equal(result$`Vegbank Code`, c("pj.1", "pj.2", "pj.314"))
  expect_equal(result$Project, c("Alpha", "Beta", "Walker et al. 2010"))
  expect_equal(result$Description, c("Not provided", "Second", "Angelina National Forest.  Sampling work conducted between 1998 and 2002 supported the application of the United States National Vegetation Classification (USNVC) standard to Region 8 Forests. This resulted in a basic list of vegetation units (alliances and community associations) for the National Forests in this region.\nThe vegetation classification produced through this agreement, and the NatureServe Ecological Systems Classification which is based on it, form the foundation for continuing use of the USNVC on U.S. Forest Service lands in Region 8 for natural resource planning and management.\nReports on the US National Vegetation Classification for various National Fores..."))
  expect_equal(result$Plots, c(0L, 12L, 149L))
  expect_equal(result$Started, c("Not provided", "1997-08-21", "2022-01-01"))
  expect_equal(result$Ended, c("2020-12-31", "Not provided", "1997-08-23"))
  expect_equal(result$`Last Plot Added`, c("2021-05-01", "2022-02-02", "Not provided"))
})

test_that("process_project_data handles empty input", {
  result <- process_project_data(vegbankweb:::PROJECT_TABLE_SCHEMA_TEMPLATE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("Actions", "Vegbank Code", "Project", "Plots", "Started", "Ended", "Last Plot Added", "Description"))
})
