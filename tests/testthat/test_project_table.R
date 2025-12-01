test_that("build_project_table configures a remote datatable", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(data_sources, required_sources, process_function, table_config) {
      expect_length(data_sources, 0)
      expect_length(required_sources, 0)
      expect_null(process_function)
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

test_that("create_project_table_config wires AJAX data source", {
  config <- create_project_table_config()

  expect_true(is.list(config))
  expect_equal(length(config$column_defs), 7)
  expect_equal(config$column_defs[[1]]$targets, 0)
  expect_equal(
    as.character(config$column_defs[[1]]$render),
    as.character(create_action_button_renderer("proj_link_click", "Details"))
  )

  expect_s3_class(config$initial_data, "data.frame")
  expect_equal(nrow(config$initial_data), 0)
  expect_equal(colnames(config$initial_data), c("Actions", "Name", "Plots", "Started", "Ended", "Last Plot Added", "Description"))

  expect_true(is.function(config$ajax))
  ajax_env <- environment(config$ajax)
  spec <- ajax_env$data_source_spec

  expect_equal(spec$table_id, "proj_table")
  expect_equal(spec$endpoint, "projects")
  expect_equal(spec$detail, "full")
  expect_false(spec$clean_names)
  expect_true(is.function(spec$normalize_fn))
  expect_true(is.function(spec$display_fn))
  expect_equal(colnames(spec$empty_factory()), PROJECT_TABLE_FIELDS)

  expect_match(config$options$language$processing, "project records", ignore.case = TRUE)
})

test_that("process_project_data formats normalized data", {
  test_data <- data.frame(
    pj_code = c("PJ.1", "PJ.2"),
    project_name = c("alpha", "Beta"),
    project_description = c(NA, "second"),
    obs_count = c(NA, 12),
    start_date = c("", "2022-01-01"),
    stop_date = c("2020-12-31", NA),
    last_plot_added_date = c("2021-05-01", "2022-02-02"),
    stringsAsFactors = FALSE
  )

  result <- process_project_data(test_data)

  expect_equal(nrow(result), 2)
  expect_equal(result$Actions, c("PJ.1", "PJ.2"))
  expect_equal(result$Name, c("Alpha", "Beta"))
  expect_equal(result$Description, c("Not provided", "Second"))
  expect_equal(result$Plots, c(0L, 12L))
  expect_equal(result$Started, c("Not provided", "2022-01-01"))
  expect_equal(result$Ended, c("2020-12-31", "Not provided"))
})

test_that("process_project_data handles empty input", {
  result <- process_project_data(create_empty_project_df())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("Actions", "Name", "Plots", "Started", "Ended", "Last Plot Added", "Description"))
})

test_that("normalize_project_data enforces schema", {
  raw <- data.frame(
    project_name = "Test",
    obs_count = "5",
    extra_col = "ignore",
    stringsAsFactors = FALSE
  )

  normalized <- normalize_project_data(raw)

  expect_equal(colnames(normalized), PROJECT_TABLE_FIELDS)
  expect_equal(normalized$project_name, "Test")
  expect_equal(normalized$obs_count, 5L)
  expect_true(is.na(normalized$project_description))
})

test_that("coerce_project_page flattens nested responses", {
  nested <- list(list(data = list(
    pj_code = "PJ.1",
    project_name = "Nested"
  )))

  result <- coerce_project_page(nested)
  expect_s3_class(result, "data.frame")
  expect_equal(result$project_name, "Nested")
})
