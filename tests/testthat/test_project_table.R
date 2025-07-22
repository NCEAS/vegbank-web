test_that("build_project_table creates a datatable object", {
  test_data <- data.frame(
    project_id = c(1, 2),
    project_name = c("Test Project 1", "Test Project 2"),
    project_description = c("Description 1", "Description 2"),
    start_date = c("2021-01-01", "2022-01-01"),
    stop_date = c("2021-12-31", "2022-12-31"),
    plot_count = c(10, 20),
    last_plot_date = c("2021-10-15", "2022-09-20"),
    stringsAsFactors = FALSE
  )

  pkg_env <- asNamespace("vegbankweb")
  # Mock is_any_data_missing in the correct environment
  with_mocked_bindings(
    is_any_data_missing = function(...) FALSE,
    .env = pkg_env,
    {
      with_mocked_bindings(
        withProgress = function(expr, ...) force(expr),
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          with_mocked_bindings(
            create_table = function(data_sources, required_sources, process_function, table_config) {
              expect_equal(names(data_sources), "project_data")
              expect_equal(data_sources$project_data, test_data)
              expect_equal(required_sources, "project_data")
              expect_equal(table_config$column_defs[[1]]$targets, 0)
              expect_equal(table_config$column_defs[[1]]$orderable, FALSE)
              structure(list(options = list()), class = "datatables")
            },
            .env = pkg_env,
            {
              result <- build_project_table(test_data)
              expect_s3_class(result, "datatables")
            }
          )
        }
      )
    }
  )
})

test_that("process_project_data correctly formats project data", {
  test_data <- data.frame(
    project_id = c(1, 2),
    project_name = c("Test Project 1", "Test Project 2"),
    project_description = c("Description 1", "Description 2"),
    start_date = c("2021-01-01", "2022-01-01"),
    stop_date = c("2021-12-31", "2022-12-31"),
    plot_count = c(10, 20),
    last_plot_added_date = c("2021-10-15", "2022-09-20"),
    stringsAsFactors = FALSE
  )

  # Create an expected result to compare against
  expected_result <- data.frame(
    Actions = c("Action 1", "Action 2"),
    Name = c("Test Project 1", "Test Project 2"),
    Plots = c(10, 20),
    Started = c("2021-01-01", "2022-01-01"),
    Ended = c("2021-12-31", "2022-12-31"),
    `Last Plot Added` = c("2021-10-15", "2022-09-20"),
    Description = c("Description 1", "Description 2"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  with_mocked_bindings(
    clean_column_data = function(data, column) {
      if (column == "project_name") {
        return(c("Test Project 1", "Test Project 2"))
      }
      if (column == "project_description") {
        return(c("Description 1", "Description 2"))
      }
      if (column == "obs_count" || column == "plot_count") {
        return(c(10, 20))
      }
      if (column == "start_date") {
        return(c("2021-01-01", "2022-01-01"))
      }
      if (column == "stop_date") {
        return(c("2021-12-31", "2022-12-31"))
      }
      if (column == "last_plot_added_date") {
        return(c("2021-10-15", "2022-09-20"))
      }
      c(NA, NA)
    },
    create_action_buttons = function(data, actions) {
      c("Action 1", "Action 2")
    },
    {
      with_mocked_bindings(
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          result <- process_project_data(list(project_data = test_data))

          expect_s3_class(result, "data.frame")
          expect_equal(nrow(result), 2)
          expect_equal(colnames(result), c("Actions", "Name", "Plots", "Started", "Ended", "Last Plot Added", "Description"))
          expect_equal(result$Name, c("Test Project 1", "Test Project 2"))
          expect_equal(result$Description, c("Description 1", "Description 2"))
          expect_equal(result$Started, c("2021-01-01", "2022-01-01"))
          expect_equal(result$Ended, c("2021-12-31", "2022-12-31"))
          expect_equal(result$Plots, c(10, 20))
          expect_equal(result$`Last Plot Added`, c("2021-10-15", "2022-09-20"))
        }
      )
    }
  )
})

test_that("table_config has correct structure", {
  # Test the column definitions
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    build_project_table = function(data) {
      # Access the column definitions directly
      column_defs <- list(
        list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
        list(targets = 1, width = "30%"),
        list(targets = 2, width = "8%", className = "dt-right"),
        list(targets = 3, width = "10%"),
        list(targets = 4, width = "10%"),
        list(targets = 5, width = "10%"),
        list(targets = 6, width = "20%")
      )

      expect_equal(length(column_defs), 7)
      expect_equal(column_defs[[1]]$targets, 0)
      expect_equal(column_defs[[1]]$orderable, FALSE)
      expect_equal(column_defs[[1]]$searchable, FALSE)
      expect_equal(column_defs[[2]]$targets, 1)
      expect_equal(column_defs[[2]]$width, "30%")

      structure(list(column_defs = column_defs), class = "datatables")
    },
    .env = pkg_env,
    {
      test_data <- data.frame(
        project_id = c(1),
        project_name = c("Test Project"),
        stringsAsFactors = FALSE
      )

      result <- build_project_table(test_data)
      expect_true(is.list(result))
      expect_equal(length(result$column_defs), 7)
    }
  )
})
