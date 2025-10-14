test_that("build_community_table creates a datatable object", {
  test_data <- data.frame(
    cc_code = c("cc.30617", "cc.30618"),
    default_name = c("Test Community 1", "Test Community 2"),
    comm_description = c("Description 1", "Description 2"),
    obs_count = c(10, 20),
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
              expect_equal(names(data_sources), "community_data")
              expect_equal(data_sources$community_data, test_data)
              expect_equal(required_sources, "community_data")
              expect_equal(table_config$column_defs[[1]]$targets, 0)
              expect_equal(table_config$column_defs[[1]]$orderable, FALSE)
              structure(list(options = list()), class = "datatables")
            },
            .env = pkg_env,
            {
              result <- build_community_table(test_data)
              expect_s3_class(result, "datatables")
            }
          )
        }
      )
    }
  )
})

test_that("process_community_data correctly formats community data", {
  test_data <- data.frame(
    cc_code = c("cc.30617", "cc.30618"),
    default_name = c("Test Community 1", "Test Community 2"),
    comm_description = c("Description 1", "Description 2"),
    obs_count = c(10, 20),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    clean_column_data = function(data, column) {
      data[[column]]
    },
    create_action_buttons = function(data, actions) {
      rep("Action HTML", nrow(data))
    }
  )
  with_mock_shiny_notifications({
    result <- process_community_data(list(community_data = test_data))
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 4)
    expect_equal(nrow(result), 2)
    expect_equal(colnames(result), c("Actions", "Name", "Observations", "Description"))
    expect_equal(result$Name, c("Test Community 1", "Test Community 2"))
    expect_equal(result$Observations, c(10, 20))
    expect_equal(result$Description, c("Description 1", "Description 2"))
  })
})

test_that("create_reference_vectors formats references correctly", {
  # Create test data
  community_data <- data.frame(
    comm_concept_id = c(1, 2),
    stringsAsFactors = FALSE
  )

  reference_data <- data.frame(
    comm_concept_id = c(1, 2),
    reference = c("Ref 1", "Ref 2"),
    stringsAsFactors = FALSE
  )

  # Test the function
  result <- create_reference_vectors(community_data, reference_data)

  expect_type(result, "character")
  expect_length(result, 2)
})
