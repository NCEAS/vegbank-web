test_that("build_party_table configures remote datatable", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
      expect_equal(length(table_config$column_defs), 5)
      expect_true(is.function(table_config$ajax))
      expect_s3_class(table_config$initial_data, "data.frame")

      structure(list(options = list()), class = "datatables")
    },
    .env = pkg_env,
    {
      result <- build_party_table()
      expect_s3_class(result, "datatables")
    }
  )
})

test_that("process_party_data formats normalized rows", {
  test_data <- data.frame(
    py_code = c("py.1", "py.2"),
    party_label = c("Norm Aaseng", NA),
    organization_name = c("MN DNR", NA),
    contact_instructions = c("email@example.com", NA),
    obs_count = c(191, NA_integer_)
  )

  result <- process_party_data(test_data)

  expect_equal(colnames(result), c("Actions", "Vegbank Code", "Party", "party_sort", "Organization", "Observations", "Contact"))
  expect_equal(result$Actions, c("py.1", "py.2"))
  expect_equal(result$`Vegbank Code`, c("py.1", "py.2"))
  expect_equal(result$Party, c("Norm Aaseng", "Not provided"))
  expect_equal(result$party_sort, c("Aaseng", ""))
  expect_equal(result$Organization, c("MN DNR", "Not provided"))
  expect_equal(result$Observations, c(191L, 0L))
  expect_equal(result$Contact, c("Email@example.com", "Not provided"))
})
