test_that("build_party_table configures remote datatable", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(data_sources, required_sources, process_function, table_config) {
      expect_equal(length(data_sources), 0)
      expect_equal(length(required_sources), 0)
      expect_null(process_function)

      expect_equal(length(table_config$column_defs), 6)
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
    given_name = c("Norm", NA),
    surname = c("Aaseng", ""),
    organization_name = c("MN DNR", NA),
    contact_instructions = c("email@example.com", NA),
    obs_count = c(191, NA_integer_)
  )

  result <- process_party_data(test_data)

  expect_equal(colnames(result), c("Actions", "Given Name", "Surname", "Organization", "Contact", "Observations"))
  expect_equal(result$Actions, c("py.1", "py.2"))
  expect_equal(result$`Given Name`, c("Norm", "Not provided"))
  expect_equal(result$Surname, c("Aaseng", "Not provided"))
  expect_equal(result$Organization, c("MN DNR", "Not provided"))
  expect_equal(result$Contact, c("Email@example.com", "Not provided"))
  expect_equal(result$Observations, c(191L, 0L))
})

test_that("normalize_party_data enforces schema", {
  raw <- list(py_code = "py.9")

  normalized <- normalize_party_data(raw)

  expect_equal(colnames(normalized), vegbankweb:::PARTY_TABLE_FIELDS)
  expect_equal(normalized$py_code, "py.9")
  expect_equal(normalized$obs_count, 0L)
  expect_type(normalized$given_name, "character")
})
