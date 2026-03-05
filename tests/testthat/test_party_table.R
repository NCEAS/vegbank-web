test_that("build_party_table configures remote datatable", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
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
  # Bind scalar columns from all three real party mocks
  cols <- c("py_code", "party_label", "organization_name", "contact_instructions", "obs_count")
  test_data <- rbind(
    mock_party_py415[, cols],
    mock_party_py17[, cols],
    mock_party_py199146[, cols]
  )

  result <- process_party_data(test_data)

  expect_equal(colnames(result), c("Actions", "Vegbank Code", "Party", "Organization", "Contributions", "Contact"))
  expect_true(all(grepl("<button", result$Actions)))
  expect_equal(
    result$`Vegbank Code`,
    vapply(c("py.415", "py.17", "py.199146"), htmltools::htmlEscape, character(1), USE.NAMES = FALSE)
  )
  # Party labels
  expect_equal(result$Party, c("Jennings, Michael", "Drake, Jim", "Heaney, Mike"))
  # Organizations: py.415 has one, py.17 has one, py.199146 is NA → "Unspecified"
  expect_equal(result$Organization, c(
    "University of Idaho, Department of Geography",
    "The Nature Conservancy",
    "Unspecified"
  ))
  # Contributions: py.415=8657 (link), py.17=290 (link), py.199146=NA→0 (span)
  expect_true(grepl(">8657</a>", result$Contributions[1]))
  expect_true(grepl("obs-count-link", result$Contributions[1]))
  expect_true(grepl(">290</a>", result$Contributions[2]))
  expect_true(grepl("obs-count-link", result$Contributions[2]))
  expect_equal(result$Contributions[3], "0")
  # Contact: py.415=NA→Unspecified, py.17=capitalized, py.199146=NA→Unspecified
  expect_equal(result$Contact[1], "Unspecified")
  expect_equal(result$Contact[2], "Contact the contributor at specified email:")
  expect_equal(result$Contact[3], "Unspecified")
})
