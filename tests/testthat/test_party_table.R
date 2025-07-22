test_that("build_party_table creates a datatable object", {
  test_data <- data.frame(
    party_id = c(1, 2),
    given_name = c("John", "Jane"),
    surname = c("Doe", "Smith"),
    organization_name = c("Organization 1", "Organization 2"),
    contact_instructions = c("Email: john@example.com", "Phone: 555-1234"),
    party_accession_code = c("PARTY001", "PARTY002"),
    stringsAsFactors = FALSE
  )

  pkg_env <- asNamespace("vegbankweb")
  # Mock is_any_data_missing in the correct environment
  with_mocked_bindings(
    is_any_data_missing = function(data_sources, required_sources) FALSE,
    .env = pkg_env,
    {
      with_mocked_bindings(
        withProgress = function(expr, ...) force(expr),
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          with_mocked_bindings(
            create_table = function(data_sources, required_sources, process_function, table_config) {
              expect_equal(names(data_sources), "party_data")
              expect_equal(data_sources$party_data, test_data)
              expect_equal(required_sources, "party_data")
              expect_equal(table_config$column_defs[[1]]$targets, 0)
              expect_equal(table_config$column_defs[[1]]$orderable, FALSE)
              structure(list(options = list()), class = "datatables")
            },
            .env = pkg_env,
            {
              result <- build_party_table(test_data)
              expect_s3_class(result, "datatables")
            }
          )
        }
      )
    }
  )
})

test_that("process_party_data correctly formats party data", {
  test_data <- data.frame(
    party_id = c(1, 2),
    given_name = c("John", "Jane"),
    surname = c("Doe", "Smith"),
    organization_name = c("Organization 1", "Organization 2"),
    contact_instructions = c("Email: john@example.com", "Phone: 555-1234"),
    party_accession_code = c("PARTY001", "PARTY002"),
    stringsAsFactors = FALSE
  )

  # Create expected output format
  with_mocked_bindings(
    clean_column_data = function(data, column) {
      if (column == "given_name") return(c("John", "Jane"))
      if (column == "surname") return(c("Doe", "Smith"))
      if (column == "organization_name") return(c("Organization 1", "Organization 2"))
      if (column == "contact_instructions") return(c("Email: john@example.com", "Phone: 555-1234"))
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
          result <- process_party_data(list(party_data = test_data))
          
          expect_s3_class(result, "data.frame")
          expect_equal(nrow(result), 2)
          expect_equal(colnames(result), c("Actions", "Given Name", "Surname", "Organization", "Contact"))
          expect_equal(result$`Given Name`, c("John", "Jane"))
          expect_equal(result$Surname, c("Doe", "Smith"))
          expect_equal(result$Organization, c("Organization 1", "Organization 2"))
          expect_equal(result$Contact, c("Email: john@example.com", "Phone: 555-1234"))
        }
      )
    }
  )
})

test_that("party_table handles empty data", {
  # Test with empty data frame
  test_data <- data.frame()
  
  # Create a mock result for testing the "No data" scenario
  mock_empty_table <- structure(
    list(x = list(data = data.frame(`No.Data.Available` = "No party data available", check.names = FALSE))),
    class = "datatables"
  )
  
  # Use local_mocked_bindings to avoid namespace issues
  local_mocked_bindings(
    is_any_data_missing = function(...) TRUE,
    create_empty_table = function(...) mock_empty_table,
    create_table = function(...) mock_empty_table
  )
  
  # Use nested with_mocked_bindings for shiny functions
  with_mocked_bindings(
    withProgress = function(expr, ...) force(expr),
    incProgress = function(...) NULL,
    .package = "shiny",
    {
      result <- build_party_table(test_data)
      expect_s3_class(result, "datatables")
      expect_equal(result$x$data$`No.Data.Available`, "No party data available")
    }
  )
})

test_that("is_any_data_missing is called with correct parameters", {
  test_data <- data.frame(
    party_id = c(1),
    given_name = c("John"),
    surname = c("Doe"),
    stringsAsFactors = FALSE
  )
  
  pkg_env <- asNamespace("vegbankweb")
  
  # Mock the is_any_data_missing function to verify its parameters
  with_mocked_bindings(
    is_any_data_missing = function(data_sources, required_sources) {
      # Verify that the parameters are as expected
      expect_equal(names(data_sources), "party_data")
      expect_equal(data_sources$party_data, test_data)
      expect_equal(required_sources, "party_data")
      
      # Return FALSE to indicate data is present
      FALSE
    },
    .env = pkg_env,
    {
      with_mocked_bindings(
        withProgress = function(expr, ...) force(expr),
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          with_mocked_bindings(
            create_table = function(...) {
              structure(list(options = list()), class = "datatables")
            },
            .env = pkg_env,
            {
              result <- build_party_table(test_data)
              expect_s3_class(result, "datatables")
            }
          )
        }
      )
    }
  )
})
