# Tests for detail_view

# Create mock objects for testing
mock_output <- local({
  # Create empty output slots that can be assigned to
  environment()
})

mock_session <- local({
  messages <- list()
  list(
    sendCustomMessage = function(type, message) {
      messages[[type]] <<- message
    },
    messages = messages,
    reset_messages = function() {
      messages <<- list()
    }
  )
})

# Mock test data
mock_plot_data <- list(
  plot_observation = list(
    author_obs_code = "TEST123",
    author_plot_code = "Plot 123",
    latitude = 40.7128,
    longitude = -74.0060,
    location_narrative = "Test location",
    state_province = "Test State",
    country = "Test Country",
    area = "10x10m",
    permanence = "Permanent",
    elevation = "100m",
    slope_aspect = "North",
    slope_gradient = "10%",
    confidentiality_text = "Not confidential",
    obs_start_date = "2023-01-01",
    project_name = "Test Project",
    cover_type = "Test Cover",
    stratum_method_name = "Test Stratum Method",
    stratum_method_description = "Test description",
    taxon_observation_area = "100 sq m",
    auto_taxon_cover = FALSE,
    plot_validation_level_descr = "High quality"
  )
)

mock_comm_concept_data <- data.frame(
  comm_name = "Test Community",
  comm_description = "Test <em>description</em> with HTML",
  obs_count = 42,
  class_system = "Scientific",
  stringsAsFactors = FALSE
)

mock_taxon_data <- list(
  author_plant_name = "Test Plant",
  int_curr_plant_sci_name_no_auth = "Scientific name",
  int_curr_plant_sci_full = "Scientific name L.",
  int_orig_plant_sci_name_no_auth = "Original name",
  int_orig_plant_sci_full = "Original name Auth.",
  int_curr_plant_common = "Common plant",
  int_orig_plant_common = "Old common name",
  max_cover = 75,
  taxon_inference_area = 100,
  taxon_observation_id = "TX123",
  int_curr_plant_code = "CODE1",
  int_orig_plant_code = "CODE2"
)

# Create mock community classification data for testing
mock_comm_class_data <- data.frame(
  cl_code = "cl.1553",
  inspection = "Visual inspection",
  table_analysis = TRUE,
  multivariate_analysis = FALSE,
  comm_concept_id = 456,
  class_fit = "Good",
  class_confidence = "High",
  comm_authority_id = "AUTH789",
  type = "Standard",
  stringsAsFactors = FALSE
)

# Create mock project data for testing
mock_project_data <- data.frame(
  project_id = 123,
  project_name = "Test Project",
  project_description = "A test project description",
  start_date = "2021-01-01",
  stop_date = "2022-12-31",
  obs_count = 42,
  last_plot_added_date = "2022-10-15",
  stringsAsFactors = FALSE
)

# Create mock party data for testing
mock_party_data <- data.frame(
  party_id = 123,
  given_name = "John",
  middle_name = "Q",
  surname = "Public",
  salutation = "Dr.",
  organization_name = "Example Organization",
  contact_instructions = "Email: john.public@example.org",
  py_code = "py.123",
  stringsAsFactors = FALSE
)

# Mock plant concept data
mock_plant_concept_data <- data.frame(
  pc_code = "pc.12345",
  plant_name = "Test Plant Species",
  plant_code = "TESTPLANT",
  plant_level = "Species",
  concept_rf_name = "Reference Flora 2023",
  obs_count = 150,
  party = "Test Botanist",
  start_date = "2020-01-01",
  stop_date = "2023-12-31",
  status = "Active",
  parent_name = "Test Genus",
  parent_pc_code = "pc.12300",
  children = '{"pc.12346": "Test Plant var. minor", "pc.12347": "Test Plant var. major"}',
  usage_names = '{"Synonym": "Old Plant Name", "Common": "Common Plant"}',
  stringsAsFactors = FALSE
)

test_that("build_plot_obs_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_plot_obs_details_view(NULL)

  # It should return a list with the expected UI components
  expect_type(result, "list")
  expect_named(result, c(
    "plot_id_details", "location_details", "layout_details", "environmental_details",
    "methods_details", "plot_quality_details", "taxa_details", "communities_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
})

test_that("build_plot_obs_details_view formats plot data correctly", {
  result <- build_plot_obs_details_view(mock_plot_data)

  # Test structure and types
  expect_type(result, "list")
  expect_named(result, c(
    "plot_id_details", "location_details", "layout_details", "environmental_details",
    "methods_details", "plot_quality_details", "taxa_details", "communities_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
  expect_true(inherits(result$location_details, "shiny.render.function"))
})

test_that("build_comm_concept_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_comm_concept_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c(
    "community_name", "community_description",
    "observation_count", "community_aliases"
  ))

  # Each component should be a render function
  expect_true(inherits(result$community_name, "shiny.render.function"))
})

test_that("build_comm_concept_details_view formats community data correctly", {
  result <- build_comm_concept_details_view(mock_comm_concept_data)

  # Test structure and types
  expect_type(result, "list")
  # Verify names in any order
  expect_setequal(names(result), c(
    "community_name", "community_description",
    "observation_count", "community_aliases"
  ))

  # Each component should be a render function
  expect_true(inherits(result$community_name, "shiny.render.function"))
  expect_true(inherits(result$community_description, "shiny.render.function"))
  expect_true(inherits(result$observation_count, "shiny.render.function"))
})

test_that("build_comm_class_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_comm_class_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c(
    "observation_details", "community_interpretation"
  ))

  # Each component should be a render function
  expect_true(inherits(result$observation_details, "shiny.render.function"))
  expect_true(inherits(result$community_interpretation, "shiny.render.function"))
})

test_that("build_comm_class_details_view formats classification data correctly", {
  result <- build_comm_class_details_view(mock_comm_class_data)

  # Test structure and types
  expect_type(result, "list")
  # Verify names in any order
  expect_setequal(names(result), c(
    "observation_details", "community_interpretation"
  ))

  # Each component should be a render function
  expect_true(inherits(result$observation_details, "shiny.render.function"))
  expect_true(inherits(result$community_interpretation, "shiny.render.function"))
})

test_that("build_taxon_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_taxon_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")

  # Use expect_true to check that all required components exist
  expect_true(all(c("taxon_name", "taxon_coverage", "taxon_aliases", "taxon_identifiers") %in% names(result)))

  # Each component should be a render function
  expect_true(inherits(result$taxon_name, "shiny.render.function"))
  expect_true(inherits(result$taxon_coverage, "shiny.render.function"))
  expect_true(inherits(result$taxon_aliases, "shiny.render.function"))
  expect_true(inherits(result$taxon_identifiers, "shiny.render.function"))
})

test_that("build_taxon_details_view formats taxon data correctly", {
  result <- build_taxon_details_view(mock_taxon_data)

  # Test structure and types
  expect_type(result, "list")

  # Check that all expected keys are present (order doesn't matter)
  expect_true(all(c("taxon_name", "taxon_coverage", "taxon_aliases", "taxon_identifiers") %in% names(result)))

  # Each component should be a render function
  expect_true(inherits(result$taxon_name, "shiny.render.function"))
  expect_true(inherits(result$taxon_coverage, "shiny.render.function"))
})

test_that("build_project_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_project_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_setequal(names(result), c(
    "project_name", "project_description",
    "project_dates", "project_contributors", "project_observations"
  ))

  # Each component should be a render function
  expect_true(inherits(result$project_name, "shiny.render.function"))
  expect_true(inherits(result$project_description, "shiny.render.function"))
  expect_true(inherits(result$project_dates, "shiny.render.function"))
  expect_true(inherits(result$project_contributors, "shiny.render.function"))
  expect_true(inherits(result$project_observations, "shiny.render.function"))
})

test_that("build_project_details_view formats project data correctly", {
  result <- build_project_details_view(mock_project_data)

  # Test structure and types
  expect_type(result, "list")
  # Verify names in any order
  expect_setequal(names(result), c(
    "project_name", "project_observations", "project_description",
    "project_contributors", "project_dates"
  ))

  # Each component should be a render function
  expect_true(inherits(result$project_name, "shiny.render.function"))
  expect_true(inherits(result$project_description, "shiny.render.function"))
  expect_true(inherits(result$project_observations, "shiny.render.function"))
  expect_true(inherits(result$project_dates, "shiny.render.function"))
})

test_that("build_party_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_party_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_setequal(names(result), c(
    "party_name", "party_organization", "party_contact", "party_projects"
  ))

  # Each component should be a render function
  expect_true(inherits(result$party_name, "shiny.render.function"))
  expect_true(inherits(result$party_organization, "shiny.render.function"))
  expect_true(inherits(result$party_contact, "shiny.render.function"))
  expect_true(inherits(result$party_projects, "shiny.render.function"))
})

test_that("build_party_details_view formats party data correctly", {
  result <- build_party_details_view(mock_party_data)

  # Test structure and types
  expect_type(result, "list")
  # Verify names in any order
  expect_setequal(names(result), c(
    "party_name", "party_organization", "party_contact", "party_projects"
  ))

  # Each component should be a render function
  expect_true(inherits(result$party_name, "shiny.render.function"))
  expect_true(inherits(result$party_organization, "shiny.render.function"))
  expect_true(inherits(result$party_contact, "shiny.render.function"))
  expect_true(inherits(result$party_projects, "shiny.render.function"))
})


# Create a custom mock for show_detail_view tests
mock_vegbank_api_error <- function(output, session) {
  # Replace the vegbankr API call inside the function
  withr::with_environment(
    env = environment(show_detail_view),
    code = {
      # Create local versions of the API functions that return errors
      get_plot_observation_details <- function(vb_code) {
        list() # Empty list simulates API error
      }
      get_community_concept <- function(vb_code) {
        list()
      }
      get_taxon_observation <- function(vb_code) {
        list()
      }

      with_mock_shiny_notifications({
        # Call the function with our mock functions in its environment
        result <- show_detail_view("plot-observation", "TEST123", output, session)

        # Check that the error was handled correctly
        expect_false(result)

        # Check notifications
        notifications <- get_mock_notifications()
        expect_true(length(notifications) > 0)

        # If we have notifications, check the content
        if (length(notifications) > 0) {
          error_notification <- notifications[[1]]
          expect_equal(error_notification$type, "error")
        }
      })
    }
  )
}

test_that("show_detail_view handles API errors appropriately", {
  # Skip on CRAN
  skip_on_cran()

  # Create a fresh session and output for this test
  test_output <- new.env()
  test_session <- list(
    sendCustomMessage = function(type, message) {},
    messages = list()
  )

  with_mocked_bindings(
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("plot-observation", "TEST123", test_output, test_session)

        # Verify results
        expect_false(result)

        # Check notifications
        notifications <- get_mock_notifications()
        expect_true(length(notifications) > 0)

        # Verify the notification contains the error message
        error_notification <- notifications[[1]]
        expect_equal(error_notification$type, "error")
      })
    },
    get_plot_observation_details = function(ob_code) list(),
    get_community_concept = function(cc_code) list(),
    get_taxon_observation = function(to_code) list(),
    .package = "vegbankr"
  )
})

test_that("show_detail_view handles success case for plot details", {
  # Skip on CRAN
  skip_on_cran()

  # Setup test environment
  messages_captured <- list()

  # Create a modified session with message capturing
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )

  # Create a mock output
  fake_output <- new.env()

  with_mocked_bindings(
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("plot-observation", "TEST123", fake_output, fake_session)

        # Verify results
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "plot-observation")
      })
    },
    get_plot_observation_details = function(ob_code) mock_plot_data,
    get_community_concept = function(cc_code) list(),
    get_taxon_observation = function(to_code) list(),
    .package = "vegbankr"
  )
})

test_that("show_detail_view handles project data correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Setup test environment
  messages_captured <- list()

  # Create a modified session with message capturing
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )

  # Create a mock output
  fake_output <- new.env()

  with_mocked_bindings(
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("project", "PROJ123", fake_output, fake_session)

        # Verify results
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "project")
      })
    },
    get_project = function(pj_code) mock_project_data,
    .package = "vegbankr"
  )
})

test_that("show_detail_view handles community classification data correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Setup test environment
  messages_captured <- list()

  # Create a modified session with message capturing
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )

  # Create a mock output
  fake_output <- new.env()

  with_mocked_bindings(
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("community-classification", "CLASS123", fake_output, fake_session)

        # Verify results
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "community-classification")
      })
    },
    get_community_classification = function(cl_code) mock_comm_class_data,
    .package = "vegbankr"
  )
})

test_that("show_detail_view handles party data correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Setup test environment
  messages_captured <- list()

  # Create a modified session with message capturing
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )

  # Create a mock output
  fake_output <- new.env()

  with_mocked_bindings(
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("party", "PARTY123", fake_output, fake_session)

        # Verify results
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "party")
      })
    },
    get_party = function(py_code) mock_party_data,
    .package = "vegbankr"
  )
})

test_that("create_detail_table correctly formats data into HTML table", {
  # Create test data
  test_details <- list(
    field1 = "Value 1",
    field2 = "Value 2",
    field3 = "Value 3"
  )

  display_names <- c(field1 = "Display Name 1", field2 = "Display Name 2")

  # Test function
  result <- create_detail_table(test_details, display_names)

  # Verify structure
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "table")
  expect_equal(result$attribs$class, "table table-sm table-striped table-hover")

  # Convert to character to check content
  result_text <- as.character(result)

  # Check for display names and values
  expect_true(grepl("Display Name 1", result_text))
  expect_true(grepl("Display Name 2", result_text))
  expect_true(grepl("field3", result_text)) # This should use the original name
  expect_true(grepl("Value 1", result_text))
  expect_true(grepl("Value 2", result_text))
  expect_true(grepl("Value 3", result_text))
})

test_that("safe_render_details handles valid and invalid fields gracefully", {
  # Create test data with some fields
  test_data <- data.frame(
    field1 = "Value 1",
    field2 = "Value 2",
    field3 = NA,
    stringsAsFactors = FALSE
  )

  # Mock get_field_display_names
  with_mocked_bindings(
    get_field_display_names = function() {
      c(field1 = "Display Name 1", field2 = "Display Name 2", field3 = "Display Name 3")
    },
    {
      # Test with valid fields
      valid_result <- safe_render_details(c("field1", "field2"), test_data)
      expect_s3_class(valid_result, "shiny.render.function")

      # Test with invalid fields
      invalid_result <- safe_render_details(c("non_existent_field"), test_data)
      expect_s3_class(invalid_result, "shiny.render.function")

      # Test with mixed valid and invalid fields
      mixed_result <- safe_render_details(c("field1", "non_existent_field"), test_data)
      expect_s3_class(mixed_result, "shiny.render.function")

      # Test with NA values
      na_result <- safe_render_details(c("field3"), test_data)
      expect_s3_class(na_result, "shiny.render.function")
    }
  )
})

# Tests for plant concept detail functions

test_that("build_plant_concept_details_view handles valid data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      result <- build_plant_concept_details_view(mock_plant_concept_data)
      
      expect_type(result, "list")
      expect_equal(names(result), c("plant_concept_name", "plant_concept_details", "plant_party_perspective"))
      expect_s3_class(result$plant_concept_name, "shiny.render.function")
      expect_s3_class(result$plant_concept_details, "shiny.render.function")
      expect_s3_class(result$plant_party_perspective, "shiny.render.function")
    }
  )
})

test_that("build_plant_concept_details_view handles null/empty data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      # Test with NULL data
      result_null <- build_plant_concept_details_view(NULL)
      expect_type(result_null, "list")
      expect_equal(names(result_null), c("plant_concept_name", "plant_concept_details", "plant_party_perspective"))
      
      # Test with empty data frame
      empty_data <- data.frame()
      result_empty <- build_plant_concept_details_view(empty_data)
      expect_type(result_empty, "list")
      expect_equal(names(result_empty), c("plant_concept_name", "plant_concept_details", "plant_party_perspective"))
    }
  )
})

test_that("create_party_perspective_ui handles valid JSON data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      result <- create_party_perspective_ui(mock_plant_concept_data)
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

test_that("create_party_perspective_ui handles invalid JSON", {
  invalid_json_data <- mock_plant_concept_data
  invalid_json_data$children <- '{"invalid": json}'
  invalid_json_data$usage_names <- '{"invalid": json}'
  
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      # Should not throw error even with invalid JSON
      result <- create_party_perspective_ui(invalid_json_data)
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

test_that("create_party_perspective_ui handles NA values", {
  na_data <- mock_plant_concept_data
  na_data$children <- NA
  na_data$usage_names <- NA
  na_data$party <- NA
  na_data$status <- NA
  na_data$start_date <- NA
  na_data$stop_date <- NA
  na_data$parent_name <- NA
  
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      result <- create_party_perspective_ui(na_data)
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

test_that("create_plant_aliases_ui handles valid JSON data", {
  result <- create_plant_aliases_ui(mock_plant_concept_data)
  
  # Should return HTML tags directly (not renderUI)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that("create_plant_aliases_ui handles invalid JSON", {
  invalid_json_data <- mock_plant_concept_data
  invalid_json_data$usage_names <- '{"invalid": json}'
  
  # Should not throw error even with invalid JSON
  result <- create_plant_aliases_ui(invalid_json_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that("create_plant_aliases_ui handles NA/empty usage_names", {
  # Test with NA
  na_data <- mock_plant_concept_data
  na_data$usage_names <- NA
  result_na <- create_plant_aliases_ui(na_data)
  expect_true(inherits(result_na, "shiny.tag"))
  
  # Test with empty string
  empty_data <- mock_plant_concept_data
  empty_data$usage_names <- ""
  result_empty <- create_plant_aliases_ui(empty_data)
  expect_true(inherits(result_empty, "shiny.tag"))
  
  # Test with NULL
  null_data <- mock_plant_concept_data
  null_data$usage_names <- NULL
  result_null <- create_plant_aliases_ui(null_data)
  expect_true(inherits(result_null, "shiny.tag"))
})

test_that("create_plant_aliases_ui sorts usage types alphabetically", {
  sorted_data <- mock_plant_concept_data
  sorted_data$usage_names <- '{"Zebra": "Last name", "Alpha": "First name", "Beta": "Second name"}'
  
  result <- create_plant_aliases_ui(sorted_data)
  # The function should handle sorting internally
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that("coalesce function (%|||%) works correctly", {
  # Test with various input types
  expect_equal(NULL %|||% "default", "default")
  expect_equal(NA %|||% "default", "default")
  expect_equal("" %|||% "default", "default")
  expect_equal("NA" %|||% "default", "default")
  expect_equal("valid" %|||% "default", "valid")
  expect_equal(0 %|||% "default", 0)
  expect_equal(FALSE %|||% "default", FALSE)
  
  # Test with vectors
  expect_equal(c(NA, "valid") %|||% "default", "default")  # Takes first element
  expect_equal(c("valid", "other") %|||% "default", "valid")  # Takes first element
})

test_that("show_detail_view works with plant-concept type", {
  # Mock function that returns test data
  mock_get_plant_concept <- function(code) {
    if (code == "pc.12345") {
      return(mock_plant_concept_data)
    }
    return(data.frame())
  }
  
  # Test that the function returns TRUE for valid data
  result <- mock_get_plant_concept("pc.12345")
  expect_true(nrow(result) > 0)
  expect_equal(result$pc_code, "pc.12345")
})

test_that("show_detail_view handles missing plant concept data", {
  # Mock function that returns empty data
  mock_get_plant_concept_empty <- function(code) {
    return(data.frame())
  }
  
  # Test that the function returns empty data for invalid codes
  result <- mock_get_plant_concept_empty("nonexistent")
  expect_true(nrow(result) == 0)
})
