# Tests for detail_view.R

# Create mock objects for testing
mock_output <- local({
  # Create empty output slots that can be assigned to
  environment()
})

mock_session <- list(
  sendCustomMessage = function(type, message) {
    mock_session$messages[[type]] <- message
  },
  messages = list()
)

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

mock_community_data <- data.frame(
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

test_that("build_community_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_community_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c(
    "community_name", "community_description",
    "observation_count", "community_aliases"
  ))

  # Each component should be a render function
  expect_true(inherits(result$community_name, "shiny.render.function"))
})

test_that("build_community_details_view formats community data correctly", {
  result <- build_community_details_view(mock_community_data)

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

# Create a custom mock for show_detail_view tests
mock_vegbank_api_error <- function(output, session) {
  # Replace the vegbankr API call inside the function
  withr::with_environment(
    env = environment(show_detail_view),
    code = {
      # Create local versions of the API functions that return errors
      get_plot_observation_details <- function(accession_code) { 
        return(list()) # Empty list simulates API error
      }
      get_community_concept <- function(accession_code) {
        return(list())
      }
      get_taxon_observation <- function(accession_code) {
        return(list())
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
  
  with_mocked_bindings({
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
  get_plot_observation_details = function(accession_code) list(), 
  get_community_concept = function(accession_code) list(),
  get_taxon_observation = function(accession_code) list(),
  .package = "vegbankr")
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
  
  with_mocked_bindings({
    with_mock_shiny_notifications({
      result <- show_detail_view("plot-observation", "TEST123", fake_output, fake_session)
      
      # Verify results
      expect_true(result)
      expect_true(!is.null(messages_captured$openOverlay))
      expect_true(!is.null(messages_captured$updateDetailType))
      expect_equal(messages_captured$updateDetailType$type, "plot-observation")
    })
  },
  get_plot_observation_details = function(accession_code) mock_plot_data,
  get_community_concept = function(accession_code) list(),
  get_taxon_observation = function(accession_code) list(),
  .package = "vegbankr")
})
