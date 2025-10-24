# Shared mock objects for detail view testing
# This file is automatically loaded by testthat before running tests

# Mock Shiny output environment
mock_output <- new.env()

# Mock Shiny session with message capture
mock_session <- list(
  sendCustomMessage = function(type, message) {
    # Store messages for verification in tests
    if (!exists("messages", envir = mock_session, inherits = FALSE)) {
      mock_session$messages <- list()
    }
    mock_session$messages[[type]] <- message
  }
)

# Mock plot observation data
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

# Mock community concept data
mock_comm_concept_data <- data.frame(
  cc_code = "cc.54321",
  comm_name = "Test Community",
  comm_code = "TESTCOMM",
  comm_level = "Association",
  comm_description = "Test <em>description</em> with HTML",
  concept_rf_code = "rf.999",
  concept_rf_name = "Reference Community 2023",
  obs_count = 42,
  party = "Test Ecologist",
  py_code = "py.777",
  status = "Active",
  start_date = "2018-04-15",
  stop_date = "2024-01-10",
  parent_name = "Parent Community",
  parent_cc_code = "cc.54000",
  class_system = "Scientific",
  stringsAsFactors = FALSE
)

mock_comm_concept_data$children <- I(list(list("cc.54322" = "Child Community")))
mock_comm_concept_data$correlations <- I(list(NULL))
mock_comm_concept_data$usages <- I(list(data.frame(
  class_system = c("Scientific", "Common"),
  comm_name = c("Prairie Alt", "Prairie Common"),
  stringsAsFactors = FALSE
)))

# Mock taxon observation data
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

# Mock community classification data
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

# Mock project data
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

# Mock party data
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
  plant_description = "<em>Test plant description</em> with HTML",
  concept_rf_code = "rf.123",
  concept_rf_name = "Reference Flora 2023",
  obs_count = 150,
  party = "Test Botanist",
  py_code = "py.123",
  start_date = "2020-01-01",
  stop_date = "2023-12-31",
  status = "Active",
  parent_name = "Test Genus",
  parent_pc_code = "pc.12300",
  stringsAsFactors = FALSE
)

mock_plant_concept_data$children <- I(list(list(
  "pc.12346" = "Test Plant var. minor",
  "pc.12347" = "Test Plant var. major"
)))
mock_plant_concept_data$correlations <- I(list(NULL))
mock_plant_concept_data$usages <- I(list(data.frame(
  class_system = c("Synonym", "Common"),
  plant_name = c("Old Plant Name", "Common Plant"),
  stringsAsFactors = FALSE
)))
