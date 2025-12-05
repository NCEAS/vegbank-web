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
mock_plot_data <- data.frame(
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
  plot_validation_level_descr = "High quality",
  stringsAsFactors = FALSE
)

mock_plot_data$top_taxon_observations <- I(list(data.frame(
  plant_name = c("Mock Plant A", "Mock Plant B"),
  pc_code = c("pc.100", "pc.101"),
  stratum_name = c("Canopy", NA),
  cover = c(65.5, 25.25),
  stringsAsFactors = FALSE
)))

mock_plot_data$top_classifications <- I(list(data.frame(
  cl_code = "cl.1553",
  comm_name = "Test Community",
  stringsAsFactors = FALSE
)))

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

# Mock community classification data with new schema
mock_comm_class_interpretations <- data.frame(
  cc_code = c("cc.123", "cc.456"),
  ci_code = c("ci.001", "ci.002"),
  class_confidence = c("High", "Medium"),
  class_fit = c("Good", "Fair"),
  comm_authority_name = c("NVC Authority", "State Authority"),
  comm_authority_rf_code = c("rf.100", NA),
  comm_code = c("COMM001", "COMM002"),
  comm_name = c("Oak-Hickory Forest", "Mixed Hardwood Forest"),
  nomenclatural_type = c("Original", NA),
  notes = c("Primary interpretation", NA),
  type = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

mock_comm_class_contributors <- data.frame(
  party = c("John Smith", "Jane Doe"),
  py_code = c("py.123", "py.456"),
  role = c("Author", "Classifier"),
  stringsAsFactors = FALSE
)

mock_comm_class_data <- data.frame(
  cl_code = "cl.1553",
  ob_code = "ob.2001",
  class_start_date = "Mon, 01 Jan 2020 00:00:00 GMT",
  class_stop_date = "Fri, 31 Dec 2021 00:00:00 GMT",
  inspection = "Visual inspection",
  table_analysis = TRUE,
  multivariate_analysis = FALSE,
  expert_system = TRUE,
  class_publication_rf_code = "rf.789",
  class_publication_rf_label = "Smith et al. 2020",
  class_notes = "Classification based on field observations",
  stringsAsFactors = FALSE
)
mock_comm_class_data$interpretations <- list(mock_comm_class_interpretations)
mock_comm_class_data$contributors <- list(mock_comm_class_contributors)

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

# Mock reference data
mock_reference_data <- data.frame(
  degree = NA,
  doi = NA,
  full_citation = "Example Author. 2020. Example citation.",
  isbn = NA,
  journal = NA,
  publication_date = "Wed, 01 Jan 2020 08:00:00 GMT",
  publication_place = "Example City",
  publisher = "Example Publisher",
  reference_type = "Book",
  rf_code = "rf.123",
  short_name = "Example Author. 2020.",
  title = "Example Title",
  total_pages = 250,
  url = NA,
  stringsAsFactors = FALSE
)
