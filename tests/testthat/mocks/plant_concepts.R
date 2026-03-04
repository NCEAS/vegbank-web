# Mock plant concept and taxon observation data for tests

# ── Plant concept ─────────────────────────────────────────────────────────────
mock_plant_concept_data <- data.frame(
  pc_code = "pc.12345",
  plant_name = "Test Plant Species",
  plant_code = "TESTPLANT",
  plant_level = "Species",
  plant_description = "<em>Test plant description</em> with HTML",
  concept_rf_code = "rf.123",
  concept_rf_label = "Reference Flora 2023",
  obs_count = 150,
  party_label = "Test Botanist",
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


# ── Taxon observation ─────────────────────────────────────────────────────────
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
