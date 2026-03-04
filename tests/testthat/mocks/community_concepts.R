# Mock community concept and community classification data for tests

# ── Community concept ─────────────────────────────────────────────────────────
mock_comm_concept_data <- data.frame(
  cc_code = "cc.54321",
  comm_name = "Test Community",
  comm_code = "TESTCOMM",
  comm_level = "Association",
  comm_description = "Test <em>description</em> with HTML",
  concept_rf_code = "rf.999",
  concept_rf_label = "Reference Community 2023",
  obs_count = 42,
  party_label = "Test Ecologist",
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
