# ── Community classification ──────────────────────────────────────────────────
mock_comm_class_interpretations <- data.frame(
  cc_code = c("cc.123", "cc.456"),
  ci_code = c("ci.001", "ci.002"),
  class_confidence = c("High", "Medium"),
  class_fit = c("Good", "Fair"),
  comm_authority_rf_label = c("NVC Authority", "State Authority"),
  comm_authority_rf_code = c("rf.100", NA),
  comm_code = c("COMM001", "COMM002"),
  comm_name = c("Oak-Hickory Forest", "Mixed Hardwood Forest"),
  nomenclatural_type = c(FALSE, NA),
  notes = c("Primary interpretation", NA),
  type = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

mock_comm_class_contributors <- data.frame(
  party_label = c("John Smith", "Jane Doe"),
  py_code = c("py.123", "py.456"),
  role = c("Author", "Classifier"),
  stringsAsFactors = FALSE
)

mock_comm_class_data <- data.frame(
  cl_code = "cl.1553",
  ob_code = "ob.2001",
  class_start_date = "Mon, 01 Jan 2020 00:00:00 GMT",
  class_stop_date = "Fri, 31 Dec 2021 00:00:00 GMT",
  inspection = FALSE,
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
