# Mock cover method data for tests
# Real API responses from vegbankr::vb_get_cover_methods(vb_code)

# cm.79 — NPS CoverMethod: no rf, no estimation method, 6 cover indexes
mock_cover_method_cm79 <- data.frame(
  cm_code = "cm.79",
  cover_estimation_method = NA_character_,
  cover_type = "NPS CoverMethod",
  rf_code = NA_character_,
  rf_label = NA_character_,
  stringsAsFactors = FALSE
)
mock_cover_method_cm79$cover_indexes <- I(list(data.frame(
  cover_code = c("01", "02", "03"),
  cover_percent = c(0.5, 3.0, 15.0),
  cv_code = c("cv.1109", "cv.1110", "cv.1111"),
  index_description = c(NA_character_, NA_character_, NA_character_),
  lower_limit = c(0, 1, 5),
  upper_limit = c(1, 5, 25),
  stringsAsFactors = FALSE
)))

# cm.1630 — Domin Cover Scale: rf.35243/UNKNOWN, has estimation method, 22 cover indexes
mock_cover_method_cm1630 <- data.frame(
  cm_code = "cm.1630",
  cover_estimation_method = "canopy cover",
  cover_type = "Domin Cover Scale",
  rf_code = "rf.35243",
  rf_label = "UNKNOWN",
  stringsAsFactors = FALSE
)
mock_cover_method_cm1630$cover_indexes <- I(list(data.frame(
  cover_code = c("1", "2", "3"),
  cover_percent = c(0.050, 0.505, 1.500),
  cv_code = c("cv.9779", "cv.9780", "cv.9781"),
  index_description = c("trace", "0-1 percent", "1-2 percent"),
  lower_limit = c(0.0, 0.1, 1.0),
  upper_limit = c(0.1, 1.0, 2.0),
  stringsAsFactors = FALSE
)))

# cm.1 — Carolina Vegetation Survey: rf.27/CVS Protocol, no estimation method, 40 cover indexes
mock_cover_method_cm1 <- data.frame(
  cm_code = "cm.1",
  cover_estimation_method = NA_character_,
  cover_type = "Carolina Vegetation Survey",
  rf_code = "rf.27",
  rf_label = "CVS Protocol",
  stringsAsFactors = FALSE
)
mock_cover_method_cm1$cover_indexes <- I(list(data.frame(
  cover_code = c("1", "2", "3"),
  cover_percent = c(0.050, 0.505, 1.500),
  cv_code = c("cv.1", "cv.2", "cv.3"),
  index_description = c("trace", "0-1 percent", "1-2 percent"),
  lower_limit = c(0.0, 0.1, 1.0),
  upper_limit = c(0.1, 1.0, 2.0),
  stringsAsFactors = FALSE
)))

# Backward-compatible alias
mock_cover_method_data <- mock_cover_method_cm1
