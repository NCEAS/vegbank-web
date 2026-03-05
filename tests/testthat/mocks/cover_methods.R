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
  cover_code = c("01", "02", "03", "04", "05", "06"),
  cover_percent = c(0.5, 3.0, 15.0, 37.5, 62.5, 87.5),
  cv_code = c("cv.1109", "cv.1110", "cv.1111", "cv.1112", "cv.1113", "cv.1114"),
  index_description = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
  lower_limit = c(0, 1, 5, 25, 50, 75),
  upper_limit = c(1, 5, 25, 50, 75, 100),
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
  cover_code = c(
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "+",
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "+"
  ),
  cover_percent = c(
    0.05, 0.505, 1.5, 3.5, 7.5, 17.5, 37.5, 62.5, 85, 97.5, 0,
    0.05, 0.505, 1.5, 3.5, 7.5, 17.5, 37.5, 62.5, 85, 97.5, 0
  ),
  cv_code = c(
    "cv.9779", "cv.9780", "cv.9781", "cv.9782", "cv.9783", "cv.9784",
    "cv.9785", "cv.9786", "cv.9787", "cv.9788", "cv.9789",
    "cv.9793", "cv.9794", "cv.9795", "cv.9796", "cv.9797", "cv.9798",
    "cv.9799", "cv.9800", "cv.9801", "cv.9802", "cv.9803"
  ),
  index_description = c(
    "trace", "0-1 percent", "1-2 percent", "2-5 percent", "5-10 percent",
    "10-25 percent", "25-50 percent", "50-75 percent", "75-95 percent",
    "95-100 percent", NA_character_,
    "trace", "0-1 percent", "1-2 percent", "2-5 percent", "5-10 percent",
    "10-25 percent", "25-50 percent", "50-75 percent", "75-95 percent",
    "95-100 percent", NA_character_
  ),
  lower_limit = c(
    0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95, 0,
    0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95, 0
  ),
  upper_limit = c(
    0.1, 1, 2, 5, 10, 25, 50, 75, 95, 100, 1,
    0.1, 1, 2, 5, 10, 25, 50, 75, 95, 100, 1
  ),
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
  cover_code = c(
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"
  ),
  cover_percent = c(
    0.05, 0.505, 1.5, 3.5, 7.5, 17.5, 37.5, 62.5, 85, 97.5,
    0.05, 0.505, 1.5, 3.5, 7.5, 17.5, 37.5, 62.5, 85, 97.5,
    0.05, 0.505, 1.5, 3.5, 7.5, 17.5, 37.5, 62.5, 85, 97.5,
    0.05, 0.505, 1.5, 3.5, 7.5, 17.5, 37.5, 62.5, 85, 97.5
  ),
  cv_code = c(
    "cv.1", "cv.2", "cv.3", "cv.4", "cv.5", "cv.6", "cv.7", "cv.8", "cv.9", "cv.11",
    "cv.9731", "cv.9732", "cv.9733", "cv.9734", "cv.9735", "cv.9736", "cv.9737", "cv.9738", "cv.9739", "cv.9740",
    "cv.9769", "cv.9770", "cv.9771", "cv.9772", "cv.9773", "cv.9774", "cv.9775", "cv.9776", "cv.9777", "cv.9778",
    "cv.9838", "cv.9839", "cv.9840", "cv.9841", "cv.9842", "cv.9843", "cv.9844", "cv.9845", "cv.9846", "cv.9847"
  ),
  index_description = c(
    "trace", "0-1 percent", "1-2 percent", "2-5 percent", "5-10 percent", "10-25 percent", "25-50 percent", "50-75 percent", "75-95 percent", "95-100 percent",
    "trace", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
    "trace", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
    "trace", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_
  ),
  lower_limit = c(
    0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95,
    0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95,
    0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95,
    0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95
  ),
  upper_limit = c(
    0.1, 1, 2, 5, 10, 25, 50, 75, 95, 100,
    0.1, 1, 2, 5, 10, 25, 50, 75, 95, 100,
    0.1, 1, 2, 5, 10, 25, 50, 75, 95, 100,
    0.1, 1, 2, 5, 10, 25, 50, 75, 95, 100
  ),
  stringsAsFactors = FALSE
)))

# Backward-compatible alias
mock_cover_method_data <- mock_cover_method_cm1
