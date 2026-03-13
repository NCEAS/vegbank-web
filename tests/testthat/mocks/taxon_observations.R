# Mock taxon observation data for tests
# All mocks are derived from real vegbankr API response shapes for
# vb_get_taxon_observations() and vb_get_taxon_interpretations().

# ── to.2178147 · Bromus sp. #5 · ob.120762 ──────────────────────────────────
# Single taxon observation with 3 importance records and a reference.
mock_taxon_obs_to2178147 <- data.frame(
  author_plant_name = "Bromus sp. #5",
  int_curr_pc_code = "pc.391162",
  int_orig_pc_code = "pc.203088",
  ob_code = "ob.120762",
  rf_code = 50592L,
  rf_label = "Weakley Jan 1, 2006",
  taxon_inference_area = 100,
  to_code = "to.2178147",
  stringsAsFactors = FALSE
)
mock_taxon_obs_to2178147$taxon_importance <- I(list(data.frame(
  basal_area = c(NA, NA, NA),
  biomass = c(NA, NA, NA),
  cover = c(0.505, 0.505, 0.505),
  cover_code = c("2", "2", "2"),
  inference_area = c(NA, NA, NA),
  sm_code = c("sm.609593", "sm.609833", NA),
  stratum_name = c("Herb", "module", "<All>"),
  tm_code = c("tm.4880838", "tm.4880836", "tm.4880837"),
  stringsAsFactors = FALSE
)))

# Interpretation data (flat dataframe returned by vb_get_taxon_interpretations)
mock_taxon_interps_to2178147 <- data.frame(
  ti_code = c("ti.2463673", "ti.2503999", "ti.2504000", "ti.2512742", "ti.2512743", "ti.2812266"),
  to_code = rep("to.2178147", 6),
  pc_code = c("pc.203088", "pc.110107", "pc.110148", "pc.110107", "pc.110107", "pc.391162"),
  py_code = c("py.191619", "py.191512", "py.191512", "py.191512", "py.191512", "py.191991"),
  plant_label = c(
    "Bromus [Weakley Jan 1, 2006]",
    "Bromus pubescens [Weakley Jan 1, 2006]",
    "Poa autumnalis [Weakley Jan 1, 2006]",
    "Bromus pubescens [Weakley Jan 1, 2006]",
    "Bromus pubescens [Weakley Jan 1, 2006]",
    "Bromus pubescens [Weakley 2015]"
  ),
  party_label = c("Wagner, Gail", "LeBlond, Richard", "LeBlond, Richard",
                  "LeBlond, Richard", "LeBlond, Richard", "Lee, Michael"),
  interpretation_date = as.POSIXct(c(
    "2011-05-12 00:00:00", "2014-04-02 11:14:15", "2014-04-02 11:14:44",
    "2015-02-02 16:53:30", "2015-02-02 16:53:37", "2015-05-18 00:00:00"
  ), tz = "UTC"),
  interpretation_type = c(
    "author", "Finer resolution", "Finer resolution",
    "Finer resolution", "Correction", "Taxonomic revision"
  ),
  taxon_fit = c(NA, "Absolutely correct", "Absolutely correct", "Good answer", "Good answer", NA),
  taxon_confidence = c(NA, "High", "High", "High", "High", NA),
  group_type = rep(NA, 6),
  is_orig = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  is_curr = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

# ── Minimal observation with no importance / no reference ────────────────────
mock_taxon_obs_minimal <- data.frame(
  author_plant_name = "Unknown sp.",
  ob_code = "ob.999",
  rf_code = NA_integer_,
  rf_label = NA_character_,
  taxon_inference_area = NA_real_,
  to_code = "to.999",
  stringsAsFactors = FALSE
)
mock_taxon_obs_minimal$taxon_importance <- I(list(data.frame()))
