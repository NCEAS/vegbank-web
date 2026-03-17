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

# ── to.650267 · Ceanothus fendleri · ob.32978 ───────────────────────────────
# Single importance record (shrubs stratum), single author interpretation,
# is_orig=TRUE and is_curr=TRUE on the same record, no taxon_inference_area.
mock_taxon_obs_to650267 <- data.frame(
  author_plant_name = "Ceanothus fendleri Gray",
  int_curr_pc_code = "pc.17202",
  int_orig_pc_code = "pc.17202",
  ob_code = "ob.32978",
  rf_code = 33L,
  rf_label = "USDA Plants 2002",
  taxon_inference_area = NA_real_,
  to_code = "to.650267",
  stringsAsFactors = FALSE
)
mock_taxon_obs_to650267$taxon_importance <- I(list(data.frame(
  basal_area = NA_real_,
  biomass = NA_real_,
  cover = 0.5,
  cover_code = "<1",
  inference_area = NA_real_,
  sm_code = "sm.337912",
  stratum_name = "shrubs",
  tm_code = "tm.906122",
  stringsAsFactors = FALSE
)))

mock_taxon_interps_to650267 <- data.frame(
  ti_code = "ti.684634",
  to_code = "to.650267",
  pc_code = "pc.17202",
  py_code = "py.191319",
  plant_label = "Ceanothus fendleri Gray [USDA Plants 2002]",
  party_label = "Lee, Michael",
  interpretation_date = as.POSIXct("2002-06-26 07:00:00", tz = "UTC"),
  interpretation_type = "Author",
  taxon_fit = NA_character_,
  taxon_confidence = NA_character_,
  group_type = NA_character_,
  role = "Plot author",
  is_orig = TRUE,
  is_curr = TRUE,
  stringsAsFactors = FALSE
)

# ── to.2176296 · Lonicera japonica · ob.120762 ──────────────────────────────
# Four importance records across strata; two interpretations — original author
# (Wagner, Gail 2011) superseded by a taxonomic revision (Lee, Michael 2015).
mock_taxon_obs_to2176296 <- data.frame(
  author_plant_name = "Lonicera japonica",
  int_curr_pc_code = "pc.392339",
  int_orig_pc_code = "pc.110027",
  ob_code = "ob.120762",
  rf_code = 50592L,
  rf_label = "Weakley Jan 1, 2006",
  taxon_inference_area = 100,
  to_code = "to.2176296",
  stringsAsFactors = FALSE
)
mock_taxon_obs_to2176296$taxon_importance <- I(list(data.frame(
  basal_area = c(NA_real_, NA_real_, NA_real_, NA_real_),
  biomass = c(NA_real_, NA_real_, NA_real_, NA_real_),
  cover = c(3.5, 0.505, 3.5, 3.5),
  cover_code = c("4", "2", "4", "4"),
  inference_area = c(NA_real_, NA_real_, NA_real_, NA_real_),
  sm_code = c("sm.609592", "sm.609593", "sm.609833", NA_character_),
  stratum_name = c("Shrub", "Herb", "module", "<All>"),
  tm_code = c("tm.4872682", "tm.4872683", "tm.4872680", "tm.4872681"),
  stringsAsFactors = FALSE
)))

mock_taxon_interps_to2176296 <- data.frame(
  ti_code = c("ti.2461897", "ti.2669722"),
  to_code = rep("to.2176296", 2),
  pc_code = c("pc.110027", "pc.392339"),
  py_code = c("py.191619", "py.191991"),
  plant_label = c(
    "Lonicera japonica [Weakley Jan 1, 2006]",
    "Lonicera japonica [Weakley 2015]"
  ),
  party_label = c("Wagner, Gail", "Lee, Michael"),
  interpretation_date = as.POSIXct(c(
    "2011-05-12 00:00:00", "2015-05-18 00:00:00"
  ), tz = "UTC"),
  interpretation_type = c("author", "Taxonomic revision"),
  taxon_fit = c(NA_character_, NA_character_),
  taxon_confidence = c(NA_character_, NA_character_),
  group_type = c(NA_character_, NA_character_),
  role = c("Author", "Data Manager"),
  is_orig = c(TRUE, FALSE),
  is_curr = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)
