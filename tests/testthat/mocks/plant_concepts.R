# Mock plant concept data for tests
# All mocks are derived from real vegbankr::vb_get_plant_concepts() API responses.

# ── pc.111478 · Acer rubrum L. (red maple) ───────────────────────────────────
# current_accepted=TRUE, obs_count=0 (not widely observed in VegBank),
# parent=Acer L., 3 children varieties, 4 usages. stop_date=NA (still active).
mock_plant_concept_acru <- data.frame(
  concept_rf_code = "rf.37",
  concept_rf_label = "USDA Plants 2011",
  correlations = NA_character_,
  current_accepted = TRUE,
  obs_count = 0L,
  parent_name = "Acer L.",
  parent_pc_code = "pc.111387",
  party_label = "USDA-NRCS-PLANTS (organization)",
  pc_code = "pc.111478",
  plant_code = "ACRU",
  plant_description = NA_character_,
  plant_level = "Species",
  plant_name = "Acer rubrum L.",
  plant_party_comments = NA_character_,
  py_code = "py.511",
  start_date = "Thu, 25 Aug 2011 07:00:00 GMT",
  status = "accepted",
  status_rf_code = "rf.37",
  status_rf_label = "USDA Plants 2011",
  stop_date = NA_character_,
  stringsAsFactors = FALSE
)
mock_plant_concept_acru$children <- I(list(data.frame(
  pc_code = c("pc.111487", "pc.111479", "pc.111483"),
  plant_name = c(
    "Acer rubrum L. var. trilobum Torr. & A. Gray ex K. Koch",
    "Acer rubrum L. var. drummondii (Hook. & Arn. ex Nutt.) Sarg.",
    "Acer rubrum L. var. rubrum"
  ),
  stringsAsFactors = FALSE
)))
mock_plant_concept_acru$usages <- I(list(data.frame(
  class_system = c("Code", "English Common", "Scientific", "Scientific without authors"),
  plant_name = c("ACRU", "red maple", "Acer rubrum L.", "Acer rubrum"),
  status = c("Standard", "Standard", "Standard", "Standard"),
  stringsAsFactors = FALSE
)))


# ── pc.389660 · Vaccinium stamineum ──────────────────────────────────────────
# Minimal concept: no party/status/level details, children=NA, usages=NA.
# Still has obs_count=828 (widely used in plots under this code).
mock_plant_concept_vaccinium <- data.frame(
  concept_rf_code = "rf.87470",
  concept_rf_label = "Flora of Virginia 2012",
  correlations = NA_character_,
  current_accepted = NA,
  obs_count = 828L,
  parent_name = NA_character_,
  parent_pc_code = NA_character_,
  party_label = NA_character_,
  pc_code = "pc.389660",
  plant_code = NA_character_,
  plant_description = NA_character_,
  plant_level = NA_character_,
  plant_name = "Vaccinium stamineum",
  plant_party_comments = NA_character_,
  py_code = NA_character_,
  start_date = NA_character_,
  status = NA_character_,
  status_rf_code = NA_character_,
  status_rf_label = NA_character_,
  stop_date = NA_character_,
  children = NA_character_,
  usages = NA_character_,
  stringsAsFactors = FALSE
)


# ── pc.47659 · Pseudotsuga menziesii (Mirbel) Franco (Douglas-fir) ───────────
# current_accepted=FALSE (superseded in 2011 by a newer USDA Plants concept),
# obs_count=7475 (most-observed taxon in VegBank), 2 children varieties, 4 usages.
mock_plant_concept_psme <- data.frame(
  concept_rf_code = "rf.33",
  concept_rf_label = "USDA Plants 2002",
  correlations = NA_character_,
  current_accepted = FALSE,
  obs_count = 7475L,
  parent_name = "Pseudotsuga Carr.",
  parent_pc_code = "pc.5402",
  party_label = "USDA-NRCS-PLANTS (organization)",
  pc_code = "pc.47659",
  plant_code = "PSME",
  plant_description = NA_character_,
  plant_level = "Species",
  plant_name = "Pseudotsuga menziesii (Mirbel) Franco",
  plant_party_comments = NA_character_,
  py_code = "py.511",
  start_date = "Tue, 20 Aug 2002 07:00:00 GMT",
  status = "accepted",
  status_rf_code = "rf.33",
  status_rf_label = "USDA Plants 2002",
  stop_date = "Wed, 24 Aug 2011 07:00:00 GMT",
  stringsAsFactors = FALSE
)
mock_plant_concept_psme$children <- I(list(data.frame(
  pc_code = c("pc.86938", "pc.86939"),
  plant_name = c(
    "Pseudotsuga menziesii (Mirbel) Franco var. glauca (Beissn.) Franco",
    "Pseudotsuga menziesii (Mirbel) Franco var. menziesii"
  ),
  stringsAsFactors = FALSE
)))
mock_plant_concept_psme$usages <- I(list(data.frame(
  class_system = c("Code", "Scientific without authors", "English Common", "Scientific"),
  plant_name = c("PSME", "Pseudotsuga menziesii", "Douglas-fir", "Pseudotsuga menziesii (Mirbel) Franco"),
  status = c("Standard", "Standard", "Standard", "Standard"),
  stringsAsFactors = FALSE
)))

# Backward-compatible alias: tests that need a generic single-row plant mock
# use mock_plant_concept_data (= ACRU, the most fully-populated concept).
mock_plant_concept_data <- mock_plant_concept_acru
