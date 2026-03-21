# Mock community concept data for tests
# All mocks are derived from real vegbankr::vb_get_community_concepts() API responses.

# ── cc.47882 · CEGL007230 · Quercus alba - Quercus (rubra, montana) Forest ───
# current_accepted=TRUE, obs_count=294, parent present, status="accepted".
# Has long HTML comm_description, 3 correlations, 5 usages. children=NA (scalar).
mock_comm_concept_cegl007230 <- data.frame(
  cc_code = "cc.47882",
  children = NA_character_,
  comm_code = "CEGL007230",
  comm_description = paste0(
    "These forests occur in a wide elevational range, from 610 to 1372 m ",
    "(2000-4500 feet), in the Southern Blue Ridge, Blue Ridge/Piedmont transition, ",
    "and the higher ridges of the Cumberland Mountains and Ridge and Valley in ",
    "southwestern Virginia. The type occurs generally on deep soils of broad ",
    "ridgetops, exposed upper slopes and saddles, occurring less frequently on ",
    "protected lower slopes, bottoms and coves. Stands of this deciduous forest ",
    "association are dominated or codominated by <i>Quercus alba</i>, occurring ",
    "with other <i>Quercus</i> species (<i>Quercus rubra, Quercus montana, ",
    "Quercus coccinea</i>)."
  ),
  comm_level = "Association",
  comm_name = "Quercus alba - Quercus (rubra, montana) / Rhododendron calendulaceum - (Gaylussacia ursina) Forest",
  comm_party_comments = NA_character_,
  concept_rf_code = "rf.36553",
  concept_rf_label = "NatureServe Biotics 2019",
  current_accepted = TRUE,
  obs_count = 294L,
  parent_cc_code = "cc.50921",
  parent_name = "Quercus rubra - Quercus alba - Carya spp. Piedmont Forest Alliance",
  party_label = "NatureServe (organization)",
  py_code = "py.512",
  start_date = "Wed, 07 Mar 2012 00:00:00 GMT",
  status = "accepted",
  status_rf_code = "rf.36553",
  status_rf_label = "NatureServe Biotics 2019",
  stop_date = NA_character_,
  stringsAsFactors = FALSE
)
mock_comm_concept_cegl007230$correlations <- I(list(data.frame(
  cc_code = c("cc.18258", "cc.33393", "cc.7034"),
  comm_name = c(
    "Quercus alba - Quercus (rubra, prinus) / Rhododendron calendulaceum - Kalmia latifolia - (Gaylussacia ursina) Forest",
    "Quercus alba - Quercus (rubra, prinus) / Rhododendron calendulaceum - Kalmia latifolia - (Gaylussacia ursina) Forest",
    "CEGL007230"
  ),
  convergence = c("similar", "similar", "similar"),
  stringsAsFactors = FALSE
)))
mock_comm_concept_cegl007230$usages <- I(list(data.frame(
  class_system = c("Scientific", "Common", "Translated", "Code", "UID"),
  comm_name = c(
    "Quercus alba - Quercus (rubra, montana) / Rhododendron calendulaceum - (Gaylussacia ursina) Forest",
    "Appalachian Montane Oak - Hickory Forest (Typic Acidic Type)",
    "White Oak - (Northern Red Oak, Chestnut Oak) / Flame Azalea - (Bear Huckleberry) Forest",
    "CEGL007230",
    "ELEMENT_GLOBAL.2.683502"
  ),
  status = c("Standard", "Standard", "Standard", "Standard", "Standard"),
  stringsAsFactors = FALSE
)))


# ── cc.38611 · Brachypodium distachyon – Bromus diandrus / Quercus douglasii ─
# current_accepted=FALSE, status="undetermined", obs_count=17, no parent.
# No comm_code, no comm_description, no correlations (scalar NA). 2 usages.
mock_comm_concept_brachypodium <- data.frame(
  cc_code = "cc.38611",
  children = NA_character_,
  comm_code = NA_character_,
  comm_description = NA_character_,
  comm_level = "Association",
  comm_name = "Brachypodium distachyon \u2013 Bromus diandrus / Quercus douglasii Semi-natural Association",
  comm_party_comments = NA_character_,
  concept_rf_code = "rf.87488",
  concept_rf_label = "MCV2",
  correlations = NA_character_,
  current_accepted = FALSE,
  obs_count = 17L,
  parent_cc_code = NA_character_,
  parent_name = NA_character_,
  party_label = "Sikes, Kendra",
  py_code = "py.198292",
  start_date = "Wed, 09 Dec 2015 09:37:50 GMT",
  status = "undetermined",
  status_rf_code = NA_character_,
  status_rf_label = NA_character_,
  stop_date = NA_character_,
  stringsAsFactors = FALSE
)
mock_comm_concept_brachypodium$usages <- I(list(data.frame(
  class_system = c("Scientific", "CODE"),
  comm_name = c(
    "Brachypodium distachyon \u2013 Bromus diandrus / Quercus douglasii Semi-natural Association",
    "42.026.24"
  ),
  status = c("standard", "standard"),
  stringsAsFactors = FALSE
)))


# ── cc.133 · VII · Sparse Vegetation Class ───────────────────────────────────
# current_accepted=FALSE, status="accepted" (superseded), obs_count=0.
# comm_code="VII" (same as comm_name), comm_level="Class" (high-level unit).
# Has 3 children, no correlations (scalar NA), 2 usages. stop_date set.
mock_comm_concept_vii <- data.frame(
  cc_code = "cc.133",
  comm_code = "VII",
  comm_description = NA_character_,
  comm_level = "Class",
  comm_name = "VII",
  comm_party_comments = NA_character_,
  concept_rf_code = "rf.32",
  concept_rf_label = "EcoArt 2002",
  correlations = NA_character_,
  current_accepted = FALSE,
  obs_count = 0L,
  parent_cc_code = NA_character_,
  parent_name = NA_character_,
  party_label = "NatureServe (organization)",
  py_code = "py.512",
  start_date = "Fri, 14 Nov 1997 08:00:00 GMT",
  status = "accepted",
  status_rf_code = "rf.32",
  status_rf_label = "EcoArt 2002",
  stop_date = "Wed, 01 May 2019 00:00:00 GMT",
  stringsAsFactors = FALSE
)
mock_comm_concept_vii$children <- I(list(data.frame(
  cc_code = c("cc.145", "cc.146", "cc.147"),
  comm_name = c("VII.B", "VII.C", "VII.A"),
  stringsAsFactors = FALSE
)))
mock_comm_concept_vii$usages <- I(list(data.frame(
  class_system = c("Code", "Scientific"),
  comm_name = c("VII", "Sparse Vegetation"),
  status = c("standard", "standard"),
  stringsAsFactors = FALSE
)))

# Backward-compatible alias: tests that need a generic single-row community mock
# use mock_comm_concept_data (= CEGL007230, the most fully-populated concept).
mock_comm_concept_data <- mock_comm_concept_cegl007230