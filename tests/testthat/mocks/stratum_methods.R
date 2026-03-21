# Mock stratum method data for tests
# Real API responses from vegbankr::vb_get_stratum_methods(vb_code)

# sm.1 — Carolina Vegetation Survey: rf.27/CVS Protocol, 11 stratum types
mock_stratum_method_sm1 <- data.frame(
  rf_code = "rf.27",
  rf_label = "CVS Protocol",
  sm_code = "sm.1",
  stratum_assignment = NA_character_,
  stratum_method_description = paste0(
    "Stratum heights are constructed to reflect the vegetation of the plot.  ",
    "Foliage is then determined to belong to a stratum based only on height of ",
    "the foliage, not the individual.  Lifeform is not considered.  An ",
    "individual's foliage may be broken into different strata."
  ),
  stratum_method_name = "Carolina Vegetation Survey",
  stringsAsFactors = FALSE
)
mock_stratum_method_sm1$stratum_types <- I(list(data.frame(
  stratum_description = c(
    "Foliage generally greater than 35m high",
    "Foliage generally less than 0.5m high",
    "Foliage generally 0.5-6m high",
    "Foliage generally 15-35m high",
    "Foliage generally 6-15m high",
    "No vertical splitting of a plot, but rather a horizontal portion of a plot, generally a 10m x 10m square, though sometimes 5m x 20m",
    "Foliage generally less than 0.5m high",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_
  ),
  stratum_index = c("E", "H", "S", "C", "U", "mod", "H", "A", "F", "O", "T"),
  stratum_name = c(
    "Emergent", "Herb", "Shrub", "Canopy", "Understory",
    "module", "Herb", "Aquatic submerged", "Floating", "Other", "Tree"
  ),
  sy_code = c(
    "sy.1", "sy.2", "sy.3", "sy.4", "sy.5",
    "sy.217663", "sy.217664", "sy.217688", "sy.218016", "sy.218017", "sy.218018"
  ),
  stringsAsFactors = FALSE
)))

# sm.5 — National Park Service: rf.29/NPS Methodology, 19 stratum types
mock_stratum_method_sm5 <- data.frame(
  rf_code = "rf.29",
  rf_label = "NPS Methodology",
  sm_code = "sm.5",
  stratum_assignment = NA_character_,
  stratum_method_description = paste0(
    "Strata are defined based on both lifeform and height.  An individual's ",
    "foliage belongs to one stratum only, as determined by the individual's height."
  ),
  stratum_method_name = "National Park Service",
  stringsAsFactors = FALSE
)
mock_stratum_method_sm5$stratum_types <- I(list(data.frame(
  stratum_description = c(
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "No vertical splitting of a plot, but rather a horizontal portion of a plot, generally a 10m x 10m square, though sometimes 5m x 20m",
    NA_character_,
    NA_character_,
    "Submergent aquatic. may include submerged seedlings of typical emergent and floating-leaved aquatic plants.",
    "Floating-leaved aquatic. Detached floating plants are treated as detritus (unvegetated surface/litter and duff layer).",
    "Emergent aquatic. Does not include trees, shrubs and vines. may include typical submergent and floating-leaved aquatic with uppermost leaves emergent due to drawdown (e.g., Nuphar spp.)."
  ),
  stratum_index = c(
    "T1", "T2", "T3", "S1", "S2", "S3", "H", "N", "V", "E",
    "Unkn", "S", "T", "mod", "TC", "H", "A3", "A2", "A1"
  ),
  stratum_name = c(
    "Emergent", "Canopy", "Sub-Canopy", "Tall Shrub", "Short Shrub", "Dwarf Shrub",
    "Herbaceous", "Nonvascular", "Vine/Liana", "Epiphyte", "Unknown stratum", "Shrub",
    "Tree", "module", "Total-Calculated", "Herbaceous", "submerged aquatic",
    "floating-leaved aquatic", "emergent aquatic"
  ),
  sy_code = c(
    "sy.33", "sy.34", "sy.35", "sy.36", "sy.37", "sy.38", "sy.39", "sy.40",
    "sy.41", "sy.42", "sy.773", "sy.774", "sy.775", "sy.217661", "sy.218050",
    "sy.218003", "sy.218004", "sy.218005", "sy.218006"
  ),
  stringsAsFactors = FALSE
)))

# sm.329 — No Strata: no rf_code/rf_label, single stratum type (module)
mock_stratum_method_sm329 <- data.frame(
  rf_code = NA_character_,
  rf_label = NA_character_,
  sm_code = "sm.329",
  stratum_assignment = NA_character_,
  stratum_method_description = "No Strata were used in collection of data",
  stratum_method_name = "No Strata",
  stringsAsFactors = FALSE
)
mock_stratum_method_sm329$stratum_types <- I(list(data.frame(
  stratum_description = paste0(
    "No vertical splitting of a plot, but rather a horizontal portion of a plot, ",
    "generally a 10m x 10m square, though sometimes 5m x 20m"
  ),
  stratum_index = "mod",
  stratum_name = "module",
  sy_code = "sy.217662",
  stringsAsFactors = FALSE
)))

# Backward-compatible alias
mock_stratum_method_data <- mock_stratum_method_sm1
