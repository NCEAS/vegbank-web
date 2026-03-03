# Shared mock objects for detail view testing
# This file is automatically loaded by testthat before running tests

# Mock Shiny output environment
mock_output <- new.env()

# Mock Shiny session with message capture
mock_session <- list(
  sendCustomMessage = function(type, message) {
    # Store messages for verification in tests
    if (!exists("messages", envir = mock_session, inherits = FALSE)) {
      mock_session$messages <- list()
    }
    mock_session$messages[[type]] <- message
  }
)

# Mock plot observation data
# Based on real API response for ob.2948 (ACAD.143, Acadia National Park)
mock_plot_data <- data.frame(
  area = NA_real_,
  author_datum = NA_character_,
  author_e = "561385",
  author_location = "ACAD -- Acadia - S ridge Cadillac on bald S of hairpin",
  author_n = "4910423",
  author_obs_code = "ACAD.143",
  author_plot_code = "ACAD.143",
  author_zone = "19",
  auto_taxon_cover = TRUE,
  azimuth = NA_real_,
  basal_area = NA_real_,
  bryophyte_quality = "Moderate",
  cm_code = "cm.79",
  confidentiality_status = 0L,
  country = "United States",
  cover_dispersion = "contiguous",
  cover_method_name = "NPS CoverMethod",
  date_accuracy = NA_character_,
  date_entered = "Fri, 19 Nov 2004 08:00:00 GMT",
  dominant_stratum = NA_character_,
  dsg_poly = NA_character_,
  effort_level = "Thorough",
  elevation = 416L,
  elevation_accuracy = NA_real_,
  elevation_range = NA_real_,
  field_cover = NA_real_,
  field_ht = NA_real_,
  floating_cover = NA_real_,
  floristic_quality = "High but incomplete",
  growthform_1_cover = NA_real_,
  growthform_1_type = NA_character_,
  growthform_2_cover = NA_real_,
  growthform_2_type = NA_character_,
  growthform_3_cover = NA_real_,
  growthform_3_type = NA_character_,
  has_observation_synonym = NA,
  homogeneity = NA_character_,
  hydrologic_regime = "Upland",
  interp_bestfit_cc_code = "cc.21780",
  interp_bestfit_ci_code = "ci.19060",
  interp_bestfit_code = "CEGL005094",
  interp_bestfit_partyname = "Lee, Michael",
  interp_bestfit_py_code = "py.410",
  interp_bestfit_sciname = "Vaccinium angustifolium - Sorbus americana / Sibbaldiopsis tridentata Dwarf-shrubland",
  interp_current_cc_code = "cc.21780",
  interp_current_ci_code = "ci.19060",
  interp_current_code = "CEGL005094",
  interp_current_partyname = "Lee, Michael",
  interp_current_py_code = "py.410",
  interp_current_sciname = "Vaccinium angustifolium - Sorbus americana / Sibbaldiopsis tridentata Dwarf-shrubland",
  interp_orig_cc_code = "cc.6456",
  interp_orig_ci_code = "ci.297",
  interp_orig_code = "CEGL005094",
  interp_orig_partyname = "Gawler, Sue",
  interp_orig_py_code = "py.605",
  interp_orig_sciname = "Vaccinium angustifolium - Sorbus americana Dwarf-shrubland",
  landform = NA_character_,
  landscape_narrative = "Microtopography: depressions and cracks; habitat patchiness: rocks <.5m, undulating granite bald. -- Current condition = 2, , .  Landscape Quality =3.",
  latitude = 44.346509409,
  layout_narrative = NA_character_,
  lichen_quality = "Moderate",
  location_accuracy = NA_character_,
  location_narrative = NA_character_,
  longitude = -68.229339874,
  max_slope_aspect = NA_real_,
  max_slope_gradient = NA_real_,
  method_narrative = NA_character_,
  min_slope_aspect = NA_real_,
  min_slope_gradient = NA_real_,
  name_other = NA_character_,
  nonvascular_cover = NA_real_,
  nonvascular_ht = NA_real_,
  number_of_taxa = NA_integer_,
  ob_code = "ob.2948",
  ob_notes_mgt = FALSE,
  ob_notes_public = FALSE,
  ob_revisions = FALSE,
  obs_end_date = "Tue, 04 Aug 1998 07:00:00 GMT",
  obs_start_date = "Tue, 04 Aug 1998 07:00:00 GMT",
  observation_narrative = "extensive",
  organic_depth = NA_real_,
  original_data = NA_character_,
  parent_pl_code = NA_character_,
  percent_bare_soil = 0L,
  percent_bed_rock = 0L,
  percent_litter = 0L,
  percent_other = 0L,
  percent_rock_gravel = 0L,
  percent_water = 0L,
  percent_wood = 0L,
  permanence = FALSE,
  phenologic_aspect = NA_character_,
  pj_code = "pj.340",
  pl_code = "pl.3004",
  pl_notes_mgt = FALSE,
  pl_notes_public = FALSE,
  pl_revisions = FALSE,
  placement_method = NA_character_,
  plot_validation_level = 2L,
  previous_ob_code = NA_character_,
  project_name = "Acadia National Park",
  replaced_by_ob_code = NA_character_,
  representativeness = "excellent",
  rf_code = NA_character_,
  rf_label = NA_character_,
  rock_type = "igneous, unspecified",
  shape = "Rectangular",
  shore_distance = NA_real_,
  shrub_cover = NA_real_,
  shrub_ht = NA_real_,
  slope_aspect = 282L,
  slope_gradient = 12L,
  sm_code = "sm.5",
  soil_depth = NA_real_,
  soil_drainage = "moderately well drained",
  soil_moisture_regime = NA_character_,
  soil_taxon_src = NA_character_,
  st_code = NA_character_,
  stand_maturity = NA_character_,
  stand_size = NA_character_,
  state_province = "Maine",
  stem_observation_area = NA_real_,
  stem_sample_method = "full census",
  stem_size_limit = 10L,
  stratum_assignment = NA_character_,
  stratum_method_description = "Strata are defined based on both lifeform and height.  An individual's foliage belongs to one stratum only, as determined by the individual's height.",
  stratum_method_name = "National Park Service",
  submerged_cover = NA_real_,
  submerged_ht = NA_real_,
  successional_status = NA_character_,
  surficial_deposits = NA_character_,
  taxon_count = 11L,
  taxon_importance_count = 22L,
  taxon_importance_count_returned = 5L,
  taxon_observation_area = NA_real_,
  top_taxon1_name = "Lichen",
  top_taxon2_name = "Sibbaldiopsis tridentata",
  top_taxon3_name = "Vaccinium boreale",
  top_taxon4_name = "Umbilicaria",
  top_taxon5_name = "Minuartia glabra",
  topo_position = "High level",
  total_cover = NA_real_,
  tree_cover = NA_real_,
  tree_ht = NA_real_,
  water_depth = NA_real_,
  water_salinity = NA_character_,
  year = "1998",
  stringsAsFactors = FALSE
)

mock_plot_data$top_taxon_observations <- I(list(data.frame(
  cover = c(0.1875, 0.1875, 0.5625, 0.5625, 0.0625),
  pc_code = c("pc.10653", "pc.10653", "pc.92211", "pc.92211", "pc.22506"),
  plant_name = c("Aronia melanocarpa", "Aronia melanocarpa", "Bryophyta", "Bryophyta", "Danthonia spicata"),
  sr_code = c(NA_character_, "sr.17464", NA_character_, "sr.17458", NA_character_),
  stratum_name = c("-all-", "Dwarf Shrub", "-all-", "Nonvascular", "-all-"),
  tm_code = c("tm.67087", "tm.67088", "tm.67086", "tm.67085", "tm.67089"),
  to_code = c("to.64984", "to.64984", "to.64983", "to.64983", "to.64985"),
  stringsAsFactors = FALSE
)))

mock_plot_data$top_classifications <- I(list(data.frame(
  cc_code = "cc.46260",
  ci_code = "ci.297",
  cl_code = "cl.1553",
  comm_code = "CEGL005094",
  comm_name = "Vaccinium angustifolium - Sorbus americana / Sibbaldiopsis tridentata Dwarf-shrubland",
  stringsAsFactors = FALSE
)))

mock_plot_data$disturbances <- I(list(data.frame(
  age = NA_character_,
  comment = "Fire: . Wind: no soil accumulation. Cutting: . Agric: . Impoundment: .",
  extent = NA_character_,
  intensity = NA_character_,
  type = "unknown",
  stringsAsFactors = FALSE
)))

mock_plot_data$soils <- I(list(data.frame(
  base_saturation = NA_real_,
  clay = NA_real_,
  coarse = NA_real_,
  color = NA_character_,
  depth_bottom = NA_real_,
  depth_top = NA_real_,
  description = "Profile: O=6cm,  ;A=4cm, 7.5yr2.5/1, ; E=13cm, 7.5yr3/1, ; B=0cm, , .  Stoniness: v-little. pH=5.",
  exchange_capacity = NA_real_,
  horizon = "unknown",
  organic = NA_real_,
  ph = NA_real_,
  sand = NA_real_,
  silt = NA_real_,
  texture = "Sands: Sand",
  stringsAsFactors = FALSE
)))

mock_plot_data$named_places <- I(list(data.frame(
  code = c("n-us-me", "n-us", "TME0863"),
  description = c(NA_character_, NA_character_, "Maine, Southern Lat:44 deg 15 min 00 sec Eastern Long: -068 deg 07 min 30 sec"),
  name = c("Maine", "United States", "Seal Harbor"),
  np_code = c("np.382", "np.360", "np.23226"),
  system = c("region|state|province", "area|country|territory", "quadrangle"),
  stringsAsFactors = FALSE
)))

# Mock plot observation: ob.2949 (ACAD.144) — seasonally flooded, null observation_narrative,
# slope_aspect/gradient = 0, peat soil, no rock_type, surficial deposits present
mock_plot_data_acad144 <- data.frame(
  area = NA_real_,
  author_datum = NA_character_,
  author_e = "561574",
  author_location = "ACAD -- Acadia - S Ridge Trail of Cadillac, Featherbed",
  author_n = "4909704",
  author_obs_code = "ACAD.144",
  author_plot_code = "ACAD.144",
  author_zone = "19",
  auto_taxon_cover = TRUE,
  azimuth = NA_real_,
  basal_area = NA_real_,
  bryophyte_quality = "Moderate",
  cm_code = "cm.79",
  confidentiality_status = 0L,
  country = "United States",
  cover_dispersion = "contiguous",
  cover_method_name = "NPS CoverMethod",
  date_accuracy = NA_character_,
  date_entered = "Fri, 19 Nov 2004 08:00:00 GMT",
  dominant_stratum = NA_character_,
  dsg_poly = NA_character_,
  effort_level = "Thorough",
  elevation = 300L,
  elevation_accuracy = NA_real_,
  elevation_range = NA_real_,
  field_cover = NA_real_,
  field_ht = NA_real_,
  floating_cover = NA_real_,
  floristic_quality = "High but incomplete",
  growthform_1_cover = NA_real_,
  growthform_1_type = NA_character_,
  growthform_2_cover = NA_real_,
  growthform_2_type = NA_character_,
  growthform_3_cover = NA_real_,
  growthform_3_type = NA_character_,
  has_observation_synonym = NA,
  homogeneity = NA_character_,
  hydrologic_regime = "Seasonally flooded",
  interp_bestfit_cc_code = "cc.20652",
  interp_bestfit_ci_code = "ci.19308",
  interp_bestfit_code = "CEGL006153",
  interp_bestfit_partyname = "Lee, Michael",
  interp_bestfit_py_code = "py.410",
  interp_bestfit_sciname = "Typha (angustifolia, latifolia) - (Schoenoplectus spp.) Eastern Herbaceous Vegetation",
  interp_current_cc_code = "cc.20652",
  interp_current_ci_code = "ci.19308",
  interp_current_code = "CEGL006153",
  interp_current_partyname = "Lee, Michael",
  interp_current_py_code = "py.410",
  interp_current_sciname = "Typha (angustifolia, latifolia) - (Schoenoplectus spp.) Eastern Herbaceous Vegetation",
  interp_orig_cc_code = "cc.6701",
  interp_orig_ci_code = "ci.298",
  interp_orig_code = "CEGL006153",
  interp_orig_partyname = "Gawler, Sue",
  interp_orig_py_code = "py.605",
  interp_orig_sciname = "Typha (angustifolia, latifolia) - (Schoenoplectus spp.) Eastern Herbaceous Vegetation",
  landform = NA_character_,
  landscape_narrative = "Microtopography: ; habitat patchiness: uniform. -- Current condition = 1, , .  Landscape Quality =3.",
  latitude = 44.340020888,
  layout_narrative = NA_character_,
  lichen_quality = "Moderate",
  location_accuracy = NA_character_,
  location_narrative = NA_character_,
  longitude = -68.227053809,
  max_slope_aspect = NA_real_,
  max_slope_gradient = NA_real_,
  method_narrative = NA_character_,
  min_slope_aspect = NA_real_,
  min_slope_gradient = NA_real_,
  name_other = NA_character_,
  nonvascular_cover = NA_real_,
  nonvascular_ht = NA_real_,
  number_of_taxa = NA_integer_,
  ob_code = "ob.2949",
  ob_notes_mgt = FALSE,
  ob_notes_public = FALSE,
  ob_revisions = FALSE,
  obs_end_date = "Tue, 04 Aug 1998 07:00:00 GMT",
  obs_start_date = "Tue, 04 Aug 1998 07:00:00 GMT",
  observation_narrative = NA_character_,        # NULL in API — important edge case
  organic_depth = NA_real_,
  original_data = NA_character_,
  parent_pl_code = NA_character_,
  percent_bare_soil = 0L,
  percent_bed_rock = 0L,
  percent_litter = 0L,
  percent_other = 0L,
  percent_rock_gravel = 0L,
  percent_water = 0L,
  percent_wood = 0L,
  permanence = FALSE,
  phenologic_aspect = NA_character_,
  pj_code = "pj.340",
  pl_code = "pl.3005",
  pl_notes_mgt = FALSE,
  pl_notes_public = FALSE,
  pl_revisions = FALSE,
  placement_method = NA_character_,
  plot_validation_level = 2L,
  previous_ob_code = NA_character_,
  project_name = "Acadia National Park",
  replaced_by_ob_code = NA_character_,
  representativeness = "the nc is uniform. this is the only one we know",
  rf_code = NA_character_,
  rf_label = NA_character_,
  rock_type = NA_character_,                    # NULL in API — no rock type
  shape = "Rectangular",
  shore_distance = NA_real_,
  shrub_cover = NA_real_,
  shrub_ht = NA_real_,
  slope_aspect = 0L,                            # Zero values — should be treated as valid
  slope_gradient = 0L,
  sm_code = "sm.5",
  soil_depth = NA_real_,
  soil_drainage = "very poorly drained",
  soil_moisture_regime = NA_character_,
  soil_taxon_src = NA_character_,
  st_code = NA_character_,
  stand_maturity = NA_character_,
  stand_size = NA_character_,
  state_province = "Maine",
  stem_observation_area = NA_real_,
  stem_sample_method = "full census",
  stem_size_limit = 10L,
  stratum_assignment = NA_character_,
  stratum_method_description = "Strata are defined based on both lifeform and height.  An individual's foliage belongs to one stratum only, as determined by the individual's height.",
  stratum_method_name = "National Park Service",
  submerged_cover = NA_real_,
  submerged_ht = NA_real_,
  successional_status = NA_character_,
  surficial_deposits = "Glacial Deposits: Till",  # Present in this observation
  taxon_count = 8L,
  taxon_importance_count = 16L,
  taxon_importance_count_returned = 5L,
  taxon_observation_area = NA_real_,
  top_taxon1_name = "Juncus militaris",
  top_taxon2_name = "Sphagnum",
  top_taxon3_name = "Myrica gale",
  top_taxon4_name = "Sphagnum pylaesii",
  top_taxon5_name = "Carex oligosperma",
  topo_position = "Low level",
  total_cover = NA_real_,
  tree_cover = NA_real_,
  tree_ht = NA_real_,
  water_depth = NA_real_,
  water_salinity = NA_character_,
  year = "1998",
  stringsAsFactors = FALSE
)

mock_plot_data_acad144$top_taxon_observations <- I(list(data.frame(
  cover = c(2.9375, 2.9375, 1.375, 1.375, 0.1875),
  pc_code = c("pc.16364", "pc.16364", "pc.17908", "pc.17908", "pc.24066"),
  plant_name = c("Carex oligosperma", "Carex oligosperma", "Chamaedaphne calyculata", "Chamaedaphne calyculata", "Drosera intermedia"),
  sr_code = c(NA_character_, "sr.17480", "sr.17486", NA_character_, "sr.17480"),
  stratum_name = c("-all-", "Herbaceous", "Dwarf Shrub", "-all-", "Herbaceous"),
  tm_code = c("tm.67106", "tm.67105", "tm.67108", "tm.67107", "tm.67109"),
  to_code = c("to.64993", "to.64993", "to.64994", "to.64994", "to.64995"),
  stringsAsFactors = FALSE
)))

mock_plot_data_acad144$top_classifications <- I(list(data.frame(
  cc_code = "cc.47167",
  ci_code = "ci.298",
  cl_code = "cl.1554",
  comm_code = "CEGL006153",
  comm_name = "Typha (angustifolia, latifolia) - (Schoenoplectus spp.) Eastern Marsh",
  stringsAsFactors = FALSE
)))

mock_plot_data_acad144$disturbances <- I(list(data.frame(
  age = NA_character_,
  comment = "Fire: . Wind: . Cutting: . Agric: . Impoundment: basin in bedrock.",
  extent = NA_character_,
  intensity = NA_character_,
  type = "unknown",
  stringsAsFactors = FALSE
)))

mock_plot_data_acad144$soils <- I(list(data.frame(
  base_saturation = NA_real_,
  clay = NA_real_,
  coarse = NA_real_,
  color = NA_character_,
  depth_bottom = NA_real_,
  depth_top = NA_real_,
  description = "pH=5.4; peat depth=51cm; vonPost=2.",
  exchange_capacity = NA_real_,
  horizon = "unknown",
  organic = NA_real_,
  ph = NA_real_,
  sand = NA_real_,
  silt = NA_real_,
  texture = NA_character_,                     # NULL in API — no texture for peat
  stringsAsFactors = FALSE
)))

mock_plot_data_acad144$named_places <- I(list(data.frame(
  code = c("n-us-me", "TME0863", "n-us"),
  description = c(NA_character_, "Maine, Southern Lat:44 deg 15 min 00 sec Eastern Long: -068 deg 07 min 30 sec", NA_character_),
  name = c("Maine", "Seal Harbor", "United States"),
  np_code = c("np.382", "np.23226", "np.360"),
  system = c("region|state|province", "quadrangle", "area|country|territory"),
  stringsAsFactors = FALSE
)))

# Mock plot observation: ob.2950 (ACAD.145) — woodland, 28 taxa, loamy sand soil,
# topo_position = "Midslope", slope_aspect = 270
mock_plot_data_acad145 <- data.frame(
  area = NA_real_,
  author_datum = NA_character_,
  author_e = "561567",
  author_location = "ACAD -- Acadia - S ridge of Cadillac, W of Featherbed",
  author_n = "4909655",
  author_obs_code = "ACAD.145",
  author_plot_code = "ACAD.145",
  author_zone = "19",
  auto_taxon_cover = TRUE,
  azimuth = NA_real_,
  basal_area = NA_real_,
  bryophyte_quality = "Moderate",
  cm_code = "cm.79",
  confidentiality_status = 0L,
  country = "United States",
  cover_dispersion = "contiguous",
  cover_method_name = "NPS CoverMethod",
  date_accuracy = NA_character_,
  date_entered = "Fri, 19 Nov 2004 08:00:00 GMT",
  dominant_stratum = NA_character_,
  dsg_poly = NA_character_,
  effort_level = "Thorough",
  elevation = 297L,
  elevation_accuracy = NA_real_,
  elevation_range = NA_real_,
  field_cover = NA_real_,
  field_ht = NA_real_,
  floating_cover = NA_real_,
  floristic_quality = "High but incomplete",
  growthform_1_cover = NA_real_,
  growthform_1_type = NA_character_,
  growthform_2_cover = NA_real_,
  growthform_2_type = NA_character_,
  growthform_3_cover = NA_real_,
  growthform_3_type = NA_character_,
  has_observation_synonym = NA,
  homogeneity = NA_character_,
  hydrologic_regime = "Upland",
  interp_bestfit_cc_code = "cc.22120",
  interp_bestfit_ci_code = "ci.19259",
  interp_bestfit_code = "CEGL006116",
  interp_bestfit_partyname = "Lee, Michael",
  interp_bestfit_py_code = "py.410",
  interp_bestfit_sciname = "Pinus rigida / (Quercus ilicifolia) / Photinia melanocarpa / Deschampsia flexuosa Woodland",
  interp_current_cc_code = "cc.22120",
  interp_current_ci_code = "ci.19259",
  interp_current_code = "CEGL006116",
  interp_current_partyname = "Lee, Michael",
  interp_current_py_code = "py.410",
  interp_current_sciname = "Pinus rigida / (Quercus ilicifolia) / Photinia melanocarpa / Deschampsia flexuosa Woodland",
  interp_orig_cc_code = "cc.6673",
  interp_orig_ci_code = "ci.299",
  interp_orig_code = "CEGL006116",
  interp_orig_partyname = "Gawler, Sue",
  interp_orig_py_code = "py.605",
  interp_orig_sciname = "Pinus rigida / Photinia melanocarpa / Deschampsia flexuosa - Schizachyrium scoparium Woodland",
  landform = NA_character_,
  landscape_narrative = "Microtopography: fairly flat and uniform; habitat patchiness: open rock and areas of soil accumulation. -- Current condition = 1, , .  Landscape Quality =2.",
  latitude = 44.339580377,
  layout_narrative = NA_character_,
  lichen_quality = "Moderate",
  location_accuracy = NA_character_,
  location_narrative = NA_character_,
  longitude = -68.227147405,
  max_slope_aspect = NA_real_,
  max_slope_gradient = NA_real_,
  method_narrative = NA_character_,
  min_slope_aspect = NA_real_,
  min_slope_gradient = NA_real_,
  name_other = NA_character_,
  nonvascular_cover = NA_real_,
  nonvascular_ht = NA_real_,
  number_of_taxa = NA_integer_,
  ob_code = "ob.2950",
  ob_notes_mgt = FALSE,
  ob_notes_public = FALSE,
  ob_revisions = FALSE,
  obs_end_date = "Tue, 04 Aug 1998 07:00:00 GMT",
  obs_start_date = "Tue, 04 Aug 1998 07:00:00 GMT",
  observation_narrative = "small",
  organic_depth = NA_real_,
  original_data = NA_character_,
  parent_pl_code = NA_character_,
  percent_bare_soil = 0L,
  percent_bed_rock = 0L,
  percent_litter = 0L,
  percent_other = 0L,
  percent_rock_gravel = 0L,
  percent_water = 0L,
  percent_wood = 0L,
  permanence = FALSE,
  phenologic_aspect = NA_character_,
  pj_code = "pj.340",
  pl_code = "pl.3006",
  pl_notes_mgt = FALSE,
  pl_notes_public = FALSE,
  pl_revisions = FALSE,
  placement_method = NA_character_,
  plot_validation_level = 2L,
  previous_ob_code = NA_character_,
  project_name = "Acadia National Park",
  replaced_by_ob_code = NA_character_,
  representativeness = "more thickety than most PPW's.",
  rf_code = NA_character_,
  rf_label = NA_character_,
  rock_type = "igneous, unspecified",
  shape = "Rectangular",
  shore_distance = NA_real_,
  shrub_cover = NA_real_,
  shrub_ht = NA_real_,
  slope_aspect = 270L,
  slope_gradient = 2L,
  sm_code = "sm.5",
  soil_depth = NA_real_,
  soil_drainage = "somewhat excessively drained",
  soil_moisture_regime = NA_character_,
  soil_taxon_src = NA_character_,
  st_code = NA_character_,
  stand_maturity = NA_character_,
  stand_size = NA_character_,
  state_province = "Maine",
  stem_observation_area = NA_real_,
  stem_sample_method = "full census",
  stem_size_limit = 10L,
  stratum_assignment = NA_character_,
  stratum_method_description = "Strata are defined based on both lifeform and height.  An individual's foliage belongs to one stratum only, as determined by the individual's height.",
  stratum_method_name = "National Park Service",
  submerged_cover = NA_real_,
  submerged_ht = NA_real_,
  successional_status = NA_character_,
  surficial_deposits = NA_character_,
  taxon_count = 28L,
  taxon_importance_count = 63L,
  taxon_importance_count_returned = 5L,
  taxon_observation_area = NA_real_,
  top_taxon1_name = "Pinus rigida",
  top_taxon2_name = "Vaccinium angustifolium",
  top_taxon3_name = "Gaylussacia baccata",
  top_taxon4_name = "Kalmia angustifolia",
  top_taxon5_name = "Rhododendron canadense",
  topo_position = "Midslope",
  total_cover = NA_real_,
  tree_cover = NA_real_,
  tree_ht = NA_real_,
  water_depth = NA_real_,
  water_salinity = NA_character_,
  year = "1998",
  stringsAsFactors = FALSE
)

mock_plot_data_acad145$top_taxon_observations <- I(list(data.frame(
  cover = c(3.9375, 3.9375, 2.25, 2.25, 0.375),
  pc_code = c("pc.6792", "pc.6792", "pc.7189", "pc.7189", "pc.2095"),
  plant_name = c("Abies balsamea", "Abies balsamea", "Acer rubrum", "Acer rubrum", "Amelanchier"),
  sr_code = c(NA_character_, "sr.17498", "sr.17495", NA_character_, "sr.17503"),
  stratum_name = c("-all-", "Herbaceous", "Short Shrub", "-all-", "Dwarf Shrub"),
  tm_code = c("tm.67171", "tm.67170", "tm.67172", "tm.67173", "tm.67175"),
  to_code = c("to.65023", "to.65023", "to.65024", "to.65024", "to.65025"),
  stringsAsFactors = FALSE
)))

mock_plot_data_acad145$top_classifications <- I(list(data.frame(
  cc_code = "cc.47130",
  ci_code = "ci.299",
  cl_code = "cl.1555",
  comm_code = "CEGL006116",
  comm_name = "Pinus rigida / (Quercus ilicifolia) / Aronia melanocarpa / Deschampsia flexuosa Woodland",
  stringsAsFactors = FALSE
)))

mock_plot_data_acad145$disturbances <- I(list(data.frame(
  age = NA_character_,
  comment = "Fire: large, sheety flakes. Wind: fairly sheltered here. Cutting: . Agric: . Impoundment: bedrock basins N and E of here.",
  extent = NA_character_,
  intensity = NA_character_,
  type = "unknown",
  stringsAsFactors = FALSE
)))

mock_plot_data_acad145$soils <- I(list(data.frame(
  base_saturation = NA_real_,
  clay = NA_real_,
  coarse = NA_real_,
  color = NA_character_,
  depth_bottom = NA_real_,
  depth_top = NA_real_,
  description = "Profile: O=15cm,  ;A=2cm, 10yr, ; E=0cm, , ; B=0cm, , .  Stoniness: v-little. pH=5.6.",
  exchange_capacity = NA_real_,
  horizon = "unknown",
  organic = NA_real_,
  ph = NA_real_,
  sand = NA_real_,
  silt = NA_real_,
  texture = "Loamy Sands: Loamy Sand",
  stringsAsFactors = FALSE
)))

mock_plot_data_acad145$named_places <- I(list(data.frame(
  code = c("TME0863", "n-us", "n-us-me"),
  description = c("Maine, Southern Lat:44 deg 15 min 00 sec Eastern Long: -068 deg 07 min 30 sec", NA_character_, NA_character_),
  name = c("Seal Harbor", "United States", "Maine"),
  np_code = c("np.23226", "np.360", "np.382"),
  system = c("quadrangle", "area|country|territory", "region|state|province"),
  stringsAsFactors = FALSE
)))

# Mock for testing synonym notification banner:
# has_observation_synonym = TRUE and replaced_by_ob_code is set
mock_plot_data_with_synonym <- mock_plot_data
mock_plot_data_with_synonym$has_observation_synonym <- TRUE
mock_plot_data_with_synonym$replaced_by_ob_code <- "ob.9999"
mock_plot_data_with_synonym$ob_code <- "ob.2948"

# Mock for testing a plot that has a reference and a previous observation code
mock_plot_data_with_reference <- mock_plot_data
mock_plot_data_with_reference$rf_code <- "rf.501"
mock_plot_data_with_reference$rf_label <- "Gawler, S. 2000."
mock_plot_data_with_reference$previous_ob_code <- "ob.2100"

# Multi-row mock combining all three ACAD observations (for table/map tests)
# Build scalar columns only first, then assign nested list columns
mock_plot_observations_multi <- rbind(
  mock_plot_data[, setdiff(names(mock_plot_data), c("top_taxon_observations", "top_classifications", "disturbances", "soils", "named_places"))],
  mock_plot_data_acad144[, setdiff(names(mock_plot_data_acad144), c("top_taxon_observations", "top_classifications", "disturbances", "soils", "named_places"))],
  mock_plot_data_acad145[, setdiff(names(mock_plot_data_acad145), c("top_taxon_observations", "top_classifications", "disturbances", "soils", "named_places"))]
)
mock_plot_observations_multi$top_taxon_observations <- list(
  mock_plot_data$top_taxon_observations[[1]],
  mock_plot_data_acad144$top_taxon_observations[[1]],
  mock_plot_data_acad145$top_taxon_observations[[1]]
)
mock_plot_observations_multi$top_classifications <- list(
  mock_plot_data$top_classifications[[1]],
  mock_plot_data_acad144$top_classifications[[1]],
  mock_plot_data_acad145$top_classifications[[1]]
)
mock_plot_observations_multi$disturbances <- list(
  mock_plot_data$disturbances[[1]],
  mock_plot_data_acad144$disturbances[[1]],
  mock_plot_data_acad145$disturbances[[1]]
)
mock_plot_observations_multi$soils <- list(
  mock_plot_data$soils[[1]],
  mock_plot_data_acad144$soils[[1]],
  mock_plot_data_acad145$soils[[1]]
)

# Mock community concept data
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

# Mock taxon observation data
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

# Mock community classification data with new schema
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

# Mock project data
mock_project_data <- data.frame(
  project_id = 123,
  project_name = "Test Project",
  project_description = "A test project description",
  start_date = "2021-01-01",
  stop_date = "2022-12-31",
  obs_count = 42,
  last_plot_added_date = "2022-10-15",
  stringsAsFactors = FALSE
)

# Mock party data
mock_party_data <- data.frame(
  party_id = 123,
  given_name = "John",
  middle_name = "Q",
  surname = "Public",
  salutation = "Dr.",
  organization_name = "Example Organization",
  contact_instructions = "Email: john.public@example.org",
  py_code = "py.123",
  stringsAsFactors = FALSE
)

# Mock plant concept data
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

# Mock reference data
mock_reference_data <- data.frame(
  degree = NA,
  doi = NA,
  full_citation = "Example Author. 2020. Example citation.",
  isbn = NA,
  journal = NA,
  publication_date = "Wed, 01 Jan 2020 08:00:00 GMT",
  publication_place = "Example City",
  publisher = "Example Publisher",
  reference_type = "Book",
  rf_code = "rf.123",
  rf_label = "Example Author. 2020.",
  short_name = "Example Author. 2020.",
  title = "Example Title",
  total_pages = 250,
  url = NA,
  stringsAsFactors = FALSE
)
