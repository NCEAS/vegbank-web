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

# Mock plot observation: ob.3776 (GRSM.225) — Great Smoky Mountains National Park,
# confidential location, float elevation, Tennessee, null soils, permanence = TRUE,
# slope_aspect = 0 (valid zero), confidentiality_status = 1
mock_plot_data_grsm225 <- data.frame(
  area = 1000,
  author_datum = "<confidential>",
  author_e = "<confidential>",
  author_location = "<confidential>",
  author_n = "<confidential>",
  author_obs_code = "GRSM.225",
  author_plot_code = "GRSM.225",
  author_zone = "<confidential>",
  auto_taxon_cover = FALSE,
  azimuth = NA_real_,
  basal_area = NA_real_,
  bryophyte_quality = "Very incomplete",
  cm_code = "cm.79",
  confidentiality_status = 1L,                 # Confidential observation
  country = "United States",
  cover_dispersion = "contiguous",
  cover_method_name = "NPS CoverMethod",
  date_accuracy = NA_character_,
  date_entered = "Fri, 19 Nov 2004 08:00:00 GMT",
  dominant_stratum = NA_character_,
  dsg_poly = NA_character_,
  effort_level = "Thorough",
  elevation = 929.639998586947,                # Float elevation — stored as double
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
  interp_bestfit_cc_code = "cc.24082",
  interp_bestfit_ci_code = "ci.19583",
  interp_bestfit_code = "CEGL007695",
  interp_bestfit_partyname = "Lee, Michael",
  interp_bestfit_py_code = "py.410",
  interp_bestfit_sciname = "Aesculus flava - Acer saccharum - (Fraxinus americana, Tilia americana var. heterophylla) / Hydrophyllum canadense - Solidago flexicaulis Forest",
  interp_current_cc_code = "cc.24082",
  interp_current_ci_code = "ci.19583",
  interp_current_code = "CEGL007695",
  interp_current_partyname = "Lee, Michael",
  interp_current_py_code = "py.410",
  interp_current_sciname = "Aesculus flava - Acer saccharum - (Fraxinus americana, Tilia americana var. heterophylla) / Hydrophyllum canadense - Solidago flexicaulis Forest",
  interp_orig_cc_code = "cc.7302",
  interp_orig_ci_code = "ci.998",
  interp_orig_code = "CEGL007695",
  interp_orig_partyname = "Patterson, Karen",
  interp_orig_py_code = "py.221",
  interp_orig_sciname = "Aesculus flava - Acer saccharum - (Fraxinus americana, Tilia americana) / Hydrophyllum canadense - Solidago flexicaulis Forest",
  landform = "cove",
  landscape_narrative = "Some rock outcrops within plot -- Ridges surrounding the cove are pine and rhododendron community.",
  latitude = 35.58,
  layout_narrative = NA_character_,
  lichen_quality = "Very incomplete",
  location_accuracy = NA_character_,
  location_narrative = "<confidential>",
  longitude = -83.8,
  max_slope_aspect = NA_real_,
  max_slope_gradient = NA_real_,
  method_narrative = NA_character_,
  min_slope_aspect = NA_real_,
  min_slope_gradient = NA_real_,
  name_other = NA_character_,
  nonvascular_cover = NA_real_,
  nonvascular_ht = NA_real_,
  number_of_taxa = NA_integer_,
  ob_code = "ob.3776",
  ob_notes_mgt = FALSE,
  ob_notes_public = FALSE,
  ob_revisions = FALSE,
  obs_end_date = "Thu, 16 Jul 1998 07:00:00 GMT",
  obs_start_date = "Thu, 16 Jul 1998 07:00:00 GMT",
  observation_narrative = NA_character_,        # NULL in API
  organic_depth = NA_real_,
  original_data = NA_character_,
  parent_pl_code = NA_character_,
  percent_bare_soil = 0L,
  percent_bed_rock = 10L,
  percent_litter = 10L,
  percent_other = 0L,
  percent_rock_gravel = 0L,
  percent_water = 0L,
  percent_wood = 5L,
  permanence = TRUE,                           # TRUE — important edge case vs ACAD FALSE
  phenologic_aspect = NA_character_,
  pj_code = "pj.928",
  pl_code = "pl.3832",
  pl_notes_mgt = FALSE,
  pl_notes_public = FALSE,
  pl_revisions = FALSE,
  placement_method = NA_character_,
  plot_validation_level = 2L,
  previous_ob_code = NA_character_,
  project_name = "Great Smoky Mountains National Park",
  replaced_by_ob_code = NA_character_,
  representativeness = NA_character_,
  rf_code = NA_character_,
  rf_label = NA_character_,
  rock_type = NA_character_,
  shape = "Rectangular",
  shore_distance = NA_real_,
  shrub_cover = NA_real_,
  shrub_ht = NA_real_,
  slope_aspect = 0L,                           # Zero aspect — valid, not missing
  slope_gradient = 36L,
  sm_code = "sm.5",
  soil_depth = 0,                              # Zero depth (not NA)
  soil_drainage = NA_character_,
  soil_moisture_regime = NA_character_,
  soil_taxon_src = NA_character_,
  st_code = NA_character_,
  stand_maturity = NA_character_,
  stand_size = NA_character_,
  state_province = "Tennessee",
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
  taxon_count = 42L,
  taxon_importance_count = 89L,
  taxon_importance_count_returned = 5L,
  taxon_observation_area = 1000,
  top_taxon1_name = "Acer saccharum",
  top_taxon2_name = "Fraxinus americana",
  top_taxon3_name = "Aesculus flava",
  top_taxon4_name = "Aesculus flava",
  top_taxon5_name = "Aristolochia macrophylla",
  topo_position = "High slope",
  total_cover = NA_real_,
  tree_cover = NA_real_,
  tree_ht = NA_real_,
  water_depth = NA_real_,
  water_salinity = NA_character_,
  year = "1998",
  stringsAsFactors = FALSE
)

mock_plot_data_grsm225$top_taxon_observations <- I(list(data.frame(
  cover = c(0.55, 0.55, 62.5, 62.5, 37.5),
  pc_code = c("pc.7186", "pc.7186", "pc.7191", "pc.7191", "pc.7191"),
  plant_name = c("Acer pensylvanicum", "Acer pensylvanicum", "Acer saccharum", "Acer saccharum", "Acer saccharum"),
  sr_code = c(NA_character_, "sr.52160", NA_character_, "sr.52167", "sr.52166"),
  stratum_name = c("-all-", "Short Shrub", "-all-", "Canopy", "Sub-Canopy"),
  tm_code = c("tm.112679", "tm.112680", "tm.112682", "tm.112683", "tm.112684"),
  to_code = c("to.87944", "to.87944", "to.87945", "to.87945", "to.87945"),
  stringsAsFactors = FALSE
)))

mock_plot_data_grsm225$top_classifications <- I(list(data.frame(
  cc_code = "cc.48250",
  ci_code = "ci.998",
  cl_code = "cl.2254",
  comm_code = "CEGL007695",
  comm_name = "Aesculus flava - Acer saccharum - (Tilia americana var. heterophylla) / Hydrophyllum canadense - Solidago flexicaulis Forest",
  stringsAsFactors = FALSE
)))

mock_plot_data_grsm225$disturbances <- I(list(data.frame(
  age = NA_character_,
  comment = "Some natural dead and down trees.",
  extent = NA_character_,
  intensity = NA_character_,
  type = "unknown",
  stringsAsFactors = FALSE
)))

mock_plot_data_grsm225$soils <- I(list(NULL))   # NULL in API — no soil data recorded

mock_plot_data_grsm225$named_places <- I(list(data.frame(
  code = c("n-us-tn", "n-us", "TTN0089"),
  description = c(NA_character_, NA_character_, "Tennessee, Southern Lat:35\u00b0 30' 00\" Eastern Long: -083\u00b0 45' 00\""),
  name = c("Tennessee", "United States", "Cades Cove"),
  np_code = c("np.416", "np.360", "np.48435"),
  system = c("region|state|province", "area|country|territory", "quadrangle"),
  stringsAsFactors = FALSE
)))

# Mock plot observation: ob.206444 (OLYM.Z.681.0003) — Olympic National Park,
# null lat/lng, null state/country, null top_taxon_observations, null disturbances,
# null soils, zero taxon_count, has rf_code, year = 2012
mock_plot_data_olym <- data.frame(
  area = 50,
  author_datum = "<confidential>",
  author_e = "<confidential>",
  author_location = "<confidential>",
  author_n = "<confidential>",
  author_obs_code = "OLYM.Z.681.0003",
  author_plot_code = "OLYM.Z.681.0003",
  author_zone = "<confidential>",
  auto_taxon_cover = NA,
  azimuth = NA_real_,
  basal_area = NA_real_,
  bryophyte_quality = NA_character_,
  cm_code = NA_character_,
  confidentiality_status = 1L,
  country = NA_character_,                     # NULL in API
  cover_dispersion = NA_character_,
  cover_method_name = NA_character_,
  date_accuracy = "1",
  date_entered = "Thu, 30 Apr 2020 16:17:56 GMT",
  dominant_stratum = NA_character_,
  dsg_poly = NA_character_,
  effort_level = NA_character_,
  elevation = 1915L,
  elevation_accuracy = NA_real_,
  elevation_range = NA_real_,
  field_cover = NA_real_,
  field_ht = NA_real_,
  floating_cover = NA_real_,
  floristic_quality = "High",
  growthform_1_cover = NA_real_,
  growthform_1_type = NA_character_,
  growthform_2_cover = NA_real_,
  growthform_2_type = NA_character_,
  growthform_3_cover = NA_real_,
  growthform_3_type = NA_character_,
  has_observation_synonym = NA,
  homogeneity = NA_character_,
  hydrologic_regime = NA_character_,
  interp_bestfit_cc_code = "cc.53823",
  interp_bestfit_ci_code = "ci.114063",
  interp_bestfit_code = NA_character_,         # NULL in API
  interp_bestfit_partyname = "Ramm-Granberg, Tynan",
  interp_bestfit_py_code = "py.199465",
  interp_bestfit_sciname = NA_character_,
  interp_current_cc_code = "cc.53823",
  interp_current_ci_code = "ci.114063",
  interp_current_code = NA_character_,
  interp_current_partyname = "Ramm-Granberg, Tynan",
  interp_current_py_code = "py.199465",
  interp_current_sciname = NA_character_,
  interp_orig_cc_code = "cc.53823",
  interp_orig_ci_code = "ci.114063",
  interp_orig_code = NA_character_,
  interp_orig_partyname = "Ramm-Granberg, Tynan",
  interp_orig_py_code = "py.199465",
  interp_orig_sciname = NA_character_,
  landform = NA_character_,
  landscape_narrative = NA_character_,
  latitude = NA_real_,                         # NULL in API — no GPS recorded
  layout_narrative = NA_character_,
  lichen_quality = NA_character_,
  location_accuracy = "10",
  location_narrative = "<confidential>",
  longitude = NA_real_,
  max_slope_aspect = NA_real_,
  max_slope_gradient = NA_real_,
  method_narrative = ">5m (or not overtopped by other trees) = Overstory Tree, <5 m (and overtopped by other trees) = Understory/Regen",
  min_slope_aspect = NA_real_,
  min_slope_gradient = NA_real_,
  name_other = NA_character_,
  nonvascular_cover = NA_real_,
  nonvascular_ht = NA_real_,
  number_of_taxa = NA_integer_,
  ob_code = "ob.206444",
  ob_notes_mgt = NA,
  ob_notes_public = NA,
  ob_revisions = NA,
  obs_end_date = "Mon, 17 Sep 2012 00:00:00 GMT",
  obs_start_date = "Mon, 17 Sep 2012 00:00:00 GMT",
  observation_narrative = "These data has been modified (QA/QC, species ID changes, association call changes, location adjustments, etc) since initial collection. Original classification and map training plot data collected by the National Park Service are available through the Int",
  organic_depth = NA_real_,
  original_data = "Vegetation classification and map model training data. NPS Data.",
  parent_pl_code = NA_character_,
  percent_bare_soil = NA_integer_,
  percent_bed_rock = NA_integer_,
  percent_litter = NA_integer_,
  percent_other = NA_integer_,
  percent_rock_gravel = NA_integer_,
  percent_water = NA_integer_,
  percent_wood = NA_integer_,
  permanence = NA,
  phenologic_aspect = NA_character_,
  pj_code = "pj.11129",
  pl_code = "pl.208702",
  pl_notes_mgt = NA,
  pl_notes_public = NA,
  pl_revisions = NA,
  placement_method = NA_character_,
  plot_validation_level = NA_integer_,
  previous_ob_code = NA_character_,
  project_name = "NVC Plot Template Import NCCN_plus_External_Sources_Plot_Data",
  replaced_by_ob_code = NA_character_,
  representativeness = NA_character_,
  rf_code = "rf.87576",
  rf_label = "This USNVC Submission",
  rock_type = NA_character_,
  shape = "Variably sized polygon",
  shore_distance = NA_real_,
  shrub_cover = NA_real_,
  shrub_ht = NA_real_,
  slope_aspect = 151L,
  slope_gradient = 19L,
  sm_code = NA_character_,
  soil_depth = NA_real_,
  soil_drainage = NA_character_,
  soil_moisture_regime = NA_character_,
  soil_taxon_src = NA_character_,
  st_code = NA_character_,
  stand_maturity = NA_character_,
  stand_size = NA_character_,
  state_province = NA_character_,              # NULL in API
  stem_observation_area = 50L,
  stem_sample_method = NA_character_,
  stem_size_limit = NA_integer_,
  stratum_assignment = NA_character_,
  stratum_method_description = NA_character_,
  stratum_method_name = NA_character_,
  submerged_cover = NA_real_,
  submerged_ht = NA_real_,
  successional_status = NA_character_,
  surficial_deposits = NA_character_,
  taxon_count = 0L,                            # Zero taxa recorded
  taxon_importance_count = 0L,
  taxon_importance_count_returned = 0L,
  taxon_observation_area = 50,
  top_taxon1_name = NA_character_,
  top_taxon2_name = NA_character_,
  top_taxon3_name = NA_character_,
  top_taxon4_name = NA_character_,
  top_taxon5_name = NA_character_,
  topo_position = NA_character_,
  total_cover = NA_real_,
  tree_cover = NA_real_,
  tree_ht = NA_real_,
  water_depth = NA_real_,
  water_salinity = NA_character_,
  year = "2012",
  stringsAsFactors = FALSE
)

mock_plot_data_olym$top_taxon_observations <- I(list(NULL))   # NULL in API — no taxa
mock_plot_data_olym$top_classifications <- I(list(data.frame(
  cc_code = "cc.53823",
  ci_code = "ci.114063",
  cl_code = "cl.167378",
  comm_code = "CEGL008260",
  comm_name = "Luina hypoleuca - (Lomatium martindalei - Castilleja parviflora) Alpine Sparse Vegetation [Proposed]",
  stringsAsFactors = FALSE
)))
mock_plot_data_olym$disturbances <- I(list(NULL))             # NULL in API
mock_plot_data_olym$soils <- I(list(NULL))                    # NULL in API
mock_plot_data_olym$named_places <- I(list(NULL))             # NULL in API

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

# Multi-row mock with three diverse observations (for table/map tests):
#   ob.2948  ACAD.143  — Acadia NP, Maine (full detail, has soils/taxa)
#   ob.3776  GRSM.225  — Great Smoky Mountains NP, Tennessee (confidential, null soils)
#   ob.206444 OLYM.Z.681.0003 — Olympic NP region, null lat/lng, null taxa/disturbances
# Build scalar columns only first, then assign nested list columns
mock_plot_observations_multi <- rbind(
  mock_plot_data[, setdiff(names(mock_plot_data), c("top_taxon_observations", "top_classifications", "disturbances", "soils", "named_places"))],
  mock_plot_data_grsm225[, setdiff(names(mock_plot_data_grsm225), c("top_taxon_observations", "top_classifications", "disturbances", "soils", "named_places"))],
  mock_plot_data_olym[, setdiff(names(mock_plot_data_olym), c("top_taxon_observations", "top_classifications", "disturbances", "soils", "named_places"))]
)
mock_plot_observations_multi$top_taxon_observations <- list(
  mock_plot_data$top_taxon_observations[[1]],
  mock_plot_data_grsm225$top_taxon_observations[[1]],
  mock_plot_data_olym$top_taxon_observations[[1]]
)
mock_plot_observations_multi$top_classifications <- list(
  mock_plot_data$top_classifications[[1]],
  mock_plot_data_grsm225$top_classifications[[1]],
  mock_plot_data_olym$top_classifications[[1]]
)
mock_plot_observations_multi$disturbances <- list(
  mock_plot_data$disturbances[[1]],
  mock_plot_data_grsm225$disturbances[[1]],
  mock_plot_data_olym$disturbances[[1]]
)
mock_plot_observations_multi$soils <- list(
  mock_plot_data$soils[[1]],
  mock_plot_data_grsm225$soils[[1]],
  mock_plot_data_olym$soils[[1]]
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
