# Mock community classification data for tests
# All mocks are derived from real vegbankr::vb_get_community_classifications() API responses.

# ── cl.1943 · BADL.119 · ob.3407 ─────────────────────────────────────────────
# inspection=TRUE, table=FALSE, multivariate=FALSE, expert=NA, no publication.
# 1 interpretation (CEGL001471, type=FALSE, nomenclatural=FALSE, has notes).
# 2 contributors (both Classifiers).
mock_comm_class_cl1943 <- data.frame(
  author_obs_code = "BADL.119",
  cl_code = "cl.1943",
  class_notes = NA_character_,
  class_publication_rf_code = NA_character_,
  class_publication_rf_label = NA_character_,
  class_start_date = "Wed, 01 Jul 1998 07:00:00 GMT",
  class_stop_date = NA_character_,
  expert_system = NA,
  inspection = TRUE,
  multivariate_analysis = FALSE,
  ob_code = "ob.3407",
  table_analysis = FALSE,
  stringsAsFactors = FALSE
)
mock_comm_class_cl1943$interpretations <- I(list(data.frame(
  cc_code = "cc.42757",
  ci_code = "ci.687",
  class_confidence = NA_character_,
  class_fit = NA_character_,
  comm_authority_name = NA_character_,
  comm_authority_rf_code = NA_character_,
  comm_code = "CEGL001471",
  comm_label = "Calamovilfa longifolia - Carex inops ssp. heliophila Grassland [NatureServe Biotics 2019]",
  comm_name = "Calamovilfa longifolia - Carex inops ssp. heliophila Grassland",
  nomenclatural_type = FALSE,
  notes = "Calamovilfa longifolia Grassland",
  type = FALSE,
  stringsAsFactors = FALSE
)))
mock_comm_class_cl1943$contributors <- I(list(data.frame(
  party_label = c("Von Loh, Jim", "Faber-Langendoen, Don"),
  py_code = c("py.1246", "py.352"),
  role = c("Classifier", "Classifier"),
  stringsAsFactors = FALSE
)))


# ── cl.2358 · ISRO.020 · ob.3884 ─────────────────────────────────────────────
# inspection=FALSE, table=FALSE, multivariate=TRUE, expert=NA, no publication.
# 1 interpretation (CEGL002509, Picea glauca forest), 2 contributors.
mock_comm_class_cl2358 <- data.frame(
  author_obs_code = "ISRO.020",
  cl_code = "cl.2358",
  class_notes = NA_character_,
  class_publication_rf_code = NA_character_,
  class_publication_rf_label = NA_character_,
  class_start_date = "Thu, 01 Apr 1999 08:00:00 GMT",
  class_stop_date = NA_character_,
  expert_system = NA,
  inspection = FALSE,
  multivariate_analysis = TRUE,
  ob_code = "ob.3884",
  table_analysis = FALSE,
  stringsAsFactors = FALSE
)
mock_comm_class_cl2358$interpretations <- I(list(data.frame(
  cc_code = "cc.43800",
  ci_code = "ci.1102",
  class_confidence = NA_character_,
  class_fit = NA_character_,
  comm_authority_name = NA_character_,
  comm_authority_rf_code = NA_character_,
  comm_code = "CEGL002509",
  comm_label = "Picea glauca - Abies balsamea / Pleurozium schreberi Forest [NatureServe Biotics 2019]",
  comm_name = "Picea glauca - Abies balsamea / Pleurozium schreberi Forest",
  nomenclatural_type = FALSE,
  notes = "Balsam fir - paper birch forest",
  type = FALSE,
  stringsAsFactors = FALSE
)))
mock_comm_class_cl2358$contributors <- I(list(data.frame(
  party_label = c("Reschke, Carole", "Faber-Langendoen, Don"),
  py_code = c("py.3026", "py.352"),
  role = c("Classifier", "Classifier"),
  stringsAsFactors = FALSE
)))


# ── cl.167392 · OLYM.Z.725.0002 · ob.206458 ──────────────────────────────────
# All method flags NA (data import), no publication, no start/stop date.
# 1 interpretation (CEGL008281, proposed, all details NA), 1 contributor.
mock_comm_class_cl167392 <- data.frame(
  author_obs_code = "OLYM.Z.725.0002",
  cl_code = "cl.167392",
  class_notes = NA_character_,
  class_publication_rf_code = NA_character_,
  class_publication_rf_label = NA_character_,
  class_start_date = NA_character_,
  class_stop_date = NA_character_,
  expert_system = NA,
  inspection = NA,
  multivariate_analysis = NA,
  ob_code = "ob.206458",
  table_analysis = NA,
  stringsAsFactors = FALSE
)
mock_comm_class_cl167392$interpretations <- I(list(data.frame(
  cc_code = "cc.53844",
  ci_code = "ci.114077",
  class_confidence = NA_character_,
  class_fit = NA_character_,
  comm_authority_name = NA_character_,
  comm_authority_rf_code = NA_character_,
  comm_code = "CEGL008281",
  comm_label = "Spiraea splendens / Carex spectabilis - (Polygonum bistortoides) Shrubland [Proposed] [Ramm-Granberg 2020]",
  comm_name = "Spiraea splendens / Carex spectabilis - (Polygonum bistortoides) Shrubland [Proposed]",
  nomenclatural_type = NA,
  notes = NA_character_,
  type = NA,
  stringsAsFactors = FALSE
)))
mock_comm_class_cl167392$contributors <- I(list(data.frame(
  party_label = "Ramm-Granberg, Tynan",
  py_code = "py.199465",
  role = "Data aggregator",
  stringsAsFactors = FALSE
)))


# Backward-compatible aliases
mock_comm_class_data <- mock_comm_class_cl1943
mock_comm_class_interpretations <- mock_comm_class_cl1943$interpretations[[1]]
mock_comm_class_contributors <- mock_comm_class_cl1943$contributors[[1]]
