# Mock project data for tests
# Real API responses from vegbankr::vb_get_projects(vb_code)

# pj.339 — vegetation alliances, 12962 obs, no dates, last added 2004-11-19
mock_project_pj339 <- data.frame(
  last_plot_added_date = "Fri, 19 Nov 2004 08:00:00 GMT",
  obs_count = 12962L,
  pj_code = "pj.339",
  project_description = paste0(
    "Improved policy and management to reduce the rate of human-induced loss of ",
    "biodiversity depends on basic knowledge of distribution, status, and trends ",
    "in species and their habitats. Vegetation monitoring provides a practical ",
    "means of tracking many components of biodiversity over space and time."
  ),
  project_name = "Composition and function of vegetation alliances in the Interior Northwest, USA",
  start_date = NA_character_,
  stop_date = NA_character_,
  stringsAsFactors = FALSE
)

# pj.10559 — Ecological Site Descriptions, 24 obs, start 2013-05-01
mock_project_pj10559 <- data.frame(
  last_plot_added_date = "Tue, 17 Mar 2015 16:00:55 GMT",
  obs_count = 24L,
  pj_code = "pj.10559",
  project_description = paste0(
    "USDA Natural Resource Conservation Service of Grand Rapids is describing ",
    "plant communities associated with soil groupings (Ecological Sites)"
  ),
  project_name = "Ecological Site Descriptions",
  start_date = "Wed, 01 May 2013 07:00:00 GMT",
  stop_date = NA_character_,
  stringsAsFactors = FALSE
)

# pj.11008 — Carolina Vegetation Survey, 48 obs, no description, no dates
mock_project_pj11008 <- data.frame(
  last_plot_added_date = "Fri, 11 Dec 2015 18:28:33 GMT",
  obs_count = 48L,
  pj_code = "pj.11008",
  project_description = NA_character_,
  project_name = "Carolina Vegetation Survey (121): Pulse 2010B = Mountain lacunae",
  start_date = NA_character_,
  stop_date = NA_character_,
  stringsAsFactors = FALSE
)

# Backward-compatible alias used by existing tests
mock_project_data <- mock_project_pj10559
