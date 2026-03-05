# Mock reference (literature citation) data for tests
# Real API responses from vegbankr::vb_get_references(vb_code)

# rf.36675 — NYNHP Field Form Instructions: Report type, has URL, 31 pages
mock_reference_rf36675 <- data.frame(
  degree = NA_character_,
  doi = NA_character_,
  full_citation = "Edinger et al. 2000. Community Field Form Instructions: Community Forms 1, 2, & 3. New York Natural Heritage Program. Latham, NY.",
  isbn = NA_character_,
  journal = NA_character_,
  publication_date = "Thu, 22 Jun 1905 08:00:00 GMT",
  publication_place = "Latham, NY",
  publisher = "New York Natural Heritage Program",
  reference_type = "Report",
  rf_code = "rf.36675",
  rf_label = "NYNHP Field Form Instructions",
  short_name = "NYNHP Field Form Instructions",
  title = "Community Field Form Instructions: Community Forms 1, 2, & 3.",
  total_pages = 31L,
  url = "http://whiteoak.natureserve.org/hdms/Biotics-FieldForms.shtml",
  stringsAsFactors = FALSE
)

# rf.37 — USDA Plants 2011: no reference_type, no URL/DOI/ISBN
mock_reference_rf37 <- data.frame(
  degree = NA_character_,
  doi = NA_character_,
  full_citation = "USDA, NRCS. 2011. The PLANTS Database (http://plants.usda.gov, 25 August 2011). National Plant Data Team, Greensboro, NC 27401-4901 USA.",
  isbn = NA_character_,
  journal = NA_character_,
  publication_date = "Thu, 25 Aug 2011 07:00:00 GMT",
  publication_place = "Greensboro, NC, USA",
  publisher = "National Plant Data Team",
  reference_type = NA_character_,
  rf_code = "rf.37",
  rf_label = "USDA Plants 2011",
  short_name = "USDA Plants 2011",
  title = "The Plants Database",
  total_pages = NA_integer_,
  url = NA_character_,
  stringsAsFactors = FALSE
)

# rf.35243 — UNKNOWN: essentially all fields NA
mock_reference_rf35243 <- data.frame(
  degree = NA_character_,
  doi = NA_character_,
  full_citation = "UNKNOWN",
  isbn = NA_character_,
  journal = NA_character_,
  publication_date = NA_character_,
  publication_place = NA_character_,
  publisher = NA_character_,
  reference_type = NA_character_,
  rf_code = "rf.35243",
  rf_label = "UNKNOWN",
  short_name = "UNKNOWN",
  title = NA_character_,
  total_pages = NA_integer_,
  url = NA_character_,
  stringsAsFactors = FALSE
)

# Backward-compatible alias used by existing tests
mock_reference_data <- mock_reference_rf36675
