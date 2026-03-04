# Mock party (person/organization) data for tests
# Real API responses from vegbankr::vb_get_parties(vb_code)

# py.415 — Michael Jennings, University of Idaho, 8657 observations
mock_party_py415 <- data.frame(
  contact_instructions = NA_character_,
  given_name = "Michael",
  middle_name = NA_character_,
  obs_count = 8657L,
  organization_name = "University of Idaho, Department of Geography",
  party_label = "Jennings, Michael",
  py_code = "py.415",
  salutation = NA_character_,
  surname = "Jennings",
  stringsAsFactors = FALSE
)

# py.17 — Jim Drake, The Nature Conservancy, 290 observations
mock_party_py17 <- data.frame(
  contact_instructions = "contact the contributor at specified email:",
  given_name = "Jim",
  middle_name = NA_character_,
  obs_count = 290L,
  organization_name = "The Nature Conservancy",
  party_label = "Drake, Jim",
  py_code = "py.17",
  salutation = "Mr.",
  surname = "Drake",
  stringsAsFactors = FALSE
)

# py.199146 — Mike Heaney, no organization, no contact, no obs_count
mock_party_py199146 <- data.frame(
  contact_instructions = NA_character_,
  given_name = "Mike",
  middle_name = NA_character_,
  obs_count = NA_integer_,
  organization_name = NA_character_,
  party_label = "Heaney, Mike",
  py_code = "py.199146",
  salutation = NA_character_,
  surname = "Heaney",
  stringsAsFactors = FALSE
)

# Backward-compatible alias used by existing tests
mock_party_data <- mock_party_py415
