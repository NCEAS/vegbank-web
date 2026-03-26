# Mock dataset data for tests.
# Reproduces the three sample rows provided by the user, mirroring the
# shape returned by vegbankr::vb_get_user_datasets().

mock_dataset_ds201120 <- data.frame(
  ds_code        = "ds.201120",
  accession_code = "VB.ds.201120.DWLFOT",
  start          = as.POSIXct("2017-04-12 01:14:43", tz = "UTC"),
  stop           = NA_integer_,
  name           = "CEGL007141.v2",
  description    = "",
  type           = "normal",
  owner_label    = "Palmquist, Kyle",
  owner_email    = "kapalmqu@yahoo.com",
  py_code        = "py.191860",
  obs_count      = 6,
  stringsAsFactors = FALSE
)

mock_dataset_ds201398 <- data.frame(
  ds_code        = "ds.201398",
  accession_code = "VB.ds.201398.UNNAMEDDATASET",
  start          = as.POSIXct("2018-03-09 17:18:28", tz = "UTC"),
  stop           = NA_integer_,
  name           = "unnamed dataset",
  description    = NA_character_,
  type           = "dataset",
  owner_label    = "Albers, Gayle",
  owner_email    = "galbers@uga.edu",
  py_code        = "py.198599",
  obs_count      = 17514,
  stringsAsFactors = FALSE
)

mock_dataset_ds201907 <- data.frame(
  ds_code        = "ds.201907",
  accession_code = "VB.ds.201907.HAKKENBERG",
  start          = as.POSIXct("2019-11-19 21:00:00", tz = "UTC"),
  stop           = NA_integer_,
  name           = "Chris Hakkenberg",
  description    = NA_character_,
  type           = "normal",
  owner_label    = "Lee, Michael",
  owner_email    = "mikelee@email.com",
  py_code        = "py.410",
  obs_count      = 1901,
  stringsAsFactors = FALSE
)

mock_dataset_ds201910 <- data.frame(
  ds_code        = "ds.201910",
  accession_code = "10.5072/FK26D61D4V",
  start          = as.POSIXct("2026-03-25 21:00:00", tz = "UTC"),
  stop           = NA_integer_,
  name           = "vegbankr test - DC recommended fields",
  description    = "A test dataset for checking the identifier system works",
  type           = "normal",
  owner_label    = "Nenuji, Rushirah",
  owner_email    = "nenuji@email.com",
  py_code        = "py.410",
  obs_count      = 3,
  stringsAsFactors = FALSE
)
