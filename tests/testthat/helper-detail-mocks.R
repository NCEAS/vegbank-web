# Mock objects are organized by entity type in the mocks/ subdirectory.
# testthat does not auto-load subdirectories, so we source them explicitly here.
mocks_dir <- testthat::test_path("mocks")
for (.f in list.files(mocks_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(.f)
}
rm(.f, mocks_dir)
