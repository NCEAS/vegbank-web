# Must be kept at the top level of the app directory to be recognized and run by the vscode
# shiny extension. R build ignores this file, so it won't be included in the package bundle.
withr::with_options(new = list(shiny.autoload.r = FALSE), code = {
  if (!interactive()) {
    sink(stderr(), type = "output")
    tryCatch(
      expr = {
        library(vegbankweb)
      },
      error = function(e) {
        pkgload::load_all()
      }
    )
  } else {
    pkgload::load_all()
  }
  vegbankweb::run_app(
    options = list(test.mode = TRUE)
  )
})
