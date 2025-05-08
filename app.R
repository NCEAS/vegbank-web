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
