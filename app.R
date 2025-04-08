withr::with_options(new = list(shiny.autoload.r = FALSE), code = {
  if (!interactive()) {
    sink(stderr(), type = "output")
    tryCatch(
      expr = {
        library(VegBankWeb)
      },
      error = function(e) {
        pkgload::load_all()
      }
    )
  } else {
    pkgload::load_all()
  }
  VegBankWeb::run_app(
    options = list(test.mode = TRUE)
  )
})
