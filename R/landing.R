#' Build Home / Landing Page Panel
#'
#' Constructs the nav_panel displayed when no tab is specified in the URL.
#' Contains a hero image, VegBank description, CTA buttons, and a getting
#' started guide. The panel is static — no server-side initialization required.
#'
#' @return A \code{bslib::nav_panel} for the Home tab.
#'
#' @noRd
build_landing_panel <- function() {
  bslib::nav_panel(
    title = "Home",
    htmltools::tags$div(
      class = "vb-landing",

      # Hero image with gradient overlay and text
      htmltools::tags$div(
        class = "vb-hero",
        htmltools::tags$img(
          src = "assets/vegbank-hero.jpg",
          alt = "Researcher recording vegetation plot data in a wildflower meadow at sunrise",
          class = "vb-hero-img"
        ),
        htmltools::tags$div(
          class = "vb-hero-overlay",
          htmltools::tags$div(
            class = "vb-hero-text",
            htmltools::tags$h1("VegBank"),
            htmltools::tags$p(
              class = "vb-hero-tagline",
              "The ESA's open repository for vegetation plot data"
            )
          )
        )
      ),

      # Main content: description, CTAs, getting started guide
      htmltools::tags$div(
        class = "vb-landing-content",

        # Welcome to VegBank
        htmltools::tags$div(
          class = "vb-landing-section",
          htmltools::tags$h2("Welcome to VegBank!"),
          htmltools::tags$p(
            "VegBank is the public, open repository for vegetation plot data,",
            " maintained by the ",
            htmltools::tags$a(
              href = "https://esa.org/vegpanel/",
              target = "_blank",
              "Ecological Society of America's Vegetation Classification Panel"
            ),
            ". It stores thousands of plot observations recording plant species",
            " occurrences across North America,",
            " supporting the development of the U.S. National Vegetation Classification,",
            " ecological research, conservation planning, and biodiversity monitoring."
          ),
          htmltools::tags$p(
            "For programmatic data access, see the ",
            htmltools::tags$a(
              href = "https://github.com/NCEAS/vegbankr",
              target = "_blank",
              "vegbankr R package"
            ),
            ". You can also browse the ",
            htmltools::tags$a(
              href = "?tab=FAQ",
              "FAQ page"
            ),
            " and other info in the About dropdown for additional background and documentation."
          )
        ),

        # CTA buttons
        htmltools::tags$div(
          class = "vb-landing-cta",
          htmltools::tags$a(
            href = "?tab=Plots",
            class = "btn btn-primary btn-lg vb-cta-btn",
            "Browse and Download Plots"
          ),
          htmltools::tags$a(
            href = "?tab=Map",
            class = "btn btn-outline-primary btn-lg vb-cta-btn",
            "Explore the Map"
          ),
          htmltools::tags$a(
            href = "?tab=Overview",
            class = "btn btn-outline-secondary btn-lg vb-cta-btn",
            "Get an Overview"
          )
        ),

        # Getting started guide (reuses the shared markdown file)
        htmltools::tags$div(
          class = "vb-landing-section",
          shiny::includeMarkdown(
            system.file("shiny", "www", "getting_started.md", package = "vegbankweb")
          )
        )
      )
    )
  )
}
