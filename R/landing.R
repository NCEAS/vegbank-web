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
            "For programmatic data access or to upload data, see the ",
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
          # Browse dropdown
          htmltools::tags$div(
            class = "btn-group",
            htmltools::tags$button(
              type = "button",
              class = "btn btn-primary btn-lg vb-cta-btn dropdown-toggle",
              `data-bs-toggle` = "dropdown",
              `aria-expanded` = "false",
              "Browse Plots"
            ),
            htmltools::tags$ul(
              class = "dropdown-menu",
              htmltools::tags$li(htmltools::tags$a(
                class = "dropdown-item",
                href = "?tab=Plots",
                "All Plots in a Table"
              )),
              htmltools::tags$li(htmltools::tags$a(
                class = "dropdown-item",
                href = "?tab=Map",
                "All Plots on a Map"
              )),
              htmltools::tags$li(htmltools::tags$hr(class = "dropdown-divider")),
              htmltools::tags$li(htmltools::tags$a(
                class = "dropdown-item",
                href = "?tab=Communities",
                "By Vegetation Community"
              )),
              htmltools::tags$li(htmltools::tags$a(
                class = "dropdown-item",
                href = "?tab=Plants",
                "By Plant Species"
              )),
              htmltools::tags$li(htmltools::tags$a(
                class = "dropdown-item",
                href = "?tab=Parties",
                "By Data Contributor"
              )),
              htmltools::tags$li(htmltools::tags$a(
                class = "dropdown-item",
                href = "?tab=Projects",
                "By Sampling Project"
              ))
            )
          ),
          htmltools::tags$a(
            href = "https://github.com/NCEAS/vegbankr",
            target = "_blank",
            class = "btn btn-outline-primary btn-lg vb-cta-btn",
            "Contribute Plots"
          ),
          htmltools::tags$a(
            href = "?tab=Getting Started",
            class = "btn btn-outline-primary btn-lg vb-cta-btn",
            "Learn More"
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
