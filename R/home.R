#' Build Home / Landing Page Panel
#'
#' Constructs the nav_panel displayed when no tab is specified in the URL.
#' Contains a hero image, VegBank description, CTA buttons, and links to the
#' getting started guide, and affiliate logos. The panel is static — no
#' server-side initialization required.
#'
#' @return A \code{bslib::nav_panel} for the Home tab.
#'
#' @noRd
build_home_panel <- function() {
  hero_images <- list(
    list(src = "assets/hero_chiso_mountains.jpg",  alt = "Chisos Mountains landscape"),
    list(src = "assets/hero_crater_lake.jpg",      alt = "Crater Lake surrounded by forest"),
    list(src = "assets/hero_joshua_tree.jpg",      alt = "Joshua Tree desert landscape"),
    list(src = "assets/hero_sage_brush.jpg",       alt = "Sage brush steppe landscape")
  )
  hero <- hero_images[[sample(length(hero_images), 1L)]]

  bslib::nav_panel(
    title = "Home",
    htmltools::tags$div(
      class = "vb-home",

      # Hero image with gradient overlay and text
      htmltools::tags$div(
        class = "vb-hero",
        htmltools::tags$img(
          src = hero$src,
          alt = hero$alt,
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
        class = "vb-home-content",

        # Welcome to VegBank
        htmltools::tags$div(
          class = "vb-home-section",
          htmltools::tags$h2("Welcome to VegBank!"),
          htmltools::tags$p(
            "VegBank is the public repository for vegetation plot data,",
            " maintained by the ",
            htmltools::tags$a(
              href = "https://esa.org/vegpanel/",
              target = "_blank",
              rel = "noopener noreferrer",
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
              rel = "noopener noreferrer",
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
          class = "vb-home-cta",
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
            rel = "noopener noreferrer",
            class = "btn btn-outline-primary btn-lg vb-cta-btn",
            "Contribute Plots"
          ),
          htmltools::tags$a(
            href = "?tab=Getting Started",
            class = "btn btn-outline-primary btn-lg vb-cta-btn",
            "Learn More"
          )
        ),

        # Beta notice
        htmltools::tags$h5("Beta Notice"),
        htmltools::tags$p(
          htmltools::tags$em(
            " The content of this page may change. This is still a beta release; ",
            " things may be slow and buggy. Please ",
            htmltools::tags$a(href = "mailto:help@vegbank.org", "report bugs here"),
            " with details about what you were doing when the problem occurred."
          )
        ),

        # Partner / affiliation logos
        htmltools::tags$div(
          class = "vb-partner-logos",
          htmltools::tags$a(
            href = "https://esa.org",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "vb-partner-logo-link",
            htmltools::tags$img(
              src = "assets/logo_esa.jpg",
              alt = "Ecological Society of America",
              class = "vb-partner-logo"
            )
          ),
          htmltools::tags$a(
            href = "https://www.nceas.ucsb.edu",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "vb-partner-logo-link",
            htmltools::tags$img(
              src = "assets/logo_nceas_full.png",
              alt = "National Center for Ecological Analysis and Synthesis",
              class = "vb-partner-logo"
            )
          ),
          htmltools::tags$a(
            href = "https://www.usnvc.org",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "vb-partner-logo-link",
            htmltools::tags$img(
              src = "assets/logo_usnvc.png",
              alt = "U.S. National Vegetation Classification",
              class = "vb-partner-logo"
            )
          )
        )
      )
    )
  )
}
