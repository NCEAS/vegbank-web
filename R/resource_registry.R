#' VegBank Resource Registry
#'
#' Centralized registry containing all metadata about VegBank resource types.
#' This registry serves as the single source of truth for resource information
#' used throughout the application.
#'
#' @description
#' The RESOURCE_REGISTRY consolidates:
#' - API type mappings (for citation resolution and data fetching)
#' - Detail type identifiers (for detail overlay views)
#' - VegBank code prefixes (for cross-resource navigation)
#' - Tab navigation targets
#' - Display names (singular/plural for UI text)
#'
#' @format A named list where each entry contains:
#' \describe{
#'   \item{api_type}{API resource name used by vegbankr (e.g., "plot-observations")}
#'   \item{detail_type}{Detail view identifier (e.g., "plot-observation")}
#'   \item{vb_prefix}{VegBank code prefix (e.g., "ob" for observation codes like "ob.2948")}
#'   \item{tab}{Navbar tab name to navigate to (e.g., "Plots")}
#'   \item{singular}{Singular display name (e.g., "plot observation")}
#'   \item{plural}{Plural display name (e.g., "plot observations")}
#' }
#'
#' @examples
#' \dontrun{
#' # Get plot observation resource info
#' plot_info <- RESOURCE_REGISTRY[["plot-observations"]]
#'
#' # Get resource by prefix
#' project_info <- get_resource_by_prefix("pj")
#'
#' # Get resource by detail type
#' party_info <- get_resource_by_detail_type("party")
#' }
#'
#' @noRd
RESOURCE_REGISTRY <- list(
  "plot-observations" = list(
    api_type = "plot-observations",
    detail_type = "plot-observation",
    vb_prefix = "ob",
    tab = "Plots",
    singular = "plot observation",
    plural = "plot observations"
  ),
  "community-classifications" = list(
    api_type = "community-classifications",
    detail_type = "community-classification",
    vb_prefix = "cl",
    tab = "Plots", # Comm class are accessed via the Plots tab since they are plot-level metadata
    singular = "community classification",
    plural = "community classifications",
    is_dataset = FALSE
  ),
  "community-concepts" = list(
    api_type = "community-concepts",
    detail_type = "community-concept",
    vb_prefix = "cc",
    tab = "Communities",
    singular = "community concept",
    plural = "community concepts"
  ),
  "plant-concepts" = list(
    api_type = "plant-concepts",
    detail_type = "plant-concept",
    vb_prefix = "pc",
    tab = "Plants",
    singular = "plant concept",
    plural = "plant concepts"
  ),
  "projects" = list(
    api_type = "projects",
    detail_type = "project",
    vb_prefix = "pj",
    tab = "Projects",
    singular = "project",
    plural = "projects"
  ),
  "parties" = list(
    api_type = "parties",
    detail_type = "party",
    vb_prefix = "py",
    tab = "Parties",
    singular = "party",
    plural = "parties"
  ),
  "references" = list(
    api_type = "references",
    detail_type = "reference",
    vb_prefix = "rf",
    tab = NULL,  # No dedicated tab, accessed via detail views
    singular = "reference",
    plural = "references"
  ),
  "cover-methods" = list(
    api_type = "cover-methods",
    detail_type = "cover-method",
    vb_prefix = "cm",
    tab = NULL,  # No dedicated tab, accessed via detail views
    singular = "cover method",
    plural = "cover methods"
  ),
  "stratum-methods" = list(
    api_type = "stratum-methods",
    detail_type = "stratum-method",
    vb_prefix = "sm",
    tab = NULL,  # No dedicated tab, accessed via detail views
    singular = "stratum method",
    plural = "stratum methods"
  ),
  "user-datasets" = list(
    api_type = "user-datasets",
    vb_prefix = "ds",
    detail_type = NULL,  # Datasets use a filter-based view, not a detail overlay
    tab = "Plots",
    singular = "dataset",
    plural = "datasets"
  )
)

#' Get Resource Info by VegBank Code Prefix
#'
#' Looks up resource metadata from RESOURCE_REGISTRY using the VB code prefix.
#' VegBank codes follow the pattern prefix.id (e.g., "ob.2948", "pj.340").
#'
#' @param prefix Character string, the VB code prefix (e.g., "ob", "pj", "cc")
#' @return Resource info list from RESOURCE_REGISTRY, or NULL if not found
#'
#' @examples
#' \dontrun{
#' # Get project resource info from code "pj.340"
#' resource <- get_resource_by_prefix("pj")
#' resource$singular  # "project"
#' resource$tab       # "Projects"
#' }
#'
#' @noRd
get_resource_by_prefix <- function(prefix) {
  prefix_lower <- tolower(prefix)
  for (resource_info in RESOURCE_REGISTRY) {
    if (!is.null(resource_info$vb_prefix) && resource_info$vb_prefix == prefix_lower) {
      return(resource_info)
    }
  }
  NULL
}

#' Get Resource Info by Detail Type
#'
#' Looks up resource metadata from RESOURCE_REGISTRY using the detail_type.
#' Detail types are used in detail overlay views and URL parameters.
#'
#' @param detail_type Character string, the detail type (e.g., "plot-observation", "party")
#' @return Resource info list from RESOURCE_REGISTRY, or NULL if not found
#'
#' @examples
#' \dontrun{
#' # Get resource info for plot observation detail view
#' resource <- get_resource_by_detail_type("plot-observation")
#' resource$singular  # "plot observation"
#' resource$vb_prefix # "ob"
#' }
#'
#' @noRd
get_resource_by_detail_type <- function(detail_type) {
  for (resource_info in RESOURCE_REGISTRY) {
    if (!is.null(resource_info$detail_type) && resource_info$detail_type == detail_type) {
      return(resource_info)
    }
  }
  NULL
}

#' Get Resource Info by API Type
#'
#' Looks up resource metadata from RESOURCE_REGISTRY using the API type name.
#' API types are returned by vegbankr::vb_resolve() for citation resolution.
#'
#' @param api_type Character string, the API resource type (e.g., "plot-observations", "projects")
#' @return Resource info list from RESOURCE_REGISTRY, or NULL if not found
#'
#' @examples
#' \dontrun{
#' # Get resource info from vb_resolve result
#' result <- vb_resolve("VB.Ob.2948.ACAD143")
#' resource <- get_resource_by_api_type(result$vb_resource_type)
#' resource$tab  # "Plots"
#' }
#'
#' @noRd
get_resource_by_api_type <- function(api_type) {
  RESOURCE_REGISTRY[[api_type]]
}

#' Extract entity type from VegBank code prefix
#'
#' VegBank codes follow the pattern: prefix.id (e.g., pj.340, py.123, pc.456)
#' This function extracts the prefix and looks up the resource info from
#' RESOURCE_REGISTRY to get the singular entity type name.
#'
#' @param vb_code The VegBank code (e.g., "pj.340")
#' @return Character string of entity type (e.g., "project"), or NULL if unknown
#' @noRd
convert_code_to_singular <- function(vb_code) {
  if (!is_valid_vb_code(vb_code)) {
    return(NULL)
  }

  # Extract prefix before the dot
  parts <- strsplit(as.character(vb_code), "\\.")[[1]]
  if (length(parts) < 2) {
    return(NULL)
  }

  prefix <- tolower(parts[1])
  resource_info <- get_resource_by_prefix(prefix)

  if (is.null(resource_info)) {
    return(NULL)
  }

  resource_info$singular
}

#' Load an SVG file from the package www folder for inline-HTML use
#'
#' Single loader for all icon SVGs. Callers may optionally supply an inline
#' `style` string:
#' - When `style` includes explicit `width`/`height` (e.g.
#'   `"width:12px;height:12px;margin-right:4px;flex-shrink:0"`), those values
#'   take precedence over any `width`/`height` SVG attributes via the CSS cascade.
#' - When `style` is `NULL` (the default), sizing is handled entirely by CSS
#'   (e.g. the `#detail-type-banner .detail-type-icon > svg` rule with
#'   `!important`, or button-icon CSS).
#'
#' Classes on the root `<svg>` (e.g. `detail-icon-classification`) are always
#' preserved so CSS rules that target them continue to work.
#'
#' @param name Icon filename stem (e.g. `"eye"` for `icon_eye.svg`)
#' @param style Optional inline CSS string injected into the root `<svg>`, or `NULL`
#' @return A plain character string of the processed SVG markup, or `""` if not found.
#' @noRd
load_svg_icon <- function(name, style = NULL) {
  path <- system.file("shiny", "www", "icons", paste0("icon_", name, ".svg"), package = "vegbankweb")
  if (!nzchar(path)) return("")
  # Use " " so attributes split across lines (e.g. community_concept.svg has
  # a newline mid-element-tag) remain whitespace-separated after joining,
  # while the resulting string stays newline-free. Newlines would be fine in
  # detail banner icons (set via innerHTML) but would break button icons that
  # are embedded directly in DataTable JS config strings.
  svg <- paste(readLines(path, warn = FALSE), collapse = " ")
  # Strip XML declaration and DOCTYPE — not valid when SVG is inline in HTML.
  svg <- sub("<\\?xml[^?]*\\?>", "", svg)
  svg <- sub("<!DOCTYPE[^>]*>", "", svg)
  svg <- trimws(svg)
  # Map hardcoded dark fill/stroke colours to currentColor so icons inherit
  # surrounding text colour (button hover or banner white).
  svg <- gsub('fill="#[0-9A-Fa-f]{6}"', 'fill="currentColor"', svg, perl = TRUE)
  svg <- gsub('stroke="#[0-9A-Fa-f]{6}"', 'stroke="currentColor"', svg, perl = TRUE)
  # Inject aria-hidden (and optional inline style) into the root <svg> element.
  style_attr <- if (is.null(style)) "" else sprintf(' style="%s"', style)
  svg <- sub("<svg ", sprintf('<svg aria-hidden="true"%s ', style_attr), svg, fixed = TRUE)
  svg
}

# Named list of processed SVG strings for every detail type, keyed by detail_type
# (e.g. "plot-observation"). Injected into window.DETAIL_ICONS by ui.R.
# Built once at package load — no per-session file I/O or browser fetches needed.
DETAIL_ICONS <- local({
  icons <- lapply(RESOURCE_REGISTRY, function(entry) {
    dt <- entry$detail_type
    if (is.null(dt)) return(NULL)
    # File naming: dashes in detail_type become underscores in filename
    file_stem <- gsub("-", "_", dt)
    svg <- load_svg_icon(file_stem)
    if (!nzchar(svg)) return(NULL)
    list(detail_type = dt, svg = svg)
  })
  icons <- Filter(Negate(is.null), icons)
  stats::setNames(
    lapply(icons, `[[`, "svg"),
    vapply(icons, `[[`, character(1), "detail_type")
  )
})