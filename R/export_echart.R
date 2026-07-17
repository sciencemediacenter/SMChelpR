#' export_echart_png_html_svg
#'
#' Export an `echarts4r` widget to three files: a self-contained HTML version,
#' an SVG (extracted from the rendered DOM via a headless browser), and a
#' high-resolution PNG (rasterized from the SVG).
#'
#' @details
#' The SVG is produced by switching the chart to the SVG renderer, rendering it
#' in a headless Chrome session (\pkg{chromote}) and reading the `<svg>` node
#' from the DOM. A white background rectangle is inserted so the SVG/PNG are not
#' transparent. The PNG is then rasterized from that SVG with \pkg{rsvg}.
#'
#' Note: `dominant-baseline="central"` is rewritten to `dy="0.35em"` because
#' librsvg (used by `rsvg::rsvg_png()` and e.g. the GNOME image viewer) does not
#' support `central` and would otherwise shift text ~5px upwards, which can clip
#' text inside a scroll legend's clipPath.
#'
#' This function requires the suggested packages \pkg{chromote} (needs a
#' headless Chrome/Chromium installation) and \pkg{rsvg} (needs the system
#' library librsvg). They are checked at runtime.
#'
#' @param echart an `echarts4r` widget (htmlwidget) to export.
#' @param pngpfad character, output path for the PNG file.
#' @param htmlpfad character, output path for the self-contained HTML file.
#' @param svgpfad character, output path for the SVG file.
#' @param vwidth integer, viewport width in px used to render the chart for SVG
#'   extraction. Default: 800.
#' @param vheight integer, viewport height in px used to render the chart for
#'   SVG extraction. Default: 600.
#' @param delay numeric, seconds to wait after page load before reading the DOM,
#'   to allow the chart (and any animations) to finish rendering. Default: 5.
#' @param png_width integer, width in px of the rasterized PNG. Default: 2400.
#' @return Invisibly `NULL`. Called for its side effect of writing the three
#'   files.
#' @examples
#' \dontrun{
#' library(echarts4r)
#' chart <- mtcars |> e_charts(mpg) |> e_scatter(wt)
#' export_echart_png_html_svg(
#'   chart,
#'   pngpfad = file.path(tempdir(), "chart.png"),
#'   htmlpfad = file.path(tempdir(), "chart.html"),
#'   svgpfad = file.path(tempdir(), "chart.svg")
#' )
#' }
#' @export export_echart_png_html_svg
export_echart_png_html_svg <- function(
  echart,
  pngpfad,
  htmlpfad,
  svgpfad,
  vwidth = 800,
  vheight = 600,
  delay = 5,
  png_width = 2400
) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop(
      "Package 'chromote' is required for export_echart_png_html_svg() ",
      "(needs a headless Chrome/Chromium). Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop(
      "Package 'rsvg' is required for export_echart_png_html_svg() ",
      "(needs the system library librsvg). Please install it.",
      call. = FALSE
    )
  }

  # HTML: self-contained widget
  htmlwidgets::saveWidget(echart, htmlpfad, selfcontained = TRUE)

  # SVG: switch to SVG renderer, extract from DOM via chromote
  echart_svg <- echart
  echart_svg$x$renderer <- "svg"
  tmp_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(echart_svg, tmp_html, selfcontained = TRUE)

  b <- chromote::ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  b$Emulation$setDeviceMetricsOverride(
    width = vwidth,
    height = vheight,
    deviceScaleFactor = 1,
    mobile = FALSE
  )
  # Event VOR navigate() abonnieren: bei schnellen Seiten (kleine Widgets,
  # neuere Chrome-Versionen) feuert loadEventFired sonst, bevor der Listener
  # registriert ist, und das nachtraegliche Warten laeuft in den Timeout.
  p_load <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file://", normalizePath(tmp_html)), wait_ = FALSE)
  b$wait_for(p_load)
  Sys.sleep(delay)

  svg_text <- b$Runtime$evaluate(paste0(
    "var svg = document.querySelector('svg');",
    "var rect = document.createElementNS('http://www.w3.org/2000/svg','rect');",
    "rect.setAttribute('width','100%');",
    "rect.setAttribute('height','100%');",
    "rect.setAttribute('fill','#ffffff');",
    "svg.insertBefore(rect, svg.firstChild);",
    "svg.outerHTML"
  ))$result$value

  # librsvg (rsvg_png, GNOME-Bildbetrachter, ...) unterstuetzt
  # dominant-baseline="central" nicht und setzt stattdessen die Grundlinie
  # auf y - Text rutscht ~5 px nach oben und wird z. B. vom clipPath der
  # Scroll-Legende abgeschnitten. dy="0.35em" zentriert in allen Renderern
  # gleich.
  svg_text <- gsub(
    'dominant-baseline="central"',
    'dy="0.35em"',
    svg_text,
    fixed = TRUE
  )

  writeLines(svg_text, svgpfad)
  unlink(tmp_html)

  # PNG: rasterize from SVG
  rsvg::rsvg_png(svgpfad, file = pngpfad, width = png_width)

  invisible(NULL)
}
