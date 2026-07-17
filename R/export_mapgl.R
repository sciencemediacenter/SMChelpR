#' export_mapgl_png_html
#'
#' Export a `mapgl` map (a MapLibre GL / Mapbox GL widget) to two files: a
#' self-contained interactive HTML version and a static PNG. The map counterpart
#' to [export_echart_png_html_svg()].
#'
#' @details
#' Two things differ from the echarts exporter, both because MapLibre draws into
#' a WebGL canvas rather than an SVG DOM:
#'
#' * **No SVG.** There is no `<svg>` node to extract, so a static export is
#'   always a raster screenshot. If you need true vector/print output, use QGIS.
#' * **The PNG capture is delegated to [mapgl::save_map()]**, which serves the
#'   widget over a temporary local HTTP server and screenshots it in a headless
#'   Chrome session (\pkg{chromote}). Serving over HTTP rather than from a
#'   `file://` URL is what lets range-request tile sources — PMTiles data tiles,
#'   remote basemaps — actually load during the capture. `save_map()` waits for
#'   the map to reach an idle/rendered state before shooting, so the timing is
#'   deterministic instead of a fixed delay.
#'
#' **Resolution = viewport.** The PNG is `vwidth` by `vheight` pixels. There is
#' deliberately no device-scale/`scale` argument: MapLibre sizes its WebGL canvas
#' at map-creation time, so raising a device-scale factor *after* the page has
#' loaded (as `save_map()` does) does not upscale the map on headless Linux — it
#' is a no-op there (verified with mapgl 0.5.0). For a larger/sharper PNG, render
#' at a larger viewport and adjust `zoom`/`center`/`maxBounds` to keep the
#' framing, rather than reaching for a pixel-ratio knob.
#'
#' The self-contained HTML embeds the widget itself but still fetches tiles
#' (basemap style, PMTiles) from their URLs at view time — the interactive map
#' is the report's main artefact, the PNG the print fallback.
#'
#' Requires the suggested packages \pkg{mapgl} (>= 0.5.0, for `save_map()`) and
#' \pkg{chromote} (needs a headless Chrome/Chromium installation); they are
#' checked at runtime. On CI/headless machines without a GPU, Chrome needs
#' software WebGL (e.g. the `--enable-unsafe-swiftshader` flag) or the map
#' renders blank.
#'
#' @param map a `mapgl` widget (from [mapgl::maplibre()] or [mapgl::mapboxgl()]).
#' @param pngpfad character, output path for the PNG file.
#' @param htmlpfad character, output path for the self-contained HTML file.
#' @param vwidth integer, viewport width in px — also the PNG width. Default: 800.
#' @param vheight integer, viewport height in px — also the PNG height.
#'   Default: 600.
#' @param delay numeric or `NULL`, optional extra seconds to wait after the map
#'   reports idle before the screenshot — a fallback for stubborn remote
#'   basemaps that never fully settle on headless Linux. Default: `NULL` (rely on
#'   the idle wait only).
#' @param include_legend logical, keep `mapgl` legends in the PNG. Default: `TRUE`.
#' @param hide_controls logical, hide navigation/fullscreen/etc. controls in the
#'   PNG (interactive-only chrome). Default: `TRUE`.
#' @param background character, background colour behind the map in the PNG.
#'   Default: `"white"`.
#' @return Invisibly `NULL`. Called for its side effect of writing the two files.
#' @examples
#' \dontrun{
#' library(mapgl)
#' m <- maplibre(carto_style("positron"), center = c(10.4, 51.2), zoom = 5)
#' export_mapgl_png_html(
#'   m,
#'   pngpfad = file.path(tempdir(), "karte.png"),
#'   htmlpfad = file.path(tempdir(), "karte.html")
#' )
#' }
#' @seealso [export_echart_png_html_svg()] for the echarts equivalent.
#' @export export_mapgl_png_html
export_mapgl_png_html <- function(
  map,
  pngpfad,
  htmlpfad,
  vwidth = 800,
  vheight = 600,
  delay = NULL,
  include_legend = TRUE,
  hide_controls = TRUE,
  background = "white"
) {
  if (!requireNamespace("mapgl", quietly = TRUE)) {
    stop(
      "Package 'mapgl' (>= 0.5.0) is required for export_mapgl_png_html() ",
      "(provides save_map()). Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop(
      "Package 'chromote' is required for export_mapgl_png_html() ",
      "(needs a headless Chrome/Chromium). Please install it.",
      call. = FALSE
    )
  }

  # HTML: self-contained interactive widget (the report's main artefact).
  # Tiles (basemap style, PMTiles) are still fetched from their URLs at view
  # time; only the widget itself is embedded. Save this BEFORE save_map(), which
  # mutates its own copy of the widget (preserveDrawingBuffer, width/height).
  htmlwidgets::saveWidget(map, htmlpfad, selfcontained = TRUE)

  # PNG: delegate to mapgl::save_map(). It serves the widget over a local HTTP
  # server (so range-request tile sources load), waits for the map to report
  # idle, and screenshots it via chromote. Resolution is the viewport size.
  mapgl::save_map(
    map,
    filename = pngpfad,
    width = vwidth,
    height = vheight,
    include_legend = include_legend,
    hide_controls = hide_controls,
    background = background,
    delay = delay
  )

  invisible(NULL)
}
