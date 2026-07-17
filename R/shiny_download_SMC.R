# shiny_download_SMC.R — download UI + handlers below charts in SMC Shiny
# apps, mirroring the image_helper_light() output of the SMC data reports:
# a centered source line followed by "Die Daten zur Erstellung dieser
# Abbildung herunterladen: als CSV." and "Diese Abbildung herunterladen:
# als PNG, als SVG oder als HTML."
#
# CSV and HTML are generated server-side on the fly; PNG and SVG are
# exported client-side from the live ECharts instance (WYSIWYG including
# the current zoom and legend state) by the bundled JS asset
# (inst/assets/smc-image-download.js) — the charts must therefore run the
# SVG renderer (`e_charts(renderer = "svg")`, the SMC default).
#
# shiny is only suggested: every function checks for it at runtime, like
# export_echart_png_html_svg() does for chromote/rsvg.

check_shiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required for the SMC download helpers. ",
      "Please install it.",
      call. = FALSE
    )
  }
}

#' csv_clean_column_names
#'
#' Clean column names for a CSV export: line breaks inside column names
#' (a legacy of some SMC data sets, e.g. "Gespeichertes Gas \n in TWh")
#' break CSV headers and are replaced by a single space.
#'
#' @param df a data.frame or tibble.
#' @return The data.frame with cleaned column names.
#' @examples
#' df <- data.frame(`a \n b` = 1, check.names = FALSE)
#' names(csv_clean_column_names(df))
#' @export csv_clean_column_names
csv_clean_column_names <- function(df) {
  names(df) <- gsub("\\s*\n\\s*", " ", names(df))
  df
}

#' filename_slug
#'
#' Build a filename component: lower case, German umlauts transliterated,
#' everything non-alphanumeric collapsed to "-" ("Österreich" ->
#' "oesterreich").
#'
#' @param x character vector.
#' @return The slugified character vector.
#' @examples
#' filename_slug("Vereinigtes Königreich")
#' @export filename_slug
filename_slug <- function(x) {
  x <- tolower(x)
  x <- gsub("ä", "ae", x)
  x <- gsub("ö", "oe", x)
  x <- gsub("ü", "ue", x)
  x <- gsub("ß", "ss", x)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

# htmlDependency delivering the client-side PNG/SVG export handler; attached
# automatically by caption_download_line() when image downloads are used.
image_download_dependency <- function() {
  htmltools::htmlDependency(
    name = "smc-image-download",
    version = as.character(utils::packageVersion("SMChelpR")),
    package = "SMChelpR",
    src = "assets",
    script = "smc-image-download.js"
  )
}

#' caption_download_line
#'
#' Source + download lines below a chart in a Shiny app — the same layout
#' as the [image_helper_light()] output below the figures of the SMC data
#' reports: the caption, then "Die Daten zur Erstellung dieser Abbildung
#' herunterladen: als CSV.", and optionally "Diese Abbildung herunterladen:
#' als PNG, als SVG oder als HTML."
#'
#' PNG and SVG are rendered as [shiny::actionLink()]s for the client-side
#' export (wire them up with [image_download_observer()]); HTML is a
#' [shiny::downloadLink()] (serve it with [download_html_handler()]). The
#' JS asset for the client-side export is attached automatically.
#'
#' @param id namespaced id of the CSV [shiny::downloadHandler()]
#'   (`ns("dl_...")`).
#' @param caption character, the source line ("Quelle: ...").
#' @param id_image namespaced base id for the image downloads
#'   (`ns("image_...")`): the actionLinks `<id_image>_png` /
#'   `<id_image>_svg` and the downloadLink `<id_image>_html` are derived
#'   from it. `NULL` renders the data line only. Default: `NULL`.
#' @return An [htmltools::tagList()]-compatible tag.
#' @export caption_download_line
caption_download_line <- function(id, caption, id_image = NULL) {
  check_shiny()
  zeile <- htmltools::div(
    class = "text-center text-muted small mb-0",
    caption,
    htmltools::tags$br(),
    "Die Daten zur Erstellung dieser Abbildung herunterladen: ",
    shiny::downloadLink(id, "als CSV."),
    if (!is.null(id_image)) {
      # German enumeration "als PNG, als SVG oder als HTML." — the
      # punctuation is glued to the links via .noWS (htmltools would
      # otherwise render a blank before the comma/period)
      htmltools::tagList(
        htmltools::tags$br(),
        "Diese Abbildung herunterladen: ",
        shiny::actionLink(paste0(id_image, "_png"), "als PNG"),
        htmltools::tags$span(", ", .noWS = "before"),
        shiny::actionLink(paste0(id_image, "_svg"), "als SVG"),
        " oder ",
        shiny::downloadLink(paste0(id_image, "_html"), "als HTML"),
        htmltools::tags$span(".", .noWS = "before")
      )
    }
  )
  if (is.null(id_image)) {
    return(zeile)
  }
  htmltools::attachDependencies(
    zeile,
    image_download_dependency(),
    append = TRUE
  )
}

#' download_csv_handler
#'
#' [shiny::downloadHandler()] for a CSV export generated on the fly: use
#' the same data function that feeds the chart, so download and figure can
#' never drift apart and the current filter state is exported. Column
#' names are cleaned via [csv_clean_column_names()]; while no data is
#' available (backend unreachable, fetch still running), [shiny::req()]
#' aborts the download.
#'
#' @param filename function returning the file name without extension
#'   (e.g. built with [filename_slug()] from the current selection); the
#'   download date and ".csv" are appended.
#' @param data function returning the data.frame to export.
#' @return A [shiny::downloadHandler()].
#' @export download_csv_handler
download_csv_handler <- function(filename, data) {
  check_shiny()
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop(
      "Package 'readr' is required for download_csv_handler(). ",
      "Please install it.",
      call. = FALSE
    )
  }
  shiny::downloadHandler(
    filename = function() {
      paste0(filename(), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- data()
      shiny::req(is.data.frame(df), nrow(df) > 0)
      readr::write_csv(csv_clean_column_names(df), file)
    }
  )
}

#' download_html_handler
#'
#' [shiny::downloadHandler()] for an HTML export: rebuilds the chart from
#' the current filters and saves it as a self-contained widget (pandoc) —
#' the same interactive standalone artifact as in the SMC data reports,
#' generated on the fly. Unlike the client-side PNG/SVG export it does not
#' know the client-side zoom/legend state; the HTML is interactive anyway.
#'
#' @param filename function returning the file name without extension; the
#'   download date and ".html" are appended.
#' @param chart function returning the `echarts4r` widget to save.
#' @return A [shiny::downloadHandler()].
#' @export download_html_handler
download_html_handler <- function(filename, chart) {
  check_shiny()
  shiny::downloadHandler(
    filename = function() {
      paste0(filename(), "_", Sys.Date(), ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(chart(), file, selfcontained = TRUE)
    }
  )
}

#' image_download_observer
#'
#' Wires up the client-side PNG/SVG downloads of a chart: clicking the
#' actionLinks from [caption_download_line()] sends the output id, format
#' and file name as a custom message to the browser, where the bundled JS
#' asset exports the live ECharts instance via `getDataURL()` — WYSIWYG
#' including the current zoom and legend state, without any server-side
#' rendering. The chart must run the SVG renderer (`e_charts(renderer =
#' "svg")`); the PNG is rasterized client-side from the SVG at 3x
#' resolution.
#'
#' @param input,session the module server's `input`/`session`.
#' @param id_image base id as passed to [caption_download_line()]
#'   (without namespace).
#' @param output_id id of the `echarts4rOutput` (without namespace).
#' @param filename function returning the file name without extension; the
#'   download date and format are appended.
#' @return Called for its side effect of registering two
#'   [shiny::observeEvent()]s.
#' @export image_download_observer
image_download_observer <- function(
  input,
  session,
  id_image,
  output_id,
  filename
) {
  check_shiny()
  observe_format <- function(format) {
    shiny::observeEvent(input[[paste0(id_image, "_", format)]], {
      session$sendCustomMessage(
        "smc_image_download",
        list(
          id = session$ns(output_id),
          type = format,
          filename = paste0(filename(), "_", Sys.Date(), ".", format)
        )
      )
    })
  }
  observe_format("png")
  observe_format("svg")
}
