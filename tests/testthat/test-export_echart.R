#############################
## export_echart_* tests   ##
#############################

test_that("export_echart_png_html_svg() writes html, svg and png files", {
  skip_on_cran()
  skip_if_not_installed("echarts4r")
  skip_if_not_installed("chromote")
  skip_if_not_installed("rsvg")
  skip_if_not_installed("htmlwidgets")
  # needs a headless Chrome/Chromium to be discoverable
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium found")

  withr::local_package("echarts4r")
  dir <- withr::local_tempdir()

  chart <- mtcars |> e_charts(mpg) |> e_scatter(wt)

  pngpfad <- file.path(dir, "chart.png")
  htmlpfad <- file.path(dir, "chart.html")
  svgpfad <- file.path(dir, "chart.svg")

  res <- export_echart_png_html_svg(
    chart,
    pngpfad = pngpfad,
    htmlpfad = htmlpfad,
    svgpfad = svgpfad,
    delay = 2
  )

  expect_null(res)
  expect_true(file.exists(htmlpfad) && file.size(htmlpfad) > 0)
  expect_true(file.exists(svgpfad) && file.size(svgpfad) > 0)
  expect_true(file.exists(pngpfad) && file.size(pngpfad) > 0)

  # the svg should be a real svg with the dominant-baseline workaround applied
  svg <- paste(readLines(svgpfad, warn = FALSE), collapse = "\n")
  expect_match(svg, "<svg")
  expect_false(grepl('dominant-baseline="central"', svg, fixed = TRUE))
})
