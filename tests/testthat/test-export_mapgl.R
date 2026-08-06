#############################
## export_mapgl_* tests    ##
#############################

test_that("export_mapgl_png_html() writes html and png files", {
  skip_on_cran()
  skip_if_not_installed("mapgl")
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("htmlwidgets")
  # needs a headless Chrome/Chromium to be discoverable
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium found")

  withr::local_package("mapgl")
  dir <- withr::local_tempdir()

  # carto_style() is a self-contained style object (no SMC tileserver needed),
  # so the test does not depend on network access to tileserver.smc.report.
  m <- maplibre(carto_style("positron"), center = c(10.4, 51.2), zoom = 5)

  pngpfad <- file.path(dir, "karte.png")
  htmlpfad <- file.path(dir, "karte.html")

  res <- export_mapgl_png_html(
    m,
    pngpfad = pngpfad,
    htmlpfad = htmlpfad,
    vwidth = 400,
    vheight = 300,
    delay = 1
  )

  expect_null(res)
  expect_true(file.exists(htmlpfad) && file.size(htmlpfad) > 0)
  expect_true(file.exists(pngpfad) && file.size(pngpfad) > 0)
})
