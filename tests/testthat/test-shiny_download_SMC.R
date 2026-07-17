##############################
## shiny_download_SMC tests ##
##############################

test_that("csv_clean_column_names entfernt Zeilenumbrueche aus Spaltennamen", {
  df <- data.frame(
    `Gespeichertes Gas \n in TWh` = 1,
    `Veraenderung zum Schnitt\n 2018-2021` = 2,
    Datum = as.Date("2022-01-01"),
    check.names = FALSE
  )
  expect_equal(
    names(csv_clean_column_names(df)),
    c(
      "Gespeichertes Gas in TWh",
      "Veraenderung zum Schnitt 2018-2021",
      "Datum"
    )
  )
})

test_that("filename_slug transliteriert Umlaute und kebab-cased", {
  expect_equal(filename_slug("Deutschland"), "deutschland")
  expect_equal(filename_slug("Österreich"), "oesterreich")
  expect_equal(
    filename_slug("Vereinigtes Königreich"),
    "vereinigtes-koenigreich"
  )
  expect_equal(filename_slug("Rehden (astora)"), "rehden-astora")
  expect_equal(filename_slug("Weiß & Grün"), "weiss-gruen")
})

test_that("caption_download_line baut Quelle + Download-Zeilen samt JS-Dependency", {
  skip_if_not_installed("shiny")

  tag <- caption_download_line("nat-dl", "Quelle: Test", "nat-image")
  html <- as.character(tag)
  expect_match(html, "Quelle: Test", fixed = TRUE)
  expect_match(
    html,
    "Die Daten zur Erstellung dieser Abbildung herunterladen:",
    fixed = TRUE
  )
  expect_match(html, "nat-dl", fixed = TRUE)
  expect_match(html, "als CSV.", fixed = TRUE)
  # deutsche Aufzaehlung "als PNG, als SVG oder als HTML." — Satzzeichen
  # kleben per .noWS ohne Leerzeichen an den Links
  expect_match(html, "Diese Abbildung herunterladen:", fixed = TRUE)
  expect_match(html, "nat-image_png", fixed = TRUE)
  expect_match(html, "als PNG</span></a><span>, </span>", fixed = TRUE)
  expect_match(html, "nat-image_svg", fixed = TRUE)
  expect_match(html, "oder", fixed = TRUE)
  expect_match(html, "als HTML</a><span>.</span>", fixed = TRUE)
  # JS-Asset fuer den client-seitigen Export haengt als Dependency dran
  deps <- htmltools::findDependencies(tag)
  expect_true(
    "smc-image-download" %in%
      vapply(deps, function(d) d$name, character(1))
  )

  # ohne id_image nur die Daten-Zeile, keine Dependency
  tag <- caption_download_line("x-dl", "Quelle")
  expect_no_match(
    as.character(tag),
    "Diese Abbildung herunterladen:",
    fixed = TRUE
  )
  expect_length(htmltools::findDependencies(tag), 0)
})

test_that("download_csv_handler schreibt die bereinigte CSV on the fly", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readr")

  srv <- function(input, output, session) {
    output$dl <- download_csv_handler(
      filename = function() paste0("fuellstand_", filename_slug("Österreich")),
      data = function() {
        data.frame(`a \n b` = 1:3, check.names = FALSE)
      }
    )
  }
  shiny::testServer(srv, {
    pfad <- output$dl
    expect_match(basename(pfad), "^fuellstand_oesterreich_.*\\.csv$")
    csv <- readr::read_csv(pfad, show_col_types = FALSE)
    expect_named(csv, "a b")
    expect_equal(nrow(csv), 3)
  })
})

test_that("image_download_observer registriert klickbare PNG/SVG-Beobachter", {
  skip_if_not_installed("shiny")

  srv <- function(input, output, session) {
    image_download_observer(
      input,
      session,
      id_image = "image_chart",
      output_id = "chart",
      filename = function() "name"
    )
  }
  shiny::testServer(srv, {
    # sendCustomMessage ist im Mock ein No-Op — geprueft wird, dass die
    # Beobachter fehlerfrei verdrahtet sind
    session$setInputs(image_chart_png = 1)
    session$setInputs(image_chart_svg = 1)
    succeed()
  })
})
