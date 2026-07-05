##########################
## echarts4r_SMC tests  ##
##########################

testchart <- function() {
  df <- data.frame(x = as.Date("2024-01-01") + 0:9, y = 1:10)
  df |>
    echarts4r::e_charts(x) |>
    echarts4r::e_line(y)
}

test_that("e_smc_style setzt Palette, Grid, Toolbox und zentrierten Titel", {
  e <- testchart() |> e_smc_style(title = "Testtitel")

  expect_equal(e$x$opts$color, as.list(colors_SMC()))
  expect_equal(e$x$opts$title[[1]]$text, "Testtitel")
  expect_equal(e$x$opts$title[[1]]$left, "center")
  expect_equal(e$x$opts$grid[[1]]$top, 50)
  expect_equal(e$x$opts$grid[[1]]$left, 80)
  expect_equal(e$x$opts$grid[[1]]$right, 40)
  expect_equal(e$x$opts$grid[[1]]$bottom, 80) # Legende "bottom"
  expect_named(
    e$x$opts$toolbox$feature,
    c("dataZoom", "restore"),
    ignore.order = TRUE
  )
  expect_false(e$x$opts$toolbox$feature$dataZoom$yAxisIndex)
})

test_that("e_smc_style: Legenden-Varianten steuern grid_bottom und Legende", {
  ohne <- testchart() |> e_smc_style(legend = "none")
  expect_equal(ohne$x$opts$grid[[1]]$bottom, 60)
  expect_false(ohne$x$opts$legend$show)

  scroll <- testchart() |> e_smc_style(legend = "scroll")
  expect_equal(scroll$x$opts$grid[[1]]$bottom, 80)
  expect_equal(scroll$x$opts$legend$type, "scroll")
  expect_equal(scroll$x$opts$legend$bottom, 0)
})

test_that("e_smc_style: mehrzeiliger Titel vergroessert grid_top um 25 px/Zeile", {
  e <- testchart() |> e_smc_style(title = "Zeile 1\nZeile 2")
  expect_equal(e$x$opts$grid[[1]]$top, 75)

  e3 <- testchart() |> e_smc_style(title = "1\n2\n3")
  expect_equal(e3$x$opts$grid[[1]]$top, 100)

  explizit <- testchart() |> e_smc_style(title = "1\n2", grid_top = 42)
  expect_equal(explizit$x$opts$grid[[1]]$top, 42)
})

test_that("e_smc_hline haengt eine stille markLine an die letzte Serie", {
  e <- testchart() |> e_smc_hline(100, opacity = 0.6)
  mark <- e$x$opts$series[[1]]$markLine

  expect_equal(mark$data[[1]]$yAxis, 100)
  expect_equal(mark$silent, TRUE)
  expect_false(mark$label$show)
  expect_equal(mark$lineStyle$opacity, 0.6)
  expect_equal(mark$lineStyle$type, "solid")
})

test_that("e_smc_y_percent formatiert Prozent und dehnt die Achse optional", {
  e <- testchart() |> e_smc_y_percent(name = "Anteil")
  achse <- e$x$opts$yAxis[[1]]
  expect_equal(achse$axisLabel$formatter, "{value} %")
  expect_equal(achse$name, "Anteil")
  expect_equal(achse$nameLocation, "middle")
  expect_null(achse$max)

  gedehnt <- testchart() |> e_smc_y_percent(extend_to = 105)
  achse <- gedehnt$x$opts$yAxis[[1]]
  expect_s3_class(achse$max, "JS_EVAL")
  expect_match(as.character(achse$max), "105")
  expect_equal(achse$interval, 20)
})

test_that("e_smc_x_time setzt Achsenlinie und deutschen Monats-Formatter", {
  e <- testchart() |> e_smc_x_time()
  achse <- e$x$opts$xAxis[[1]]
  expect_equal(achse$axisLine$show, TRUE)
  expect_s3_class(achse$axisLabel$formatter, "JS_EVAL")
  expect_match(as.character(achse$axisLabel$formatter), "getFullYear")

  ohne_jahr <- testchart() |> e_smc_x_time(show_year = FALSE)
  expect_no_match(
    as.character(ohne_jahr$x$opts$xAxis[[1]]$axisLabel$formatter),
    "getFullYear"
  )
})

test_that("e_smc_x_category rotiert Labels und zeigt die Achsenlinie", {
  e <- testchart() |> e_smc_x_category()
  achse <- e$x$opts$xAxis[[1]]
  expect_equal(achse$axisLine$show, TRUE)
  expect_false(achse$boundaryGap)
  expect_equal(achse$axisLabel$rotate, 45)
  expect_equal(achse$axisLabel$fontSize, 10)

  gerade <- testchart() |> e_smc_x_category(rotate = 0)
  expect_null(gerade$x$opts$xAxis[[1]]$axisLabel$fontSize)
})

test_that("e_smc_tooltip baut axis-Trigger mit deutschem Formatter", {
  e <- testchart() |> e_smc_tooltip(unit = " %", digits = 2)
  tooltip <- e$x$opts$tooltip
  expect_equal(tooltip$trigger, "axis")
  formatter <- as.character(tooltip$formatter)
  expect_match(formatter, "de-DE", fixed = TRUE)
  expect_match(formatter, "maximumFractionDigits: 2", fixed = TRUE)
  expect_match(formatter, "getMonth") # Zeitachsen-Kopf

  kategorie <- testchart() |> e_smc_tooltip(axis_type = "category")
  expect_no_match(
    as.character(kategorie$x$opts$tooltip$formatter),
    "getMonth"
  )
})

test_that("e_smc_placeholder rendert ohne Koordinatensystem", {
  e <- e_smc_placeholder("Keine Daten")
  expect_s3_class(e, "echarts4r")
  expect_equal(e$x$opts$title[[1]]$text, "Keine Daten")
  # yAxis ohne xAxis laesst ECharts beim Rendern abstuerzen (axisBuilder)
  expect_null(e$x$opts$yAxis)
  expect_null(e$x$opts$xAxis)
})

test_that("colors_SMC_ramp liefert exakt n Farben, interpoliert ueber 9", {
  expect_equal(colors_SMC_ramp(3), colors_SMC()[1:3])
  expect_equal(colors_SMC_ramp(9), colors_SMC())

  viele <- colors_SMC_ramp(16)
  expect_length(viele, 16)
  expect_length(unique(viele), 16)
  is_valid_hex <- function(color) grepl("^#[0-9A-Fa-f]{6}$", color)
  expect_true(all(sapply(viele, is_valid_hex)))
})

test_that("format_SMC_kalenderwoche nutzt das ISO-Wochenjahr (%G)", {
  # 2024-12-30 ist ISO-Woche 1 von 2025 — mit %Y wuerde das Label mit dem
  # 01.01.2024 kollidieren
  expect_equal(format_SMC_kalenderwoche(as.Date("2024-12-30")), "KW 01, 2025")
  expect_equal(format_SMC_kalenderwoche(as.Date("2024-01-01")), "KW 01, 2024")
  expect_equal(format_SMC_kalenderwoche(as.Date("2026-07-05")), "KW 27, 2026")

  montage <- seq.Date(as.Date("2022-01-03"), as.Date("2026-06-29"), "week")
  expect_equal(anyDuplicated(format_SMC_kalenderwoche(montage)), 0L)
})
