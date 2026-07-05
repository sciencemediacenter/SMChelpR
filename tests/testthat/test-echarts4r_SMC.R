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
})

test_that("echarts_params ueberschreibt Stil-Konstanten gezielt", {
  e <- testchart() |>
    e_smc_style(
      title = "1\n2",
      echarts_params = list(grid_top = 40, grid_left = 100)
    )
  expect_equal(e$x$opts$grid[[1]]$top, 40 + 25) # Basis 40 + 1 Extra-Zeile
  expect_equal(e$x$opts$grid[[1]]$left, 100)
  expect_equal(e$x$opts$grid[[1]]$right, 40) # Default bleibt

  achse <- testchart() |>
    e_smc_y_percent(extend_to = 105, echarts_params = list(y_name_gap = 70))
  expect_equal(achse$x$opts$yAxis[[1]]$nameGap, 70)

  kategorie <- testchart() |>
    e_smc_x_category(echarts_params = list(category_fontsize_rotated = 12))
  expect_equal(kategorie$x$opts$xAxis[[1]]$axisLabel$fontSize, 12)

  platzhalter <- e_smc_placeholder(
    echarts_params = list(placeholder_color = "#123456")
  )
  expect_equal(platzhalter$x$opts$title[[1]]$textStyle$color, "#123456")
})

test_that("get_SMC_echarts_default_parameters deckt die e_smc_*-Konstanten ab", {
  defaults <- get_SMC_echarts_default_parameters()
  expect_type(defaults, "list")
  expect_setequal(
    names(defaults),
    c(
      "grid_top",
      "grid_top_per_title_line",
      "grid_left",
      "grid_right",
      "grid_bottom_legend",
      "grid_bottom_no_legend",
      "y_name_gap",
      "percent_interval",
      "category_fontsize_rotated",
      "placeholder_color",
      "placeholder_font_weight"
    )
  )

  # Defaults-Liste unveraendert durchreichen == Defaults (Getter und
  # get_param-Aufrufe duerfen nicht auseinanderlaufen)
  mit <- testchart() |> e_smc_style(title = "T", echarts_params = defaults)
  ohne <- testchart() |> e_smc_style(title = "T")
  expect_equal(mit$x$opts$grid, ohne$x$opts$grid)
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

test_that("format_SMC_kalenderwoche nutzt das ISO-Wochenjahr (%G)", {
  # 2024-12-30 ist ISO-Woche 1 von 2025 — mit %Y wuerde das Label mit dem
  # 01.01.2024 kollidieren
  expect_equal(format_SMC_kalenderwoche(as.Date("2024-12-30")), "KW 01, 2025")
  expect_equal(format_SMC_kalenderwoche(as.Date("2024-01-01")), "KW 01, 2024")
  expect_equal(format_SMC_kalenderwoche(as.Date("2026-07-05")), "KW 27, 2026")

  montage <- seq.Date(as.Date("2022-01-03"), as.Date("2026-06-29"), "week")
  expect_equal(anyDuplicated(format_SMC_kalenderwoche(montage)), 0L)
})
