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

test_that("e_smc_hline haengt eine stille markLine an alle Serien", {
  e <- testchart() |> e_smc_hline(100, opacity = 0.6)
  mark <- e$x$opts$series[[1]]$markLine

  expect_equal(mark$data[[1]]$yAxis, 100)
  expect_equal(mark$symbol, "none")
  expect_equal(mark$silent, TRUE)
  expect_false(mark$data[[1]]$label$show)
  expect_equal(mark$data[[1]]$lineStyle$opacity, 0.6)
  expect_equal(mark$data[[1]]$lineStyle$type, "solid")
})

test_that("e_smc_hline/e_smc_vline unterstuetzen type = 'dashed'", {
  h <- testchart() |> e_smc_hline(100, type = "dashed")
  expect_equal(h$x$opts$series[[1]]$markLine$data[[1]]$lineStyle$type, "dashed")

  v <- testchart() |> e_smc_vline(as.Date("2024-01-01"), type = "dashed")
  expect_equal(v$x$opts$series[[1]]$markLine$data[[1]]$lineStyle$type, "dashed")
})

test_that("e_smc_hline/e_smc_vline zeigen ein Label, wenn `label` gesetzt ist", {
  ohne <- testchart() |> e_smc_hline(100)
  expect_false(ohne$x$opts$series[[1]]$markLine$data[[1]]$label$show)

  mit <- testchart() |>
    e_smc_hline(100, color = "#000000", label = "Ziel: 100 GW")
  label <- mit$x$opts$series[[1]]$markLine$data[[1]]$label
  expect_true(label$show)
  expect_equal(label$formatter, "Ziel: 100 GW")
  expect_equal(label$position, "insideStartTop")
  expect_equal(label$color, "#000000")
  expect_equal(label$fontSize, 11)

  eigene_position <- testchart() |>
    e_smc_hline(100, label = "Ziel", label_position = "end")
  expect_equal(
    eigene_position$x$opts$series[[1]]$markLine$data[[1]]$label$position,
    "end"
  )

  v <- testchart() |> e_smc_vline(as.Date("2024-01-01"), label = "Heute")
  expect_true(v$x$opts$series[[1]]$markLine$data[[1]]$label$show)
})

test_that("e_smc_hline mit Label und use_phantom_series kombiniert beides", {
  e <- testchart() |>
    e_smc_hline(100, label = "Ziel: 100 GW", use_phantom_series = TRUE)
  point <- e$x$opts$series[[2]]$markLine$data[[1]]
  expect_true(point$label$show)
  expect_equal(point$yAxis, 100)
})

test_that("e_smc_hline bleibt bei mehreren Aufrufen pro Linie eigenstaendig stylebar", {
  # Regression: e_mark_line() mergt ab dem zweiten Aufruf auf dieselbe Serie
  # nur `data` und verwirft lineStyle/label aus diesem Aufruf, wenn die
  # Werte auf oberster Ebene statt je Punkt in `data` stehen
  e <- testchart() |>
    e_smc_hline(100, color = "blue") |>
    e_smc_hline(50, color = "red", opacity = 0.9)
  mark <- e$x$opts$series[[1]]$markLine

  expect_length(mark$data, 2)
  expect_equal(mark$data[[1]]$yAxis, 100)
  expect_equal(mark$data[[1]]$lineStyle$color, "blue")
  expect_equal(mark$data[[2]]$yAxis, 50)
  expect_equal(mark$data[[2]]$lineStyle$color, "red")
  expect_equal(mark$data[[2]]$lineStyle$opacity, 0.9)
})

test_that("e_smc_vline verhaelt sich wie e_smc_hline, nur auf der x-Achse", {
  e <- testchart() |> e_smc_vline(as.Date("2024-01-01"), color = "blue")
  mark <- e$x$opts$series[[1]]$markLine

  expect_equal(mark$data[[1]]$xAxis, as.Date("2024-01-01"))
  expect_equal(mark$symbol, "none")
  expect_equal(mark$silent, TRUE)
  expect_false(mark$data[[1]]$label$show)
  expect_equal(mark$data[[1]]$lineStyle$color, "blue")

  e2 <- e |> e_smc_vline(as.Date("2024-06-01"), color = "red")
  mark2 <- e2$x$opts$series[[1]]$markLine
  expect_length(mark2$data, 2)
  expect_equal(mark2$data[[2]]$lineStyle$color, "red")
})

test_that("use_phantom_series haengt die Linie an eine eigene, unsichtbare Serie", {
  e <- testchart() |> e_smc_hline(100, use_phantom_series = TRUE)

  expect_length(e$x$opts$series, 2)
  phantom <- e$x$opts$series[[2]]
  expect_equal(phantom$name, "SMC_hline_phantom")
  expect_equal(phantom$lineStyle$opacity, 0)
  expect_true(phantom$silent)
  expect_equal(phantom$markLine$data[[1]]$yAxis, 100)
  # die echte Serie ("y", aus testchart()) traegt keine markLine
  expect_null(e$x$opts$series[[1]]$markLine)
  # Legende auf die echten Serien beschraenkt, Phantom-Serie ausgeschlossen
  expect_equal(e$x$opts$legend$data, list("y"))

  # zweite Referenzlinie landet auf derselben Phantom-Serie, nicht auf einer weiteren
  e2 <- e |> e_smc_hline(50, use_phantom_series = TRUE)
  expect_length(e2$x$opts$series, 2)
  expect_length(e2$x$opts$series[[2]]$markLine$data, 2)
})

test_that("e_smc_vline mit use_phantom_series nutzt eine eigene Phantom-Serie", {
  e <- testchart() |>
    e_smc_vline(as.Date("2024-01-01"), use_phantom_series = TRUE)

  expect_length(e$x$opts$series, 2)
  phantom <- e$x$opts$series[[2]]
  expect_equal(phantom$name, "SMC_vline_phantom")
  expect_equal(phantom$markLine$data[[1]]$xAxis, as.Date("2024-01-01"))
  expect_equal(e$x$opts$legend$data, list("y"))
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

test_that("e_smc_style haengt die DE-Locale als htmlDependency an", {
  e <- testchart() |> e_smc_style(title = "Test")
  namen <- vapply(e$dependencies, function(d) d$name, character(1))
  expect_true("smc-echarts-locale-de" %in% namen)
  dep <- e$dependencies[[which(namen == "smc-echarts-locale-de")[1]]]
  pfad <- system.file(dep$src$file, dep$script, package = dep$package)
  expect_true(nzchar(pfad) && file.exists(pfad))
})

test_that("e_smc_tooltip baut axis-Trigger mit deutschem Formatter", {
  e <- testchart() |> e_smc_tooltip(unit = " %", digits = 2)
  tooltip <- e$x$opts$tooltip
  expect_equal(tooltip$trigger, "axis")
  formatter <- as.character(tooltip$formatter)
  # Zahlen im format_SMC_number-Stil: schmales Leerzeichen als Trenner,
  # digits nur fuer Nicht-Ganzzahlen
  expect_match(formatter, "font-size:50%", fixed = TRUE)
  expect_match(formatter, "toFixed(ganz ? 0 : 2)", fixed = TRUE)
  # Zeitachsen-Kopf ueber echarts.time.format mit der DE-Locale
  expect_match(formatter, "echarts.time.format", fixed = TRUE)
  expect_match(formatter, "{d}. {MMM} {yyyy}", fixed = TRUE)

  ohne_jahr <- testchart() |> e_smc_tooltip(show_year = FALSE)
  expect_match(
    as.character(ohne_jahr$x$opts$tooltip$formatter),
    "{d}. {MMM}'",
    fixed = TRUE
  )

  kategorie <- testchart() |> e_smc_tooltip(axis_type = "category")
  expect_no_match(
    as.character(kategorie$x$opts$tooltip$formatter),
    "time.format"
  )
})

test_that("e_smc_tooltip granularity = 'year' zeigt nur die Jahreszahl im Kopf", {
  e <- testchart() |> e_smc_tooltip(granularity = "year")
  formatter <- as.character(e$x$opts$tooltip$formatter)
  expect_match(formatter, "'{yyyy}'", fixed = TRUE)
  expect_no_match(formatter, "MMM", fixed = TRUE)

  # granularity wird bei axis_type = "category" ignoriert
  kategorie <- testchart() |>
    e_smc_tooltip(axis_type = "category", granularity = "year")
  expect_no_match(
    as.character(kategorie$x$opts$tooltip$formatter),
    "yyyy",
    fixed = TRUE
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

test_that("Tooltip-Zahlenformat entspricht format_SMC_number()", {
  # js_smc_tooltip_zeilen muss dasselbe Format liefern wie
  # format_SMC_number() (Komma, schmales HTML-Leerzeichen als
  # Tausendertrenner, Ganzzahlen ohne Dezimalen) — Paritaet via V8.
  # Keine .5-Rundungsgrenzfaelle in den Testwerten: formatC (C-Runden,
  # half-to-even) und JS toFixed (half-up) runden exakte Bindungen
  # unterschiedlich.
  skip_if_not_installed("V8")
  ctx <- V8::v8()
  ctx$eval(sprintf(
    "function formatiere(werte) {
      var zeilen = [];
      var params = werte.map(function(w) {
        return {value: [0, w], marker: '', seriesName: 'S'};
      });
      %s
      return zeilen;
    }",
    js_smc_tooltip_zeilen(unit = "", digits = 1)
  ))
  werte <- c(0, 7, -42, 100, 1234.5, 1234567, -9876.4, 0.19)
  js_zahlen <- sub("^\\s*S: ", "", unlist(ctx$call("formatiere", werte)))
  expect_equal(js_zahlen, format_SMC_number(werte, digits = 1))
})

test_that("e_smc_tooltip(snap) ergaenzt die springende axisPointer-Linie", {
  e <- testchart() |> e_smc_tooltip(unit = " %", snap = TRUE)
  expect_equal(
    e$x$opts$tooltip$axisPointer,
    list(type = "line", snap = TRUE)
  )
  # trigger und Formatter bleiben unangetastet
  expect_equal(e$x$opts$tooltip$trigger, "axis")
  expect_false(is.null(e$x$opts$tooltip$formatter))
  # Default: kein axisPointer
  e <- testchart() |> e_smc_tooltip()
  expect_null(e$x$opts$tooltip$axisPointer)
})
