# echarts4r_SMC.R — generic building blocks for echarts4r figures in the
# SMC ECharts style (centered title, standard grid, toolbox with
# dataZoom + restore, legend at the bottom, axis-trigger tooltips with
# German formatting). Extracted from the Gasspeicher dashboard migration so
# every SMC app/report applies the same standard by construction instead of
# re-implementing it (see the echarts-style skill for the audit checklist).

# German month abbreviations as a JS array literal — echarts4r (<= 0.5.x) has
# no locale argument, so German labels need explicit JS formatters.
js_smc_monate <- "['Jan','Feb','Mär','Apr','Mai','Jun','Jul','Aug','Sep','Okt','Nov','Dez']"

# JS snippet: one tooltip line per series, "<marker> Name: value unit" with
# de-DE number formatting; null values (gaps in pivoted series) are skipped.
js_smc_tooltip_zeilen <- function(unit, digits) {
  sprintf(
    "params.forEach(function(p) {
      var wert = Array.isArray(p.value) ? p.value[1] : p.value;
      if (wert === null || wert === undefined || isNaN(wert)) { return; }
      zeilen.push(
        p.marker + ' ' + p.seriesName + ': ' +
          Number(wert).toLocaleString('de-DE', {
            minimumFractionDigits: %d,
            maximumFractionDigits: %d
          }) + '%s'
      );
    });",
    digits,
    digits,
    unit
  )
}

# JS axis-label formatter for time axes: "Mär '24" (or "Mär" without year).
js_smc_achse_monat <- function(show_year = TRUE) {
  jahr_teil <- if (show_year) {
    "+ \" '\" + String(d.getFullYear()).slice(-2)"
  } else {
    ""
  }
  htmlwidgets::JS(sprintf(
    "function(value) {
      var monate = %s;
      var d = new Date(value);
      return monate[d.getMonth()] %s;
    }",
    js_smc_monate,
    jahr_teil
  ))
}

#' e_smc_style
#'
#' Apply the SMC ECharts base style to an `echarts4r` chart in one call:
#' SMC color palette, standard grid (`top = 50`, `left = 80`, `right = 40`;
#' `bottom` depending on the legend), toolbox with `dataZoom`
#' (`yAxisIndex = FALSE`) and `restore`, legend at the bottom (horizontal)
#' and an optional centered title.
#'
#' @details
#' The grid `top` grows by 25 px per additional title line (`"\n"` in
#' `title`), the grid `bottom` is 60 px without a legend and 80 px with one —
#' both following the SMC ECharts style standard. Pass `grid_top`/
#' `grid_bottom` to override.
#'
#' @param e an `echarts4r` chart.
#' @param title character or `NULL`. Chart title, centered; use `"\n"` for
#'   multi-line titles.
#' @param legend one of `"bottom"` (horizontal legend at the bottom, default),
#'   `"none"` (hidden) or `"scroll"` (scrollable, for more than ~8 series).
#' @param grid_top,grid_bottom numeric or `NULL`. Explicit grid distances in
#'   px; if `NULL` they are derived from `title` and `legend`.
#' @param colors character vector of series colors. Default: [colors_SMC()];
#'   for more series than colors see [colors_SMC_ramp()].
#' @return The modified `echarts4r` chart.
#' @examples
#' \dontrun{
#' df |>
#'   echarts4r::e_charts(Datum) |>
#'   echarts4r::e_line(Wert) |>
#'   e_smc_style(title = "Beispiel", legend = "none")
#' }
#' @export e_smc_style
e_smc_style <- function(
  e,
  title = NULL,
  legend = c("bottom", "none", "scroll"),
  grid_top = NULL,
  grid_bottom = NULL,
  colors = colors_SMC()
) {
  legend <- match.arg(legend)

  if (is.null(grid_top)) {
    extra_zeilen <- if (is.null(title)) {
      0L
    } else {
      lengths(regmatches(title, gregexpr("\n", title, fixed = TRUE)))
    }
    grid_top <- 50 + 25 * extra_zeilen
  }
  if (is.null(grid_bottom)) {
    grid_bottom <- if (legend == "none") 60 else 80
  }

  e <- e |>
    echarts4r::e_color(colors) |>
    echarts4r::e_grid(
      top = grid_top,
      left = 80,
      right = 40,
      bottom = grid_bottom
    ) |>
    echarts4r::e_toolbox_feature("dataZoom", yAxisIndex = FALSE) |>
    echarts4r::e_toolbox_feature("restore")

  e <- switch(
    legend,
    none = echarts4r::e_legend(e, show = FALSE),
    bottom = echarts4r::e_legend(e, bottom = 0, orient = "horizontal"),
    scroll = echarts4r::e_legend(
      e,
      bottom = 0,
      orient = "horizontal",
      type = "scroll"
    )
  )

  if (!is.null(title)) {
    e <- echarts4r::e_title(e, text = title, left = "center")
  }
  e
}

#' e_smc_hline
#'
#' Add a horizontal reference line (the `echarts4r` counterpart of
#' `ggplot2::geom_hline()`): a silent, unlabeled, solid `markLine`.
#'
#' Note that unlike `geom_hline()` a `markLine` does NOT extend the axis
#' range — if the reference value can lie above the data maximum, combine
#' this with [e_smc_y_percent()] and its `extend_to` argument (or an explicit
#' axis `max`), otherwise the line is silently clipped.
#'
#' @param e an `echarts4r` chart.
#' @param y numeric, y position of the line.
#' @param opacity numeric line opacity. Default: 0.5.
#' @param color character line color. Default: `"#666666"`.
#' @return The modified `echarts4r` chart.
#' @export e_smc_hline
e_smc_hline <- function(e, y, opacity = 0.5, color = "#666666") {
  echarts4r::e_mark_line(
    e,
    data = list(yAxis = y),
    symbol = "none",
    silent = TRUE,
    label = list(show = FALSE),
    lineStyle = list(color = color, type = "solid", opacity = opacity)
  )
}

#' e_smc_y_percent
#'
#' Percent y axis in the SMC style: labels as `"{value} %"` and an optional
#' vertical axis title (`nameLocation = "middle"`).
#'
#' @details
#' `extend_to` guarantees that the axis covers at least this value — use it
#' when a reference line (e.g. the 100 % mark via [e_smc_hline()]) may lie
#' above the data maximum: ECharts does not extend the axis for mark lines,
#' so without it the line would be clipped. The axis maximum is rounded up
#' to the next multiple of `interval`.
#'
#' @param e an `echarts4r` chart.
#' @param name character or `NULL`. Vertical axis title.
#' @param extend_to numeric or `NULL`. Minimum value the axis must reach
#'   (e.g. 105 so a 100 % line stays visible).
#' @param interval numeric or `NULL`. Fixed tick interval; defaults to 20
#'   when `extend_to` is set.
#' @return The modified `echarts4r` chart.
#' @export e_smc_y_percent
e_smc_y_percent <- function(e, name = NULL, extend_to = NULL, interval = NULL) {
  achse <- list(
    nameLocation = "middle",
    nameGap = 50,
    axisLabel = list(formatter = "{value} %")
  )
  if (!is.null(name)) {
    achse$name <- name
  }
  if (!is.null(extend_to)) {
    if (is.null(interval)) {
      interval <- 20
    }
    achse$max <- htmlwidgets::JS(sprintf(
      "function(value) {
        return Math.ceil(Math.max(value.max, %s) / %s) * %s;
      }",
      extend_to,
      interval,
      interval
    ))
  }
  if (!is.null(interval)) {
    achse$interval <- interval
  }
  do.call(echarts4r::e_y_axis, c(list(e), achse))
}

#' e_smc_x_time
#'
#' Time x axis in the SMC style: visible axis line and German month labels
#' ("Mär '24", or "Mär" without the year) — echarts4r has no locale
#' argument, so this injects a JS formatter.
#'
#' @param e an `echarts4r` chart.
#' @param show_year logical, append the two-digit year to the month label.
#'   Set to `FALSE` when all series are mapped onto one common year (e.g.
#'   year-over-year comparisons). Default: `TRUE`.
#' @return The modified `echarts4r` chart.
#' @export e_smc_x_time
e_smc_x_time <- function(e, show_year = TRUE) {
  echarts4r::e_x_axis(
    e,
    axisLine = list(show = TRUE),
    axisLabel = list(formatter = js_smc_achse_monat(show_year))
  )
}

#' e_smc_x_category
#'
#' Category x axis in the SMC style: visible axis line (per the style
#' standard this must be set explicitly on category axes), no boundary gap
#' (line charts start at the axis) and rotated labels for dense categories
#' such as calendar weeks (`rotate = 45` implies `fontSize = 10`).
#'
#' Category values MUST be unique — duplicate labels are mapped onto the
#' same x position and lines jump across the chart. For calendar weeks use
#' [format_SMC_kalenderwoche()], which avoids duplicate labels at year
#' boundaries.
#'
#' @param e an `echarts4r` chart.
#' @param rotate numeric label rotation in degrees. Default: 45.
#' @return The modified `echarts4r` chart.
#' @export e_smc_x_category
e_smc_x_category <- function(e, rotate = 45) {
  label <- list(rotate = rotate)
  if (rotate == 45) {
    label$fontSize <- 10
  }
  echarts4r::e_x_axis(
    e,
    boundaryGap = FALSE,
    axisLine = list(show = TRUE),
    axisLabel = label
  )
}

#' e_smc_tooltip
#'
#' Axis-trigger tooltip in the SMC style with German formatting: a bold
#' header (date for time axes, category label otherwise) and one line per
#' series with de-DE number formatting ("1.234,5"). Null values — gaps in
#' pivoted series — are skipped.
#'
#' @param e an `echarts4r` chart.
#' @param unit character appended to each value, e.g. `" %"` or `" GWh/d"`.
#'   Default: `""`.
#' @param digits integer number of decimal places. Default: 1.
#' @param axis_type `"time"` (header is a German date) or `"category"`
#'   (header is the category label). Default: `"time"`.
#' @param show_year logical, include the year in the date header (time axes
#'   only). Default: `TRUE`.
#' @return The modified `echarts4r` chart.
#' @export e_smc_tooltip
e_smc_tooltip <- function(
  e,
  unit = "",
  digits = 1,
  axis_type = c("time", "category"),
  show_year = TRUE
) {
  axis_type <- match.arg(axis_type)

  kopf <- if (axis_type == "time") {
    sprintf(
      "var monate = %s;
      var d = new Date(params[0].axisValue);
      var kopf = d.getDate() + '. ' + monate[d.getMonth()]%s;",
      js_smc_monate,
      if (show_year) " + ' ' + d.getFullYear()" else ""
    )
  } else {
    "var kopf = params[0].axisValue;"
  }

  echarts4r::e_tooltip(
    e,
    trigger = "axis",
    formatter = htmlwidgets::JS(sprintf(
      "function(params) {
        if (!params.length) { return ''; }
        %s
        var zeilen = ['<b>' + kopf + '</b>'];
        %s
        return zeilen.join('<br/>');
      }",
      kopf,
      js_smc_tooltip_zeilen(unit, digits)
    ))
  )
}

#' e_smc_placeholder
#'
#' Placeholder chart for the "no data (yet)" state, e.g. while an async
#' fetch is running or the backend is unreachable: an empty chart with a
#' centered grey message.
#'
#' Implementation note: `echarts4r::e_charts()` without data emits a `yAxis`
#' without an `xAxis`, which crashes ECharts at render time (`axisBuilder`
#' error) — the placeholder therefore removes the coordinate system
#' entirely.
#'
#' @param message character message shown in the chart center. Default:
#'   `"Daten derzeit nicht verfügbar"`.
#' @return An `echarts4r` chart.
#' @export e_smc_placeholder
e_smc_placeholder <- function(message = "Daten derzeit nicht verfügbar") {
  e <- echarts4r::e_charts() |>
    echarts4r::e_title(
      text = message,
      left = "center",
      top = "middle",
      textStyle = list(color = colors_SMC("grey"), fontWeight = "normal")
    )
  e$x$opts$yAxis <- NULL
  e
}

#' colors_SMC_ramp
#'
#' SMC color palette for exactly `n` series: the first `n` SMC colors, or —
#' when `n` exceeds the palette size — `n` distinct colors interpolated
#' across the full palette via `grDevices::colorRampPalette()`. Use this
#' instead of letting ECharts recycle the palette (recycling assigns the
#' same color to different series).
#'
#' @param n integer number of colors needed.
#' @return Character vector of `n` color codes.
#' @examples
#' colors_SMC_ramp(3)
#' colors_SMC_ramp(16)
#' @export colors_SMC_ramp
colors_SMC_ramp <- function(n) {
  farben <- colors_SMC()
  if (n <= length(farben)) {
    return(farben[seq_len(n)])
  }
  grDevices::colorRampPalette(farben)(n)
}

#' format_SMC_kalenderwoche
#'
#' Format dates as German calendar-week labels ("KW 05, 2026") using the ISO
#' week AND the ISO week-based year (`%G`, not `%Y`). With the calendar year
#' the labels collide at year boundaries — e.g. 2024-12-30 is ISO week 1 of
#' 2025 but `%Y` labels it "KW 01, 2024", the same as 2024-01-01. Harmless
#' as tooltip text, fatal as (category) axis values, where duplicate labels
#' make lines jump across the chart.
#'
#' @param x a `Date` (or date-time) vector.
#' @return Character vector of labels like `"KW 01, 2025"`.
#' @examples
#' format_SMC_kalenderwoche(as.Date("2024-12-30")) # "KW 01, 2025", nicht 2024
#' @export format_SMC_kalenderwoche
format_SMC_kalenderwoche <- function(x) {
  strftime(x, format = "KW %V, %G")
}
