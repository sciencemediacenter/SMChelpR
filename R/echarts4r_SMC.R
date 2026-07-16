# echarts4r_SMC.R — generic building blocks for echarts4r figures in the
# SMC ECharts style (centered title, standard grid, toolbox with
# dataZoom + restore, legend at the bottom, axis-trigger tooltips with
# German formatting). Extracted from the Gasspeicher dashboard migration so
# every SMC app/report applies the same standard by construction instead of
# re-implementing it (see the echarts-style skill for the audit checklist).
#
# German labels come from the package's 'DE' ECharts locale
# (assets/smc-echarts-locale-de.js, attached by e_smc_style()): time axes
# label natively and adaptively (years at year boundaries, months in
# between, days when zoomed), and the tooltip date header formats via
# echarts.time.format(..., 'DE'). Only NUMBER formatting (de-DE "1.234,5")
# still needs a JS formatter — the ECharts locale system does not cover it.

# JS snippet: one tooltip line per series, "<marker> Name: value unit".
# Numbers follow format_SMC_number(): comma as decimal mark, a narrow 50%
# font-size HTML space as thousands separator (tooltips render HTML) and
# whole numbers without decimals. A V8 parity test keeps the two in sync.
# Null values (gaps in pivoted series) are skipped.
js_smc_tooltip_zeilen <- function(unit, digits) {
  sprintf(
    "var trenner = '<span style=\"font-size:50%%;\"> </span>';
    params.forEach(function(p) {
      var wert = Array.isArray(p.value) ? p.value[1] : p.value;
      if (wert === null || wert === undefined || isNaN(wert)) { return; }
      wert = Number(wert);
      var ganz = wert === Math.trunc(wert);
      var teile = Math.abs(wert).toFixed(ganz ? 0 : %d).split('.');
      var vor = teile[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, trenner);
      var zahl = (wert < 0 ? '-' : '') + vor +
        (teile[1] ? ',' + teile[1] : '');
      zeilen.push(p.marker + ' ' + p.seriesName + ': ' + zahl + '%s');
    });",
    digits,
    unit
  )
}

# htmlDependency: German ECharts locale + init default
# (assets/smc-echarts-locale-de.js). Attached by e_smc_style(), so every
# chart built through the SMC standard gets German time-axis labels and
# toolbox titles natively — echarts4r itself exposes no locale option and
# bundles only the EN/ZH locales.
smc_locale_de_dependency <- function() {
  htmltools::htmlDependency(
    name = "smc-echarts-locale-de",
    version = as.character(utils::packageVersion("SMChelpR")),
    package = "SMChelpR",
    src = "assets",
    script = "smc-echarts-locale-de.js"
  )
}

#' e_smc_style
#'
#' Apply the SMC ECharts base style to an `echarts4r` chart in one call:
#' SMC color palette, standard grid (`top = 50`, `left = 80`, `right = 40`;
#' `bottom` depending on the legend), toolbox with `dataZoom`
#' (`yAxisIndex = FALSE`) and `restore`, legend at the bottom (horizontal)
#' and an optional centered title. Also attaches the package's German
#' ECharts locale as an htmlDependency: time axes and toolbox titles then
#' render in German natively, page-wide for all ECharts instances
#' (echarts4r itself exposes no locale option).
#'
#' @details
#' The grid `top` grows by 25 px per additional title line (`"\n"` in
#' `title`), the grid `bottom` is 60 px without a legend and 80 px with one —
#' both following the SMC ECharts style standard. All underlying constants
#' can be overridden selectively via `echarts_params`; see
#' [get_SMC_echarts_default_parameters()] for the full list.
#'
#' @param e an `echarts4r` chart.
#' @param title character or `NULL`. Chart title, centered; use `"\n"` for
#'   multi-line titles.
#' @param legend one of `"bottom"` (horizontal legend at the bottom, default),
#'   `"none"` (hidden) or `"scroll"` (scrollable, for more than ~8 series).
#' @param colors character vector of series colors. Default: [colors_SMC()];
#'   for more series than colors see [colors_SMC_ramp()].
#' @param echarts_params list, selective overrides of the style constants
#'   (see [get_SMC_echarts_default_parameters()]).
#' @return The modified `echarts4r` chart.
#' @examples
#' \dontrun{
#' df |>
#'   echarts4r::e_charts(Datum) |>
#'   echarts4r::e_line(Wert) |>
#'   e_smc_style(title = "Beispiel", legend = "none")
#'
#' # gezielt eine Konstante ueberschreiben
#' df |>
#'   echarts4r::e_charts(Datum) |>
#'   echarts4r::e_line(Wert) |>
#'   e_smc_style(title = "Beispiel", echarts_params = list(grid_left = 100))
#' }
#' @export e_smc_style
e_smc_style <- function(
  e,
  title = NULL,
  legend = c("bottom", "none", "scroll"),
  colors = colors_SMC(),
  echarts_params = list()
) {
  legend <- match.arg(legend)

  extra_zeilen <- if (is.null(title)) {
    0L
  } else {
    lengths(regmatches(title, gregexpr("\n", title, fixed = TRUE)))
  }
  grid_top <- get_param(echarts_params, "grid_top", 50) +
    get_param(echarts_params, "grid_top_per_title_line", 25) * extra_zeilen
  grid_bottom <- if (legend == "none") {
    get_param(echarts_params, "grid_bottom_no_legend", 60)
  } else {
    get_param(echarts_params, "grid_bottom_legend", 80)
  }

  e <- e |>
    echarts4r::e_color(colors) |>
    echarts4r::e_grid(
      top = grid_top,
      left = get_param(echarts_params, "grid_left", 80),
      right = get_param(echarts_params, "grid_right", 40),
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

  # Deutsche ECharts-Locale (Zeitachsen-Labels, Toolbox-Titel) als
  # htmlDependency anhängen — wirkt seitenweit auf alle Instanzen
  # (htmltools dedupliziert beim Rendern über den Dependency-Namen).
  e$dependencies <- c(e$dependencies, list(smc_locale_de_dependency()))
  e
}

# Internal: markLine label for e_smc_hline()/e_smc_vline() — hidden unless
# `label` is a string, in which case it's shown per the SMC annotation-label
# standard (fontSize 11), colored like the line unless told otherwise.
e_smc_markline_label <- function(label, position, color) {
  if (is.null(label)) {
    list(show = FALSE)
  } else {
    list(
      show = TRUE,
      formatter = label,
      position = position,
      color = color,
      fontSize = 11
    )
  }
}

# Internal: attach one markLine data-item (`point`, e.g. `list(yAxis = 100,
# ...)`) to a dedicated invisible "phantom" line series named
# `phantom_name`, creating that series on first use. A markLine shares its
# host series' legend visibility, so a reference line attached directly to
# a real series disappears when that series is toggled off; hosting it on
# its own always-shown series avoids that. The phantom series is excluded
# from the legend by whitelisting whichever series already exist at the
# time it's first created — so this must run after all real data series
# have been added, and before e_smc_style() (which sets other legend keys
# but does not touch an existing legend$data).
#
# Adds one extra entry to e$x$opts$series: if used together with
# e_facet() (which infers its row/col grid from length(series)), call
# e_facet() first, or pass explicit rows/cols, to avoid miscounting panels.
e_smc_add_phantom_markline <- function(e, point, phantom_name) {
  series_names <- vapply(
    e$x$opts$series,
    function(s) if (is.null(s$name)) "" else s$name,
    character(1)
  )
  idx <- which(series_names == phantom_name)

  if (length(idx) == 0) {
    if (is.null(e$x$opts$legend$data)) {
      if (is.null(e$x$opts$legend)) {
        e$x$opts$legend <- list()
      }
      e$x$opts$legend$data <- as.list(series_names)
    }
    e$x$opts$series[[length(e$x$opts$series) + 1]] <- list(
      name = phantom_name,
      type = "line",
      data = list(),
      showSymbol = FALSE,
      silent = TRUE,
      tooltip = list(show = FALSE),
      lineStyle = list(opacity = 0),
      markLine = list(symbol = "none", data = list(point))
    )
  } else {
    e$x$opts$series[[idx]]$markLine$data <- append(
      e$x$opts$series[[idx]]$markLine$data,
      list(point)
    )
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
#' Calling `e_smc_hline()`/`e_smc_vline()` more than once on the same chart
#' is safe — each call's own `color`/`opacity`/`type`/`label` is preserved
#' regardless of how many reference lines already exist.
#'
#' @param e an `echarts4r` chart.
#' @param y numeric, y position of the line.
#' @param opacity numeric line opacity. Default: 0.5.
#' @param color character line color. Default: `"#666666"`.
#' @param type character line style: `"solid"` (default), `"dashed"` or
#'   `"dotted"`.
#' @param label character or `NULL` (default). `NULL` draws an unlabeled
#'   line. A string is shown as the markLine label — `fontSize = 11` per the
#'   SMC annotation-label standard, colored to match `color` (pass a more
#'   legible `color` than the line's if it needs one, e.g. a light line with
#'   a dark label).
#' @param label_position character, ECharts markLine label `position`.
#'   Default: `"insideStartTop"` (label just past the start of the line,
#'   above it).
#' @param use_phantom_series logical. `FALSE` (default) attaches the line to
#'   every series currently on the chart, matching the standard `markLine`
#'   behavior — visible as long as at least one series is shown. Set `TRUE`
#'   to instead host it on a dedicated invisible series, so it stays visible
#'   no matter which real series the reader toggles off via the legend. Add
#'   all real data series to the chart, and call this (with
#'   `use_phantom_series = TRUE`) before `e_smc_style()`.
#' @return The modified `echarts4r` chart.
#' @export e_smc_hline
e_smc_hline <- function(
  e,
  y,
  opacity = 0.5,
  color = "#666666",
  type = "solid",
  label = NULL,
  label_position = "insideStartTop",
  use_phantom_series = FALSE
) {
  point <- list(
    yAxis = y,
    label = e_smc_markline_label(label, label_position, color),
    lineStyle = list(color = color, type = type, opacity = opacity)
  )
  if (use_phantom_series) {
    e_smc_add_phantom_markline(e, point, "SMC_hline_phantom")
  } else {
    echarts4r::e_mark_line(e, symbol = "none", silent = TRUE, data = point)
  }
}

#' e_smc_vline
#'
#' Add a vertical reference line (the `echarts4r` counterpart of
#' `ggplot2::geom_vline()`): a silent `markLine`. Typical uses: a "today"
#' marker on a time axis, or a target date.
#'
#' Calling `e_smc_hline()`/`e_smc_vline()` more than once on the same chart
#' is safe — each call's own `color`/`opacity`/`type`/`label` is preserved
#' regardless of how many reference lines already exist.
#'
#' @param e an `echarts4r` chart.
#' @param x x position of the line — a `Date` on a time axis.
#' @param opacity numeric line opacity. Default: 0.5.
#' @param color character line color. Default: `"#666666"`.
#' @param type character line style: `"solid"` (default), `"dashed"` or
#'   `"dotted"`.
#' @param label character or `NULL` (default), see [e_smc_hline()].
#' @param label_position character, see [e_smc_hline()]. Default:
#'   `"insideStartTop"`.
#' @param use_phantom_series logical, see [e_smc_hline()]. Default: `FALSE`.
#' @return The modified `echarts4r` chart.
#' @export e_smc_vline
e_smc_vline <- function(
  e,
  x,
  opacity = 0.5,
  color = "#666666",
  type = "solid",
  label = NULL,
  label_position = "insideStartTop",
  use_phantom_series = FALSE
) {
  point <- list(
    xAxis = x,
    label = e_smc_markline_label(label, label_position, color),
    lineStyle = list(color = color, type = type, opacity = opacity)
  )
  if (use_phantom_series) {
    e_smc_add_phantom_markline(e, point, "SMC_vline_phantom")
  } else {
    echarts4r::e_mark_line(e, symbol = "none", silent = TRUE, data = point)
  }
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
#' @param echarts_params list, selective overrides of the style constants
#'   (see [get_SMC_echarts_default_parameters()]).
#' @return The modified `echarts4r` chart.
#' @export e_smc_y_percent
e_smc_y_percent <- function(
  e,
  name = NULL,
  extend_to = NULL,
  interval = NULL,
  echarts_params = list()
) {
  achse <- list(
    nameLocation = "middle",
    nameGap = get_param(echarts_params, "y_name_gap", 50),
    axisLabel = list(formatter = "{value} %")
  )
  if (!is.null(name)) {
    achse$name <- name
  }
  if (!is.null(extend_to)) {
    if (is.null(interval)) {
      interval <- get_param(echarts_params, "percent_interval", 20)
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

# Es gibt bewusst kein e_smc_x_time mehr: Zeitachsen brauchen mit der
# 'DE'-Locale (siehe e_smc_style()) keinen SMC-Baustein — deutsche,
# adaptive Tick-Labels kommen nativ von ECharts. Für die verbleibenden
# Sonderfälle reichen native Einzeiler (dokumentiert im
# echarts-style-Skill):
#   Randstreifen rechts:  e_x_axis(boundaryGap = c("0%", "2%"))
#   Jahresdaten:          e_x_axis(axisLabel = list(formatter = "{yyyy}"))
#   Referenzjahr-Overlay: e_x_axis(axisLabel = list(
#                           formatter = list(year = "{MMM}")))

# Auch e_smc_x_category gibt es nicht mehr: Kategorie-Achsen zeigen die
# Achsenlinie per ECharts-Default (nur value-Achsen verstecken sie seit
# v5), und der Rest ist ein nativer Einzeiler (Stilkonstanten rotate 45 /
# fontSize 10 dokumentiert der echarts-style-Skill):
#   e_x_axis(boundaryGap = FALSE,
#            axisLabel = list(rotate = 45, fontSize = 10))
# Kategorie-Werte muessen eindeutig sein (Kalenderwochen ueber
# format_SMC_kalenderwoche(), sonst springen Linien quer durchs Diagramm).

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
#' @param digits integer number of decimal places for non-integer values —
#'   numbers follow [format_SMC_number()]: comma as decimal mark, a narrow
#'   HTML space as thousands separator, whole numbers without decimals.
#'   Default: 1.
#' @param axis_type `"time"` (header is a German date) or `"category"`
#'   (header is the category label). Default: `"time"`.
#' @param show_year logical, include the year in the date header (time axes
#'   only, `granularity = "day"` only — at `granularity = "year"` the year
#'   is always shown). Default: `TRUE`.
#' @param granularity `"day"` (date header "1. Mär 2024") or `"year"`
#'   (header is just the year, e.g. "1950") — time axes only, matching the
#'   label granularity of the x axis. Default: `"day"`.
#' @param snap logical: add a vertical axis-pointer line that snaps to the
#'   nearest data point instead of following the mouse — visible feedback
#'   for which day is selected on dense time axes. This must be set here
#'   rather than via a second [echarts4r::e_tooltip()] call, which would
#'   reset `trigger` and `formatter` to their defaults. Default: `FALSE`.
#' @return The modified `echarts4r` chart.
#' @export e_smc_tooltip
e_smc_tooltip <- function(
  e,
  unit = "",
  digits = 1,
  axis_type = c("time", "category"),
  show_year = TRUE,
  granularity = c("day", "year"),
  snap = FALSE
) {
  axis_type <- match.arg(axis_type)
  granularity <- match.arg(granularity)

  vorlage <- if (granularity == "year") {
    "{yyyy}"
  } else if (show_year) {
    "{d}. {MMM} {yyyy}"
  } else {
    "{d}. {MMM}"
  }
  # Datums-Kopf ueber echarts.time.format mit der 'DE'-Locale des Pakets
  # (registriert durch das von e_smc_style() angehaengte Asset; ohne
  # Registrierung faellt ECharts still auf englische Monatsnamen zurueck).
  # Der vierte lang-Parameter fehlt in der API-Doku, ist aber seit
  # ECharts 5 Teil der stabilen Signatur von time.format.
  kopf <- if (axis_type == "time") {
    sprintf(
      "var kopf = echarts.time.format(params[0].axisValue, '%s', false, 'DE');",
      vorlage
    )
  } else {
    "var kopf = params[0].axisValue;"
  }

  args <- list(
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
  if (snap) {
    args$axisPointer <- list(type = "line", snap = TRUE)
  }
  do.call(echarts4r::e_tooltip, args)
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
#' @param echarts_params list, selective overrides of the style constants
#'   (see [get_SMC_echarts_default_parameters()]).
#' @return An `echarts4r` chart.
#' @export e_smc_placeholder
e_smc_placeholder <- function(
  message = "Daten derzeit nicht verfügbar",
  echarts_params = list()
) {
  e <- echarts4r::e_charts() |>
    echarts4r::e_title(
      text = message,
      left = "center",
      top = "middle",
      textStyle = list(
        color = get_param(
          echarts_params,
          "placeholder_color",
          colors_SMC("grey")
        ),
        fontWeight = get_param(
          echarts_params,
          "placeholder_font_weight",
          "normal"
        )
      )
    )
  e$x$opts$yAxis <- NULL
  e
}

#' get_SMC_echarts_default_parameters
#'
#' Returns the style constants used by the `e_smc_*` functions — the SMC
#' ECharts style standard as data. Modify entries selectively and pass the
#' list to the respective function via its `echarts_params` argument
#' (analogous to [get_SMC_ggplotly_default_parameters()] /
#' `ggplotly_params`).
#'
#' Per-figure semantics (title, legend variant, unit, digits, rotation,
#' `extend_to`) remain regular function arguments; `echarts_params` only
#' carries the style constants that are otherwise invisible defaults.
#'
#' @return List of default parameters for the `e_smc_*` functions:
#' * `grid_top`, `grid_top_per_title_line`, `grid_left`, `grid_right`,
#'   `grid_bottom_legend`, `grid_bottom_no_legend` — grid geometry in px
#'   ([e_smc_style()]).
#' * `y_name_gap`, `percent_interval` — y axis ([e_smc_y_percent()]).
#' * `placeholder_color`, `placeholder_font_weight` —
#'   ([e_smc_placeholder()]).
#' @examples
#' get_SMC_echarts_default_parameters()
#' @export get_SMC_echarts_default_parameters
get_SMC_echarts_default_parameters <- function() {
  list(
    # grid (px) — top grows per additional title line
    grid_top = 50,
    grid_top_per_title_line = 25,
    grid_left = 80,
    grid_right = 40,
    grid_bottom_legend = 80,
    grid_bottom_no_legend = 60,

    # y axis
    y_name_gap = 50,
    percent_interval = 20,

    # placeholder chart
    placeholder_color = colors_SMC("grey"),
    placeholder_font_weight = "normal"
  )
}
