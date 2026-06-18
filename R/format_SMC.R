#' format_SMC_number
#'
#' Format a numeric value (or vector) as a string in SMC style for use in
#' inline text or rendered HTML. By default a comma is used as the decimal mark
#' and a narrow (50% font-size) HTML space as the thousands separator, both
#' configurable via `decimal_mark` and `big_mark`. Whole numbers are shown
#' without decimals, non-integers with `digits` decimals. `NA` is rendered as
#' `na_text`.
#'
#' @param x numeric, value or vector to be formatted.
#' @param digits integer, number of decimals used for non-integer values
#'   (whole numbers are always shown without decimals). Defaults to 1.
#' @param decimal_mark character, decimal mark used in the output. Defaults to ",".
#' @param big_mark character, thousands separator. The sentinel "thin_space"
#'   (default) inserts a narrow 50% font-size HTML space; any other string is
#'   used verbatim.
#' @param na_text character, text used for `NA` values. Defaults to "keine Daten".
#' @return character vector of the same length as `x` with the formatted values.
#' @examples
#' format_SMC_number(105.526)
#' format_SMC_number(c(100, 1234.5, NA), digits = 1)
#' format_SMC_number(1234.5, big_mark = ".", decimal_mark = ",")
#' @export format_SMC_number
format_SMC_number <- function(
  x,
  digits = 1,
  decimal_mark = ",",
  big_mark = "thin_space",
  na_text = "keine Daten"
) {
  separator <- if (identical(big_mark, "thin_space")) {
    '<span style="font-size:50%;"> </span>'
  } else {
    big_mark
  }
  mapply(
    function(val) {
      if (is.na(val)) {
        return(na_text)
      }
      if (val == floor(val)) {
        formatC(
          val,
          format = "f",
          digits = 0,
          big.mark = separator,
          decimal.mark = decimal_mark
        )
      } else {
        formatC(
          val,
          format = "f",
          digits = digits,
          big.mark = separator,
          decimal.mark = decimal_mark
        )
      }
    },
    x,
    USE.NAMES = FALSE
  )
}


#' format_SMC_datatable
#'
#' Apply SMC-style number formatting to one or more columns of a `DT::datatable`
#' object. The formatting is performed client-side via a DataTables render
#' callback, so it only affects the displayed value (comma as decimal mark,
#' narrow space as thousands separator) while sorting and filtering keep working
#' on the underlying numeric data. `NA`/`null` cells are rendered as `na_text`.
#'
#' By default (`digits = 1`) every value is shown with one decimal. Pass
#' `digits = NULL` for a dynamic mode that shows whole numbers without decimals
#' and non-integers with one decimal.
#'
#' @param table a `datatable` htmlwidget created with [DT::datatable()].
#' @param columns character, names of the columns to format (matched against
#'   the column names of the table data).
#' @param digits integer or NULL, number of decimals. An integer (default `1`)
#'   forces that number of decimals for every value; `NULL` uses 0 decimals for
#'   whole numbers and 1 decimal otherwise.
#' @param decimal_mark character, decimal mark used in the displayed value.
#'   Defaults to ",".
#' @param big_mark character, thousands separator. The sentinel "thin_space"
#'   (default) inserts a narrow 50% font-size HTML space; any other string is
#'   used verbatim.
#' @param na_text character, text shown for `NA`/`null` cells. Defaults to
#'   "keine Daten".
#' @return the `datatable` object with an added column render definition.
#' @examples
#' library(DT)
#' df <- data.frame(Energietraeger = c("Wind", "Solar"), Leistung = c(1234.5, 980))
#' datatable(df, rownames = FALSE) |>
#'   format_SMC_datatable(columns = "Leistung")
#' @export format_SMC_datatable
format_SMC_datatable <- function(
  table,
  columns,
  digits = 1,
  decimal_mark = ",",
  big_mark = "thin_space",
  na_text = "keine Daten"
) {
  separator <- if (identical(big_mark, "thin_space")) {
    '<span style="font-size:50%;"> </span>'
  } else {
    big_mark
  }
  js_esc <- function(s) gsub("'", "\\\\'", s)
  if (is.null(digits)) {
    digits_js <- "var decimals = (data === Math.floor(data)) ? 0 : 1;"
  } else {
    digits_js <- sprintf("var decimals = %d;", as.integer(digits))
  }
  js_lines <- c(
    "function(data, type, row, meta) {",
    "  if (type !== 'display') return data;",
    sprintf(
      "  if (data === null || data === undefined) return '%s';",
      js_esc(na_text)
    ),
    paste0("  ", digits_js),
    "  var parts = data.toFixed(decimals).replace('.', ',').split(',');",
    sprintf("  var sep = '%s';", gsub("\"", "\\\\\"", separator)),
    sprintf("  var decMark = '%s';", js_esc(decimal_mark)),
    "  parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, sep);",
    "  return parts.join(decMark);",
    "}"
  )
  js_render <- JS(paste(js_lines, collapse = "\n"))
  col_indices <- which(names(table$x$data) %in% columns) - 1
  existing_defs <- if (is.null(table$x$options$columnDefs)) {
    list()
  } else {
    table$x$options$columnDefs
  }
  new_def <- list(list(targets = col_indices, render = js_render))
  table$x$options$columnDefs <- c(existing_defs, new_def)
  table
}
