#########################
## format_SMC_* tests  ##
#########################

small_space <- '<span style="font-size:50%;"> </span>'

test_that("format_SMC_number() formats integers without decimals", {
  expect_equal(format_SMC_number(100), "100")
  expect_equal(format_SMC_number(1000), paste0("1", small_space, "000"))
})

test_that("format_SMC_number() uses comma decimal mark and thin-space thousands separator", {
  expect_equal(format_SMC_number(1234.5), paste0("1", small_space, "234,5"))
  expect_equal(format_SMC_number(105.526), "105,5")
  expect_equal(format_SMC_number(105.526, digits = 2), "105,53")
})

test_that("format_SMC_number() renders NA as 'keine Daten' and is vectorised", {
  expect_equal(format_SMC_number(NA), "keine Daten")
  expect_equal(
    format_SMC_number(c(100, 1234.5, NA)),
    c("100", paste0("1", small_space, "234,5"), "keine Daten")
  )
})

test_that("format_SMC_number() supports a custom na_text", {
  expect_equal(format_SMC_number(NA, na_text = "n/a"), "n/a")
  expect_equal(
    format_SMC_number(c(100, NA), na_text = "-"),
    c("100", "-")
  )
})

test_that("format_SMC_number() supports custom decimal_mark and big_mark", {
  expect_equal(
    format_SMC_number(1234.5, big_mark = ".", decimal_mark = ","),
    "1.234,5"
  )
  expect_equal(
    format_SMC_number(1234567, big_mark = " "),
    "1 234 567"
  )
  # "thin_space" sentinel remains the default
  expect_equal(format_SMC_number(1000), paste0("1", small_space, "000"))
})

test_that("format_SMC_datatable() adds a columnDef for the requested column", {
  withr::local_package("DT")

  df <- data.frame(
    Energietraeger = c("Wind", "Solar"),
    Leistung = c(1234.5, 980)
  )
  tbl <- datatable(df, rownames = FALSE) |>
    format_SMC_datatable(columns = "Leistung")

  defs <- tbl$x$options$columnDefs
  expect_true(length(defs) >= 1)

  last_def <- defs[[length(defs)]]
  # "Leistung" is the second column -> 0-based target index 1
  expect_equal(last_def$targets, 1)
  expect_s3_class(last_def$render, "JS_EVAL")
})

test_that("format_SMC_datatable() defaults to one decimal (digits = 1)", {
  withr::local_package("DT")

  df <- data.frame(Leistung = c(1234.5, 980))
  tbl <- datatable(df, rownames = FALSE) |>
    format_SMC_datatable(columns = "Leistung")

  render <- as.character(
    tbl$x$options$columnDefs[[length(tbl$x$options$columnDefs)]]$render
  )
  expect_match(render, "var decimals = 1;", fixed = TRUE)
})

test_that("format_SMC_datatable() supports dynamic decimals via digits = NULL", {
  withr::local_package("DT")

  df <- data.frame(Leistung = c(1234.5, 980))
  tbl <- datatable(df, rownames = FALSE) |>
    format_SMC_datatable(columns = "Leistung", digits = NULL)

  render <- as.character(
    tbl$x$options$columnDefs[[length(tbl$x$options$columnDefs)]]$render
  )
  expect_match(render, "Math.floor(data)", fixed = TRUE)
})

test_that("format_SMC_datatable() appends to existing columnDefs without dropping them", {
  withr::local_package("DT")

  df <- data.frame(a = 1.5, b = 2.5)
  base_tbl <- datatable(
    df,
    rownames = FALSE,
    options = list(columnDefs = list(list(targets = 0, visible = TRUE)))
  )
  n_before <- length(base_tbl$x$options$columnDefs)

  tbl <- base_tbl |> format_SMC_datatable(columns = "b")
  defs <- tbl$x$options$columnDefs

  # exactly one render def added, all previous defs preserved
  expect_equal(length(defs), n_before + 1)
  expect_identical(defs[seq_len(n_before)], base_tbl$x$options$columnDefs)
})
