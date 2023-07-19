########################
## ggplotly_SMC tests ##
########################

# test whether some parameters are set in the plotly object

test_that("ggplotly_SMC() legend params are set by default", {
  withr::local_package("ggplot2")
  withr::local_package("plotly")

  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()


  # create plotly object    
  tmp_ggplotly <- tmp_ggplot |> ggplotly_SMC()
  
  # build to JSON to get rid of changing ID in paths
  layout_params <- plotly_build(tmp_ggplotly)$x$layout
  legend <- layout_params$legend

  # check if legend params are set
  expect_equal(legend$orientation, "h")
  expect_equal(legend$entrywidth, 70)
  expect_equal(legend$yanchor, "bottom")
  expect_equal(legend$y, -0.28)
  expect_equal(legend$xanchor, "center")
  expect_equal(legend$x, 0.5)
})

# verify that legend position is not set if legende_unten = FALSE
test_that("ggplotly_SMC() does not place legend below if param is FALSE", {
  withr::local_package("ggplot2")
  withr::local_package("plotly")

  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()

  # create plotly object
  tmp_ggplotly <- tmp_ggplot |> ggplotly_SMC(legende_unten = FALSE)

  # build to JSON to get rid of changing ID in paths
  layout_params <- plotly_build(tmp_ggplotly)$x$layout
  legend <- layout_params$legend

  # check if legend position is not bottom
  expect_null(legend$yanchor)
})

