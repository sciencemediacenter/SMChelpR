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


# verify that negative y shift of legend depends on x_axis_label_gegeben and mehrzeiliger_titel
test_that("ggplotly_SMC() decreases legend_y if x axis label is not given", {
  withr::local_package("ggplot2")
  withr::local_package("plotly")
  
  tmp_ggplot_with_xlabel <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz", x = "TEST") +
    SMC_theme_ggplot()
  
  tmp_ggplot_without_xlabel <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz", x = "") +
    SMC_theme_ggplot()
  
  tmp_ggplot_with_mehrzeiliger_titel <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz \n test test test", x = "") +
    SMC_theme_ggplot()
  
  # create plotly objects
  tmp_ggplotly_with <- tmp_ggplot_with_xlabel |> ggplotly_SMC(x_axis_label_gegeben = TRUE, mehrzeiliger_titel = FALSE)
  tmp_ggplotly_without <- tmp_ggplot_without_xlabel |> ggplotly_SMC(x_axis_label_gegeben = FALSE, mehrzeiliger_titel = FALSE)
  tmp_ggplotly_with_and_mehrzeiliger_titel <- tmp_ggplot_with_mehrzeiliger_titel |> ggplotly_SMC(x_axis_label_gegeben = TRUE, mehrzeiliger_titel = TRUE)
  
  # build to JSON to get rid of changing ID in paths
  legend_y_with <- plotly_build(tmp_ggplotly_with)$x$layout$legend$y
  legend_y_without <- plotly_build(tmp_ggplotly_without)$x$layout$legend$y
  legend_y_with_and_mehrzeiliger_titel <- plotly_build(tmp_ggplotly_with_and_mehrzeiliger_titel)$x$layout$legend$y
  
  # check if absolute legend shifts are correct
  expect_equal(legend_y_with, -0.28)
  expect_equal(legend_y_without, -0.18)
  expect_equal(legend_y_with_and_mehrzeiliger_titel, -0.38)
  
  expect_true(abs(legend_y_with) > abs(legend_y_without))
  expect_true(abs(legend_y_with_and_mehrzeiliger_titel) > abs(legend_y_without))
  expect_true(abs(legend_y_with_and_mehrzeiliger_titel) > abs(legend_y_with))
  
})


# verify that font dependencies are attached if interactive == TRUE
test_that("ggplotly_SMC() attached font dependencies are attached if interactive == TRUE", {
  withr::local_package("ggplot2")
  withr::local_package("plotly")
  
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()
  
  # create plotly objects
  tmp_ggplotly <- tmp_ggplot |> ggplotly_SMC(interaktiv = TRUE)
  
  # build to JSON to get rid of changing ID in paths
  dependencies <- plotly_build(tmp_ggplotly)$dependencies
  
  # Logical check for any element matching the condition
  has_matching_element <- any(sapply(dependencies, function(element) {
    is.list(element) &&
      !is.null(element$src$href) && grepl("https://media.sciencemediacenter.de/static/fonts/circular", element$src$href) &&
      !is.null(element$stylesheet) && element$stylesheet == "web.css"
  }))
  
  # Expectation
  expect_true(has_matching_element, "The list does not contain an element with the specified href and stylesheet.")
})
