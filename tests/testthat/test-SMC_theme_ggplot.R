############################
## SMC_theme_ggplot tests ##
############################


test_that("SMC_theme_ggplot() creates figures in the SMC-CI", {
  withr::local_package("ggplot2")
  
  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()
  
  vdiffr::expect_doppelganger(title = "Beispiel-Plot im SMC-Theme",
                              fig = tmp_ggplot)
})

test_that("SMC_theme_ggplot() creates figures in the SMC-CI with custom parameters", {
  withr::local_package("ggplot2")

  # create lineplot with points in SMC-CI and custom parameters
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot(
      theme_params = list(
        margin_SMC = 10,
        title_size = 20,
        caption_hjust = 0.5,
        axis_title_size = 14,
        axis_text_size = 12,
        legend_title_size = 14,
        legend_text_size = 12,
        linewidth_in_pt = 0.9,
        pointsize_in_pt = 1.8
      )
    )

  vdiffr::expect_doppelganger(title = "Beispiel-Plot im SMC-Theme mit custom Parametern",
                              fig = tmp_ggplot)
})


test_that("get_SMC_theme_ggplot_default_parameters() returns the default parameters", {
  withr::local_package("ggplot2")
  
  # get the default parameters
  default_params <- get_SMC_theme_ggplot_default_parameters()
  
  expect_type(default_params, "list")
  
  expect_equal(names(default_params),
               c("title_hjust",
                 "title_size",
                 "background_fill",
                 "background_colour",
                 "grid_linewidth",
                 "grid_colour",
                 "axis_title_size", 
                 "axis_text_size", 
                 "legend_position",
                 "legend_title_size", 
                 "legend_text_size", 
                 "caption_hjust", 
                 "margin_SMC", 
                 "linewidth_in_pt", 
                 "pointsize_in_pt"
                 ))
  
  # just check one parameter per type
  expect_equal(default_params$margin_SMC, 8)
  expect_equal(default_params$background_fill, "white")
  expect_equal(default_params$title_size, size_in_pt(18))
})