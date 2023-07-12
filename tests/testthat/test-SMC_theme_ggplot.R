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
