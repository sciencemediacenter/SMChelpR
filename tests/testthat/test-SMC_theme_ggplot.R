############################
## SMC_theme_ggplot tests ##
############################


test_that("SMC_theme_ggplot() creates figures in the SMC-CI", {
  withr::local_package("ggplot2")
  SMChelpR::SMC_theme_ggplot()

  # create lineplot with points in SMC-CI
  tmp_ggplot <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz")  
 
  vdiffr::expect_doppelganger(
    title = "Beispiel-Plot im SMC-Theme",
    fig = tmp_ggplot
  )
})
