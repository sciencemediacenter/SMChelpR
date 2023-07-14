########################
## image_helper tests ##
########################



test_that("image_helper() can save a PNG and save_svg = FALSE works", {
  unlink("testfiles_image_helper", recursive = TRUE)

  withr::local_package("ggplot2")
  
  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()
  
  # create png and svg file 
  # test that no caption is set
  expect_snapshot(
    cat(
      image_helper(
        plot = tmp_ggplot,
        filename = "testpng",
        filepath = "testfiles_image_helper",
        save_svg = FALSE
      )
    )
  )

  # check if png and svg file exist
  expect_true(file.exists(file.path("testfiles_image_helper", "testpng.png")))
  expect_true(!file.exists(file.path("testfiles_image_helper", "testpng.svg")))
  unlink("testfiles_image_helper", recursive = TRUE)
})


test_that("image_helper() does show the caption and can save SVGs", {
  unlink("testfiles_image_helper", recursive = TRUE)
  withr::local_package("ggplot2")
  
  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()
  
  # create png and svg file
  # verify correct caption display
  expect_snapshot(
    cat(
      image_helper(
        plot = tmp_ggplot,
        filename = "testfile",
        filepath = "testfiles_image_helper",
        caption = "Dies ist ein Test"
      )
    )
  )
    
  # check if png and svg file exist
  expect_true(file.exists(file.path("testfiles_image_helper", "testfile.png")))
  expect_true(file.exists(file.path("testfiles_image_helper", "testfile.svg")))
  unlink("testfiles_image_helper", recursive = TRUE)
})

test_that("image_helper() test plotly, csv_opt and extra_html_tags params", {
  withr::local_package("ggplot2")
  
  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()

  expect_snapshot(
    cat(
      image_helper(
        plot = tmp_ggplot,
        filename = "testfile",
        filepath = "testfiles_image_helper",
        caption = "Dies ist ein Test",
        plotly = FALSE,
        csv_opt = FALSE,
        extra_html_tags = 'width = "50%"'
      )
    )
  )
  unlink("testfiles_image_helper", recursive = TRUE)
})
