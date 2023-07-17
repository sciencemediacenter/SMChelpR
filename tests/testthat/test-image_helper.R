########################
## image_helper tests ##
########################



test_that("image_helper() can save a PNG and save_svg = FALSE works", {
  unlink(file.path(tempdir(), "testfiles_image_helper"), recursive = TRUE)

  withr::local_package("ggplot2")
  
  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()
  
  # create png and svg file 
  helper_output <-  image_helper(
    plot = tmp_ggplot,
    filename = "testpng",
    filepath = file.path(tempdir(), "testfiles_image_helper"),
    save_svg = FALSE
  )

  # test that no caption is set
  expect_snapshot(
    cat(
      gsub(
        paste0(
          toString(tempdir()),
          "/"
        ),
        "",
        helper_output,
        fixed = TRUE
      )
    )
  )

  # check if png and svg file exist
  expect_true(file.exists(file.path(tempdir(), "testfiles_image_helper", "testpng.png")))
  expect_true(!file.exists(file.path(tempdir(), "testfiles_image_helper", "testpng.svg")))
  unlink(file.path(tempdir(), "testfiles_image_helper"), recursive = TRUE)
})


test_that("image_helper() does show the caption and can save SVGs. Also: extra_html_tags", {
  unlink(file.path(tempdir(), "testfiles_image_helper"), recursive = TRUE)
  withr::local_package("ggplot2")
  
  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()
  
  # create png and svg file
  helper_output <- image_helper(
    plot = tmp_ggplot,
    filename = "testfile",
    filepath = file.path(tempdir(), "testfiles_image_helper"),
    caption = "Dies ist ein Test",
    extra_html_tags = 'width = "50%"'
  )

  # verify correct caption display
  expect_snapshot(
    cat(
      gsub(
        paste0(
          toString(tempdir()),
          "/"
        ),
        "",
        helper_output,
        fixed = TRUE
      )
    )
  )
    
  # check if png and svg file exist
  expect_true(file.exists(file.path(tempdir(), "testfiles_image_helper", "testfile.png")))
  expect_true(file.exists(file.path(tempdir(), "testfiles_image_helper", "testfile.svg")))
  unlink(file.path(tempdir(), "testfiles_image_helper"), recursive = TRUE)
})

test_that("image_helper() test plotly and csv_opt params", {
  withr::local_package("ggplot2")
  
  # create lineplot with points in SMC-CI
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()

  helper_output <- image_helper(
    plot = tmp_ggplot,
    filename = "testfile",
    filepath = file.path(tempdir(), "testfiles_image_helper"),
    caption = "Dies ist ein Test",
    plotly = TRUE,
    csv_opt = FALSE
  )

  expect_snapshot(
    cat(
      gsub(
        paste0(
          toString(tempdir()),
          "/"
        ),
        "",
        helper_output,
        fixed = TRUE
      )
    )
  )
  unlink(file.path(tempdir(), "testfiles_image_helper"), recursive = TRUE)
})
