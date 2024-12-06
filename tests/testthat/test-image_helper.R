########################
## image_helper tests ##
########################



test_that("image_helper() can save a PNG and save_svg = FALSE works", {
  # Define the directory path
  target_dir <- file.path(tempdir(), "testfiles_image_helper")
  
  # Ensure the directory exists
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
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
  
  # Clean up the directory
  unlink(target_dir, recursive = TRUE)
})


test_that("image_helper() does show the caption for plotly in HTML and can save SVGs. Also: extra_html_tags", {
  # Define the directory path
  target_dir <- file.path(tempdir(), "testfiles_image_helper")
  
  # Ensure the directory exists
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
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
  
  # Clean up the directory
  unlink(target_dir, recursive = TRUE)
})

test_that("image_helper() test plotly and csv_opt params", {
  # Define the directory path
  target_dir <- file.path(tempdir(), "testfiles_image_helper")
  
  # Ensure the directory exists
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
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
  
  # Clean up the directory
  unlink(target_dir, recursive = TRUE)
})

##############################
## image_helper_light tests ##
##############################

test_directory <- file.path(tempdir(), "testfiles_image_helper_light")
dir.create(test_directory, showWarnings = FALSE)

test_that("image_helper_light() requires only captions, all other parameters are optional", {
  helper_output <- image_helper_light(c("my_first_caption", "my_second_caption"))
  expected_output <- "<center><small>my_first_caption | my_second_caption</small> <br> </center> <br>"
  
  expect_identical(helper_output, expected_output)
})

test_that("image_helper_light() works for parameter data independently of parameter images", {
  withr::local_package("readr")
  
  filename <- "test_data"
  data <- tibble(
    fileformat = c("csv"),
    filename_suffix = c("")
  )
  captions = c("my_caption")
  
  datapath <- file.path(test_directory, paste0(filename, ".csv"))
  df <- tibble(col1=c(1,2), col2=c(3,4))
  readr::write_csv(df, datapath)
  
  expect_no_error(
    image_helper_light(
      filename=filename,
      filepath=test_directory,
      data=data,
      captions=captions
    )
  )
})

test_that("image_helper_light() works for parameter images independently of parameter data", {
  withr::local_package("ggplot2")
  
  filename <- "test_image"
  images <- tibble(
    fileformat = c("png"),
    filename_suffix = c("")
  )
  captions = c("my_caption")
  
  pngpath <- file.path(test_directory, paste0(filename, ".png"))
  tmp_ggplot <-
    ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
    geom_line() +
    geom_point() +
    labs(title = "Beispiel-Plot im SMC-Theme - Schriftart: txyz") +
    SMC_theme_ggplot()
  
  ggsave(plot = tmp_ggplot,
         filename = pngpath,
         device = "png")
  
  expect_no_error(
    image_helper_light(
      filename=filename,
      filepath=test_directory,
      images=images,
      captions=captions
    )
  )
})

test_that("image_helper_light() throws error if parameters data or images are passed without filepath and filename", {
  data <- tibble(
    fileformat = c("csv"),
    filename_suffix = c("")
  )
  captions = c("my_caption")
  
  expect_error(
    image_helper_light(
      data=data,
      captions=captions
    ),
    "Parameters filename and filepath are required if data parameter is passed to image_helper_light()"
  )
})

test_that("image_helper_light() throws error if data' or images' column names differ from 'fileformat' and 'filename_suffix'", {
  filename <- "test_file"
  data <- tibble(
    another_name = c("csv"),
    filename_suffix = c("")
  )
  captions = c("my_caption")
  
  expect_error(
    image_helper_light(
      filename=filename,
      filepath=test_directory,
      data=data,
      captions=captions
    ),
    "Parameter data should have two columns only. Their names should be: 'fileformat', and 'filename_suffix'"
  )
})

test_that("image_helper_light() throws error if data or images have more than two columns", {
  filename <- "test_file"
  data <- tibble(
    fileformat = c("csv"),
    filename_suffix = c(""),
    third_col = c("bla")
  )
  captions = c("my_caption")
  
  expect_error(
    image_helper_light(
      filename=filename,
      filepath=test_directory,
      data=data,
      captions=captions
    ),
    "Parameter data should have two columns only. Their names should be: 'fileformat', and 'filename_suffix'"
  )
})

test_that("image_helper_light() throws error if data or image file path does not exist", {
  filename <- "test_file_2"
  data <- tibble(
    fileformat = c("csv"),
    filename_suffix = c("")
  )
  captions = c("my_caption")
  
  expect_error(
    image_helper_light(
      filename=filename,
      filepath=test_directory,
      data=data,
      captions=captions
    ),
    "Data files missing: check if data files exist or correct parameter data in image_helper_light()"
  )
})

unlink(test_directory, recursive = TRUE)
