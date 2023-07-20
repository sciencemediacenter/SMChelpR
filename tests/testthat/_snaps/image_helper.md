# image_helper() can save a PNG and save_svg = FALSE works

    Code
      cat(gsub(paste0(toString(tempdir()), "/"), "", helper_output, fixed = TRUE))
    Output
      <center><img src="testfiles_image_helper/testpng.png" alt="testpng" width = "100%" data-zoom="1" />[Die Daten zur Erstellung dieser Abbildung herunterladen.](testfiles_image_helper/testpng.csv) <br>Diese Abbildung herunterladen: <a href ="testfiles_image_helper/testpng.png" download>Als PNG.</a></center> <br>

# image_helper() does show the caption for plotly in HTML and can save SVGs. Also: extra_html_tags

    Code
      cat(gsub(paste0(toString(tempdir()), "/"), "", helper_output, fixed = TRUE))
    Output
      <center><img src="testfiles_image_helper/testfile.png" alt="testfile" width = "50%" />[Die Daten zur Erstellung dieser Abbildung herunterladen.](testfiles_image_helper/testfile.csv) <br>Diese Abbildung herunterladen: <a href ="testfiles_image_helper/testfile.png" download>Als PNG.</a> <a href ="testfiles_image_helper/testfile.svg" download>Als SVG.</a></center> <br>

# image_helper() test plotly and csv_opt params

    Code
      cat(gsub(paste0(toString(tempdir()), "/"), "", helper_output, fixed = TRUE))
    Output
      <center><small>Dies ist ein Test</small> <br>Diese Abbildung herunterladen: <a href ="testfiles_image_helper/testfile.png" download>Als PNG.</a> <a href ="testfiles_image_helper/testfile.svg" download>Als SVG.</a></center> <br>

