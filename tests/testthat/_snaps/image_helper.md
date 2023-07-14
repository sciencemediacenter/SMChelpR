# image_helper() can save a PNG and save_svg = FALSE works

    Code
      cat(image_helper(plot = tmp_ggplot, filename = "testpng", filepath = "testfiles_image_helper",
        save_svg = FALSE))
    Message <rlang_message>
      Saving 7 x 7 in image
    Output
      <center><img src="testfiles_image_helper/testpng.png" alt="testpng" width = "100%" data-zoom="1" />[Die Daten zur Erstellung dieser Abbildung herunterladen.](testfiles_image_helper/testpng.csv) <br>Diese Abbildung herunterladen: <a href ="testfiles_image_helper/testpng.png" download>Als PNG.</a></center> <br>

# image_helper() does show the caption and can save SVGs

    Code
      cat(image_helper(plot = tmp_ggplot, filename = "testfile", filepath = "testfiles_image_helper",
        caption = "Dies ist ein Test"))
    Message <rlang_message>
      Saving 7 x 7 in image
      Saving 7 x 7 in image
    Output
      <center><img src="testfiles_image_helper/testfile.png" alt="testfile" width = "100%" data-zoom="1" /><small> Dies ist ein Test </small> <br>[Die Daten zur Erstellung dieser Abbildung herunterladen.](testfiles_image_helper/testfile.csv) <br>Diese Abbildung herunterladen: <a href ="testfiles_image_helper/testfile.png" download>Als PNG.</a> <a href ="testfiles_image_helper/testfile.svg" download>Als SVG.</a></center> <br>

# image_helper() test plotly, csv_opt and extra_html_tags params

    Code
      cat(image_helper(plot = tmp_ggplot, filename = "testfile", filepath = "testfiles_image_helper",
        caption = "Dies ist ein Test", plotly = FALSE, csv_opt = FALSE,
        extra_html_tags = "width = \"50%\""))
    Message <rlang_message>
      Saving 7 x 7 in image
      Saving 7 x 7 in image
    Output
      <center><img src="testfiles_image_helper/testfile.png" alt="testfile" width = "50%" /><small> Dies ist ein Test </small> <br>Diese Abbildung herunterladen: <a href ="testfiles_image_helper/testfile.png" download>Als PNG.</a> <a href ="testfiles_image_helper/testfile.svg" download>Als SVG.</a></center> <br>

