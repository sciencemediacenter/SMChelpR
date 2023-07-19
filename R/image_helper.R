#' image_helper.R
#'
#' Function to create generic image captions in dynamic documents.
#' For this purpose, HTML code is created, which contains a link to the data needed to create this figure. 
#' Also includes links to download the figure as a PNG or SVG. 
#' 
#' The image_helper() function returns a list containing the file path, the title including links, 
#' an "alt" text (consisting of the image name) and possibly other HTML tags.
#' 
#' @param plot ggplot-object: The ggplot-object to be used for static images.
#' @param filename string: The name of the image file, without file extension.
#' @param filepath file.path: The path to the folder in which the image file should be saved. CSV must be placed manually, PNG and SVG are stored automatically.
#' @param extra_html_tags string: Between \'\' further HTML-Params can be added, e.g. \'width = \"100%\" data-zoom=\"1\"\'
#' @param csv_opt boolean, FALSE: no link to a CSV-Download will be provided; default: TRUE.
#' @param plotly boolean: FALSE, a PNG of the provided ggplot-object will be displayed. TRUE: no figure is shown, plotly-figures must be displayed manually bforehand. Default: FALSE
#' @param caption string: If not empty: text for the caption, e.g. sources.
#' @param save_svg boolean: Generation of the corresponding svg. Default: TRUE. For very detailed graphics, you should refrain from creating a sv-graphic, since the file size can become too large.
#' @return none
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tmp_ggplot <- ggplot(data = mpg, aes(x = displ, y = hwy))
#' tmp_ggplot %>% image_helper("MPG", file.path(tempdir()))
#' @export image_helper


image_helper <-
  function(plot,
           filename,
           filepath,
           extra_html_tags = 'width = "100%" data-zoom="1"',
           csv_opt = TRUE,
           plotly = FALSE,
           caption = "",
           save_svg = TRUE
          ) {
    pngpfad <- file.path(filepath, paste0(filename, ".png"))
    svgpfad <- file.path(filepath, paste0(filename, ".svg"))
    datenpfad <- file.path(filepath, paste0(filename, ".csv"))
    
    plot <- plot + labs(caption = caption)
    ggsave(plot = plot,
           filename = pngpfad,
           device = "png")
    if (save_svg == TRUE) {
      ggsave(plot = plot,
             filename = svgpfad,
             device = "svg")
    } 
    
    # Create HTML code
    HTML_text <- "<center>"

    if (plotly == FALSE) {
      HTML_text <- HTML_text |>
        paste0(
          '<img src="',
          pngpfad,
          '" alt="',
          toString(filename),
          '" ',
          extra_html_tags,
          " />",
          sep = ""
        )
    }

    if(caption != ""){
      HTML_text <- HTML_text |>
        paste0(
          "<small>",
          caption,
          "</small> <br>"
        )
    }

    if (csv_opt == TRUE) {
      HTML_text <- HTML_text |>
        paste0(
          "[Die Daten zur Erstellung dieser Abbildung herunterladen.](",
          datenpfad,
          ") <br>",
          sep = ""
        )
    }
    HTML_text <- HTML_text |>
      paste0(
        'Diese Abbildung herunterladen: <a href ="',
        pngpfad,
        '" download>Als PNG.</a>',
        sep = ""
      ) 

    if (save_svg == TRUE){
      HTML_text <- HTML_text |>
        paste0(
          ' <a href ="',
          svgpfad,
          '" download>Als SVG.</a>',
          sep = ""
        )
    }
    HTML_text <- HTML_text |>
      paste0("</center> <br>")
      
    cat(HTML_text)
    invisible(HTML_text)
  }
