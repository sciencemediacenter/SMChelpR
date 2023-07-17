#' image_helper.R
#'
#' Funktion, um generische Bildunterschriften in dynamischen Dokumenten für Abbildungen zu erstellen.
#' Hierfür wird HTML-Code erstellt, der einen Link auf die zur Erstellung dieser Abbildung benötigten Daten 
#' sowie Links zu der Abbildung als PNG und SVG enthält.
#' 
#' Die Funktion image_helper() gibt eine Liste zurück, die den Dateipfad, den Titel inklusive Verlinkungen, 
#' einen "alt" Text (bestehend aus dem Bildnamen) und ggf. weitere HTML-Tags enthält.
#' 
#' @param plot Das ggplot-Objekt was für die statischen Bilder genutzt werden soll.
#' @param filename string: Dateiname für die verlinkten Dateien (ohne Endungen).
#' @param filepath file.path: Dateipfad für die verlinkten Dateien. CSV muss manuell dort abgelegt werden, PNG und SVG werden von dieser Funktion dort abgelegt.
#' @param extra_html_tags string: Zwischen \'\' können weitere HTML-Parameter angegeben werden, etwa: \'width = \"100%\" data-zoom=\"1\"\'
#' @param csv_opt boolean, FALSE: keine csv wird verlinkt; default: TRUE.
#' @param plotly boolean: FALSE, das oben übergegebene ggplot-Objekt wird angezeigt. TRUE: keine Abbildung wird angezeigt, plotly-Abbildungen müssen manuell vorher ausgegeben werden. Default: FALSE
#' @param caption string: Falls nicht leer: zusätzlicher Bild-Text, der unterhalb der Abbildung angezeigt werden kann. Etwa Datenquellen für Plotly-Abbildungen.
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
    
    # Konstruktion der HTML-Ausgabe
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
