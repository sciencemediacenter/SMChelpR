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
#' @param extra_html_tags string: Zwischen \'\' können weitere HTML-Parameter angegeben werden, etwa: \'width = \"100\%\" data-zoom=\"1\"\'
#' @param csv_opt boolean, FALSE: keine csv wird verlinkt; default: TRUE.
#' @param plotly boolean: FALSE, das oben übergegebene ggplot-Objekt wird angezeigt. TRUE: keine Abbildung wird angezeigt, plotly-Abbildungen müssen manuell vorher ausgegeben werden. Default: FALSE
#' @param caption string: Falls nicht leer: zusätzlicher Bild-Text, der unterhalb der Abbildung angezeigt werden kann. Etwa Datenquellen für Plotly-Abbildungen. 
#' @return none
#' @examples
#' \dontrun{
#' ```{r, results = 'asis'}
#' tmp_ggplot %>% image_helper("Inzidenzen", file.path("data"))
#' ```
#' }
#' @export image_helper


image_helper <-
  function(plot,
           filename,
           filepath,
           extra_html_tags = 'width = "100%" data-zoom="1"',
           csv_opt = TRUE,
           plotly = FALSE,
           caption = "") {
    pngpfad <- file.path(filepath, paste0(filename, ".png"))
    svgpfad <- file.path(filepath, paste0(filename, ".svg"))
    datenpfad <- file.path(filepath, paste0(filename, ".csv"))
    
    ggsave(plot = plot,
           filename = pngpfad,
           device = "png")
    ggsave(plot = plot,
           filename = svgpfad,
           device = "svg")
    
    cat("<center>")
    if (plotly == FALSE) {
      cat(
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
      cat(
        "<small>",
        caption,
        "</small> <br>"
      )
    }
    if (csv_opt == TRUE) {
      cat(
        "[Die Daten zur Erstellung dieser Abbildung herunterladen.](",
        datenpfad,
        ") <br>",
        sep = ""
      )
    }
    cat(
      'Diese Abbildung herunterladen: <a href ="',
      pngpfad,
      '" download>Als PNG.</a> <a href ="',
      svgpfad,
      '" download>Als SVG.</a>',
      sep = ""
    )
    cat("</center> <br>")
  }
