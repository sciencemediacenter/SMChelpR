#' ggplotly_SMC
#' Eine Wrapperfunktion für ggplotly, die das Theming und einige Parameter anpasst, insbesondere
#' Schriftgröße, Abstände, Legendenpositionierung etc.
#' 
#' @param ... Ein ggplot-Objekt, aus dem eine interaktive plotly-Abbildung erstellt werden soll. Außerdem: alle Parameter, die man üblicherweise an ggplotly übergeben würde, etwa:  ... = abbildung, tooltip = c("x", "y")
#' @param legende_unten boolean, FALSE: Legende rechts vom Plot; TRUE: Legende unter dem Plot
#' @param mehrzeiliger_titel boolean, TRUE: angepasstes Spacing zwischen Titel und Plot, falls der Titel mehrere Zeilen einnimmt
#' @return plotly-object
#' @examples
#' tmp_ggplot <- ggplot(data = ggplot2::mpg, aes(x = displ, y = hwy))
#' tmp_ggplotly <- tmp_ggplot %>% ggplotly_SMC()
#' tmp_ggplotly
#' tmp_ggplot %>% image_helper("MPG", file.path(tempdir()), plotly = TRUE)
#' @export ggplotly_SMC
ggplotly_SMC <- function(
  ..., legende_unten = TRUE, mehrzeiliger_titel = FALSE
){
  # Aus übergebenem ggplot-Objekt ein plotly-Objekt erstellen.
  # Alle angegebenen Einstellungen, etwa für's Tooltip, anwenden.
  abbildung <- ggplotly(...) %>%
    # Schriftgröße, Abstände etc. einstellen
    layout(

      title = list(font = list(size = 25)),
      xaxis = list(title = list(font = list(size = 19)),
                   tickfont = list(size = 16)),
      yaxis = list(title = list(font = list(size = 19)),
                   tickfont = list(size = 16)),
      legend = list(title = list(font = list(size = 20)), 
                    font = list(size = 16)),


      # Abstand zwischen Text und Grafikelementen
      margin = list(
        b = 20, 
        l = 60,
        pad = 10
      ),

      # Der Abstand, zwischen dem Zähl-Text an Achsen (bspw. 2019, 2020, ...) und der Achse.
      yaxis = list(
        title = list(
          standoff = 10
        )
      )
      
      # Höhe der Abbildung
      # Standardmäßig für 9 Zoll breite und 8 Zoll hohe Bilder mit 200er dpi ausgelegt.
    )

  # Legende unter den Plot setzen
  if (legende_unten == TRUE){
    abbildung <- abbildung %>% 
    layout(
      legend = list(
        orientation = "h",
        entrywidth = 70,
        yanchor = "bottom",
        y = -0.28,
        xanchor = "center",
        x = 0.5
      )
    )
  }
  
  # Abstand zwischen Titel und Abbildung erhöhen
  # erforderlich, wenn der Titel zwei Zeilen hat
  if(mehrzeiliger_titel == TRUE){
    abbildung <- abbildung %>% 
      layout(
        margin = list(
          t = -8
        )
      )
  }

  # Nicht benötigte Buttons aus der Navbar entfernen
  abbildung <- config(abbildung, modeBarButtonsToRemove = 
    list(
      "zoom2d", "pan2d", "select2d", "lasso2d",
      "zoomIn2d", "zoomOut2d", "autoScale2d",
      "toggleSpikelines", "resetViews", "toggleHover",
      "toImage", "sendDataToCloud", "toggleSpikelines"
    )
  )

  # das fertige Plotly-Objekt ausgeben
  abbildung
}
