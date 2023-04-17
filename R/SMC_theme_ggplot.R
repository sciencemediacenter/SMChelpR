#' SMC-Theme für ggplot und ggplotly
#' 
#' Die Kernfunktion ist SMC_theme_ggplot(); mit dieser werden alle Theming-Parameter 
#' für ggplot gesetzt. Sie muss zu Beginn eines jeden R-/Quarto-/R-Markdown-Dokuments aufgerufen werden.
#' Beim initialen Aufrufen im Dokument muss ausgewählt werden, ob der Fokus des Dokuments auf Plotly oder ggplot liegt.
#' ggplot_SMC_theme(use_plotly = TRUE/FALSE)
#' 
#' Abhängig davon, ob der Schwerpunkt einer Aussendung auf ggplot-PNGs oder 
#' mit ggplotly erstellten interaktiven Abbildungen liegen soll, muss eine 
#' der folgenden Einstellungen im (dynamischen) Dokument genutzt werden: 
#' Plotly-Dokumente nutzen bitte: fig-width 9, fig-height 8, fig-dpi 200
#' ggplot-Dokumente nutzen bitte: fig-width 9, fig-height 7.25, fig-dpi 300



# Die vom SMC verwendete Farbpalette (in verschiedenen Formaten)
colorlist_SMC <- list(
  blue = "#377eb8",
  green = "#4daf4a",
  purple = "#984ea3",
  orange = "#ff7f00",
  brown = "#a65628",
  pink = "#f781bf",
  grey = "#999999",
  red = "#e41a1c",
  yellow = "#ffff33"
)

# als unnamed Vektor
colorvector_SMC <- unname(unlist(colorlist_SMC))



# einige globale Parameter
margin_SMC <- 8
SMC_fontsize = 17
SMC_linesize = 0.9


#' ggplot_SMC_theme
#' @param use_plotly boolean, TRUE: Aussendung ist auf plotly ausgelegt; FALSE: Aussendung ist auf ggplot-PNGs augelegt
#' @return vector colorlist_SMC: alle SMC-Farben als Referenz
#' @export SMC_theme_ggplot
SMC_theme_ggplot <- function(use_plotly = FALSE){

  theme_set(theme_minimal()) 

  theme_update(
    plot.background = element_rect(fill = "white", colour = "transparent"),

    # Grid im Hintergrund des Plots, nur Hauptlinien, Nebenlinien ausblenden
    panel.grid.major = element_line(size = 0.25),
    panel.grid.minor = element_blank(),

    # Allgemeines styling
    legend.position = "bottom",
    text = element_text(size = SMC_fontsize, family = "Aller"),
    
    # Margins und Positionierung von Elementen
    plot.title = element_text(margin = margin(t = margin_SMC, b = margin_SMC)),
    plot.caption = element_text(hjust = 0.5, margin = margin(t = margin_SMC, b = margin_SMC)), 
    axis.text.y = element_text(margin = margin(r = margin_SMC, l = margin_SMC)),
    axis.text.x = element_text(margin = margin(t = margin_SMC, b = margin_SMC)),
    legend.margin = margin(t = margin_SMC, b = margin_SMC, l = margin_SMC, r =margin_SMC),
  )

  # setze SMC-Farbpalette für durchgehende Nutzung in linegraphs
  options(ggplot2.discrete.colour = colorvector_SMC)

  # Passe Liniendicke und Punktgröße an das jeweilige Format an
  update_geom_defaults("line", list(size = SMC_linesize, color = colorvector_SMC[1]))
  update_geom_defaults("point", list(size = 2 * SMC_linesize))
  
  # Um Bugs beim Speichern von PDFs mit Grafiken zu vermeiden 
  # müssen die Schriftarten geladen werden
  font_add(
    family = "Aller", 
    regular = fs::path_package("fonts", "Aller", "Aller_Rg.ttf", package = "SMCr"),
    italic = fs::path_package("fonts", "Aller", "Aller_It.ttf", package = "SMCr"),
    bold = fs::path_package("fonts", "Aller", "Aller_Bd.ttf", package = "SMCr"),
    bolditalic = fs::path_package("fonts", "Aller", "Aller_BdIt.ttf", package = "SMCr")
  )
  showtext_opts(dpi = 300)
  showtext::showtext_auto()
  
  # Farbpalette ausgeben, damit diese in Dokumenten genutzt werden kann
  colorvector_SMC
}



#' colors_SMC
#' Mit dieser Funktion kann man sich eine einzelne oder mehrer Farben aus der SMC-Palette
#' in beliebiger Reihenfolge als Vektor oder String ausgeben lassen. 
#' @param ColorNames Ein String mit einem einzelenn Farbnamen oder ein Vektor mit mehreren Farbnamen.
#' @param rev  boolean, if TRUE: umgekehrte Reihenfolge der Farben ausgeben
#' @return Ein String mit einem einzelenn Farbcode oder ein Vektor mit mehreren unnamed Farbcodes. 
#' @export colors_SMC

colors_SMC <- function(ColorNames = NULL, rev = FALSE){
  # Vektor mit allen Farbcodes erstellen
  ColorValues <- unlist(colorlist_SMC)

  # Falls gegeben: Einschränkung auf gewünschte Farben
  if (!is.null(ColorNames)) {
    ColorValues <- ColorValues[ColorNames]
  }

  # Umdrehen der Reihenfolge
  if (rev) {
    ColorValues <- rev(ColorValues)
  }

  # Ausgabe 
  return(unname(ColorValues))
}