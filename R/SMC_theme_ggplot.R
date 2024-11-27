#' SMC-Theme for ggplot2 and plotly 
#' 
#' Core functionality is SMC_theme_ggplot(). It sets all theme parameters for ggplot.
#' It must be called at the beginning of each R-/Quarto-/R-Markdown document.
#' 

# several global parameters for the SMC theme
margin_SMC <- 8
SMC_fontsize = 17
SMC_linesize = 0.9


#' ggplot_SMC_theme
#' 
#' Primary function for the creation of Quarto-Documents in the SMC-Theme: SMC_theme_ggplot().
#' It must be called at the beginning of each R-/Quarto-/R-Markdown document.
#' Sets font family, font size, padding, etc. Also affects Plotly.
#' 
#' @return Invisibly returns the current theme.
#' @examples
#' SMC_theme_ggplot()
#' @export SMC_theme_ggplot
SMC_theme_ggplot <- function(){
  
  theme_set(theme_minimal()) 

  theme_update(
    plot.background = element_rect(fill = "white", colour = "transparent"),

    # Grid in the background of the plot, only main lines, hide minor lines
    panel.grid.major = element_line(size = 0.25),
    panel.grid.minor = element_blank(),

    # general styling
    legend.position = "bottom",
    text = element_text(size = SMC_fontsize, family = "Aller"),
    
    # Margins and positioning of elements
    plot.title = element_text(margin = margin(t = margin_SMC, b = margin_SMC)),
    plot.caption = element_text(hjust = 0.5, margin = margin(t = margin_SMC, b = margin_SMC)), 
    axis.text.y = element_text(margin = margin(r = margin_SMC, l = margin_SMC)),
    axis.text.x = element_text(margin = margin(t = margin_SMC, b = margin_SMC)),
    legend.margin = margin(t = margin_SMC, b = margin_SMC, l = margin_SMC, r =margin_SMC),
  )

  # use the SMC colors in all linegraphs
  options(ggplot2.discrete.colour = colorvector_SMC)

  # set the default size of lines and points
  update_geom_defaults("line", list(size = SMC_linesize, color = colorvector_SMC[1]))
  update_geom_defaults("point", list(size = 2 * SMC_linesize))
  
  # to prevent bugs while saving PDFs 
  # the fonts need to be loaded in the following manner
  sysfonts::font_add(
    family = "CircularSMCWeb", 
    regular = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-Regular.ttf", package = "SMChelpR"),
    italic = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-Italic.ttf", package = "SMChelpR"),
    bold = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-Bold.ttf", package = "SMChelpR"),
    bolditalic = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-BoldItalic.ttf", package = "SMChelpR")
  )
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto()
  invisible(theme_get())
}