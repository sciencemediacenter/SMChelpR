#' SMC-Theme for ggplot2 and plotly 
#' 
#' Core functionality is SMC_theme_ggplot(). It sets all theme parameters for ggplot.
#' It must be called at the beginning of each R-/Quarto-/R-Markdown document.
#' 

# The SMCs color palette is defined in colorlist_SMC.
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

# as an unnamed vector
colorvector_SMC <- unname(unlist(colorlist_SMC))



# several global parameters for the SMC theme
# margin_SMC <- 8
SMC_fontsize = 16
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
    plot.caption = element_text(hjust = 0.5),
    # plot.title = element_text(margin = margin(t = margin_SMC, b = margin_SMC)),
    # plot.caption = element_text(hjust = 0.5, margin = margin(t = margin_SMC, b = margin_SMC)), 
    # axis.text.y = element_text(margin = margin(r = margin_SMC, l = margin_SMC)),
    # axis.text.x = element_text(margin = margin(t = margin_SMC, b = margin_SMC)),
    # legend.margin = margin(t = margin_SMC, b = margin_SMC, l = margin_SMC, r =margin_SMC),
  )

  # use the SMC colors in all linegraphs
  options(ggplot2.discrete.colour = colorvector_SMC)

  # set the default size of lines and points
  update_geom_defaults("line", list(size = SMC_linesize, color = colorvector_SMC[1]))
  update_geom_defaults("point", list(size = 2 * SMC_linesize))
  
  # to prevent bugs while saving PDFs 
  # the fonts need to be loaded in the following manner
  sysfonts::font_add(
    family = "Aller", 
    regular = fs::path_package("fonts", "Aller", "Aller_Rg.ttf", package = "SMChelpR"),
    italic = fs::path_package("fonts", "Aller", "Aller_It.ttf", package = "SMChelpR"),
    bold = fs::path_package("fonts", "Aller", "Aller_Bd.ttf", package = "SMChelpR"),
    bolditalic = fs::path_package("fonts", "Aller", "Aller_BdIt.ttf", package = "SMChelpR")
  )
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto()
  invisible(theme_get())
}



#' colors_SMC
#' This function returns one or more color codes from the SMC color palette
#' in arbitrary order as a vector or string (single color).
#' @param ColorNames A string with a single color code or a vector with several colors.
#' @param rev  boolean, if TRUE: reverse the return order of the colors.
#' @return An unnamed vector with color codes or a single color code as a string.
#' @export colors_SMC

colors_SMC <- function(ColorNames = NULL, rev = FALSE){
  # Create a vector containg all color codes from the SMC color palette
  ColorValues <- unlist(colorlist_SMC)

  # If a single color or a subset of colors is requested, the vector is shortened
  # Can be reduced to a single string for a single color.
  if (!is.null(ColorNames)) {
    ColorValues <- ColorValues[ColorNames]
  }

  # If requested, the order of the colors is reversed
  if (rev) {
    ColorValues <- rev(ColorValues)
  }

  # Return the color codes as a vector or a single string 
  return(unname(ColorValues))
}
