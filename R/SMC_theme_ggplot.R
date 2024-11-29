#' SMC-Theme for ggplot2 and plotly 
#' 
#' Core functionality is SMC_theme_ggplot(). It sets all theme parameters for ggplot.
#' It must be called at the beginning of each R-/Quarto-/R-Markdown document.
#' 

# several global parameters for the SMC theme


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
SMC_theme_ggplot <- function(
  ..., theme_params = list()
){
  # to prevent bugs while saving PDFs 
  # the fonts need to be loaded in the following manner
  sysfonts::font_add(
    family = "CircularSMC", 
    regular = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-Regular.ttf", package = "SMChelpR"),
    italic = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-Italic.ttf", package = "SMChelpR"),
    bold = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-Bold.ttf", package = "SMChelpR"),
    bolditalic = fs::path_package("fonts", "CircularXX - Latin - Desktop Fonts", "TrueType TTF", "Fonts", "CircularXXTT-BoldItalic.ttf", package = "SMChelpR")
  )
  # https://www.christophenicault.com/post/understand_size_dimension_ggplot2/
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto(enable = TRUE)
  
  theme_set(theme_minimal()) 

  old_theme <- theme_update(
    # set the font
    text = element_text(family = "CircularSMC"),
    
    # white background
    plot.background = element_rect(
      fill = get_param(theme_params, "background_fill", "white"),
      colour = get_param(theme_params, "background_colour", "transparent")
    ),
    
    # Grid in the background of the plot, only main lines, hide minor lines
    panel.grid.major = element_line(
      linewidth = get_param(theme_params, "grid_linewidth", 0.25),
      colour = get_param(theme_params, "grid_colour", "grey92")
    ),
    panel.grid.minor = element_blank(),
    
    # general styling
    legend.position = get_param(theme_params, "legend_position", "bottom"),
    
    # Margins and positioning of elements
    # Note: Margins have no effect on the html, but on the png and svg
    plot.title = element_text(
      margin = margin(
        t = get_param(theme_params, "margin_SMC", 8),
        b = get_param(theme_params, "margin_SMC", 8)
      ),
      hjust = get_param(theme_params, "title_hjust", 0.5),
      size = get_param(theme_params, "title_size", size_in_pt(size_in_px = 20)),
    ),
    
    plot.caption = element_text(
      margin = margin(
        t = get_param(theme_params, "margin_SMC", 8),
        b = get_param(theme_params, "margin_SMC", 8)
      ),
      hjust = get_param(theme_params, "caption_hjust", 0.5)
    ),
    
    axis.title = element_text(
      size = get_param(theme_params, "axis_title_size", size_in_pt(size_in_px = 16)),
    ),
    axis.text = element_text(
      size = get_param(theme_params, "axis_text_size", size_in_pt(size_in_px = 14)),
    ),
    axis.text.x = element_text(
      margin = margin(
        t = get_param(theme_params, "margin_SMC", 8),
        b = get_param(theme_params, "margin_SMC", 8)
      ),
    ),
    axis.text.y = element_text(
      margin = margin(
        r = get_param(theme_params, "margin_SMC", 8),
        l = get_param(theme_params, "margin_SMC", 8),
      ),
    ),
      
    legend.margin = margin(
      t = get_param(theme_params, "margin_SMC", 8),
      b = get_param(theme_params, "margin_SMC", 8),
      l = get_param(theme_params, "margin_SMC", 8),
      r = get_param(theme_params, "margin_SMC", 8),
    ),
    legend.title = element_text(
      size = get_param(theme_params, "legend_title_size", size_in_pt(size_in_px = 16)),
    ),
    legend.text = element_text(
      size = get_param(theme_params, "legend_text_size", size_in_pt(size_in_px = 14)),
    ),
    
  )

  # use the SMC colors in all linegraphs
  options(ggplot2.discrete.colour = colors_SMC_unnamed)

  # set the default size of lines and points
  update_geom_defaults("line", list(
    linewidth = get_param(theme_params, "linewidth_in_pt", 0.9),
    color = colors_SMC_unnamed[1]
    ))
  update_geom_defaults("point", list(
    size = get_param(theme_params, "pointsize_in_pt", 1.8)
    ))

  invisible(theme_get())
}

# ggplot uses px as units
#' @export get_SMC_theme_ggplot_default_parameters
get_SMC_theme_ggplot_default_parameters <- function() {
  list(
    # title (centered)

    title_hjust = 0.5,
    title_size = size_in_pt(size_in_px = 20),

    # background
    background_fill = "white",
    background_colour = "transparent",
    
    # grid
    grid_linewidth = 0.25,
    grid_colour = "grey92",
    
    # axis 
    axis_title_size = size_in_pt(size_in_px = 16),
    axis_text_size = size_in_pt(size_in_px = 14),
    
    # legend
    legend_position = "bottom",
    legend_title_size = size_in_pt(size_in_px = 16),
    legend_text_size = size_in_pt(size_in_px = 14),
    
    # caption (centered)
    caption_hjust = 0.5,
    
    # misc
    margin_SMC = 8,
    linewidth_in_pt = 0.9,
    pointsize_in_pt = 1.8
    
  )
}