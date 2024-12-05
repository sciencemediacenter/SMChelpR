#' SMC-Theme for ggplot2
#' 
#' Core functionality is SMC_theme_ggplot(). It sets all theme parameters for ggplot.
#' It must be called at the beginning of each R-/Quarto-/R-Markdown document.


#' SMC_theme_ggplot
#' 
#' Primary function for the creation of Quarto-Documents in the SMC-Theme: SMC_theme_ggplot().
#' It must be called at the beginning of each R-/Quarto-/R-Markdown document.
#' 
#' Theme inherits from theme_minimal() and then customizes the following parameters:
#' font family and sizes, background color, grid color, legend and caption position,
#' margin size (padding), line width and point size.
#' 
#' @param ... Additional parameters to be passed to theme_update().
#' @param theme_params list, list of the customized parameters that can be obtained via [get_SMC_ggplotly_default_parameters()].
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
    family = "PlusJakartaSans", 
    regular = fs::path_package("fonts", "Plus_Jakarta_Sans", "static", "PlusJakartaSans-Regular.ttf", package = "SMChelpR"),
    italic = fs::path_package("fonts", "Plus_Jakarta_Sans", "static", "PlusJakartaSans-Italic.ttf", package = "SMChelpR"),
    bold = fs::path_package("fonts", "Plus_Jakarta_Sans", "static", "PlusJakartaSans-Bold.ttf", package = "SMChelpR"),
    bolditalic = fs::path_package("fonts", "Plus_Jakarta_Sans", "static", "PlusJakartaSans-BoldItalic.ttf", package = "SMChelpR")
  )
  # https://www.christophenicault.com/post/understand_size_dimension_ggplot2/
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto(enable = TRUE)
  
  theme_set(theme_minimal()) 

  old_theme <- theme_update(
    # allow for passing arguments to theme_update
    ...,
    
    # set the font family
    text = element_text(family = "PlusJakartaSans"),
    
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
    # Note: Margins have no effect on the ggplotly-figure and thus the html-output, but on the png and svg
    plot.title = element_text(
      margin = margin(
        t = get_param(theme_params, "margin_SMC", 8),
        b = get_param(theme_params, "margin_SMC", 8)
      ),
      hjust = get_param(theme_params, "title_hjust", 0.5),
      size = get_param(theme_params, "title_size", size_in_pt(size_in_px = 18)),
    ),
    
    plot.caption = element_text(
      margin = margin(
        t = get_param(theme_params, "margin_SMC", 8),
        b = get_param(theme_params, "margin_SMC", 8)
      ),
      hjust = get_param(theme_params, "caption_hjust", 0.5)
    ),
    
    axis.title = element_text(
      size = get_param(theme_params, "axis_title_size", size_in_pt(size_in_px = 14)),
    ),
    axis.text = element_text(
      size = get_param(theme_params, "axis_text_size", size_in_pt(size_in_px = 12)),
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
      size = get_param(theme_params, "legend_title_size", size_in_pt(size_in_px = 14)),
    ),
    legend.text = element_text(
      size = get_param(theme_params, "legend_text_size", size_in_pt(size_in_px = 12)),
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

#' get_SMC_theme_ggplot_default_parameters
#' 
#' Function that returns the default parameters that are customized in SMC_theme_ggplot().
#' This list can be modified and passed to SMC_theme_ggplot() via the theme_params argument.
#' 
#' Note: ggplot uses pt as units for font sizes, linewidths and point sizes, whereas plotly uses px.
#' To account for this, the function size_in_pt() is used to convert from px to pt. 
#' However, a complete match is difficult to achieve due to the different fonts being used (here: PlusJakartaSans, plotly: embeds Circular from the SMC-Website).
#' 
#' @return List of default parameters for SMC_theme_ggplot().
#' @examples
#' get_SMC_theme_ggplot_default_parameters()
#' @export get_SMC_theme_ggplot_default_parameters
get_SMC_theme_ggplot_default_parameters <- function() {
  list(
    # title (centered)
    title_hjust = 0.5,
    title_size = size_in_pt(size_in_px = 18),

    # background
    background_fill = "white",
    background_colour = "transparent",
    
    # grid
    grid_linewidth = 0.25,
    grid_colour = "grey92",
    
    # axis 
    axis_title_size = size_in_pt(size_in_px = 14),
    axis_text_size = size_in_pt(size_in_px = 12),
    
    # legend
    legend_position = "bottom",
    legend_title_size = size_in_pt(size_in_px = 14),
    legend_text_size = size_in_pt(size_in_px = 12),
    
    # caption (centered)
    caption_hjust = 0.5,
    
    # misc
    margin_SMC = 8,
    linewidth_in_pt = 0.9,
    pointsize_in_pt = 1.8
    
  )
}