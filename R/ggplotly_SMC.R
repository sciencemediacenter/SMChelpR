#' ggplotly_SMC
#' A wrapper function for ggplotly that adjusts the theme and some parameters, 
#' in particular font size, spacing, legend positioning, etc.
#' 
#' @param ... A ggplot object which is the basis for a plotly object. Also: all parameters that you would usually pass to ggplotly, such as: ... = figure, tooltip = c("x", "y")
#' @param legende_unten boolean, FALSE: Legend to the right of the plot; TRUE: Legend below the plot
#' @param mehrzeiliger_titel boolean, TRUE: adjusted spacing between title and plot, if the title spans several lines
#' @return plotly-object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tmp_ggplot <- ggplot(data = mpg, aes(x = displ, y = hwy))
#' tmp_ggplotly <- tmp_ggplot %>% ggplotly_SMC()
#' tmp_ggplotly
#' tmp_ggplot %>% image_helper("MPG", file.path(tempdir()), plotly = TRUE)
#' @export ggplotly_SMC
ggplotly_SMC <- function(
    ..., legende_unten = TRUE, mehrzeiliger_titel = FALSE, interactiv = FALSE, ggplotly_params = list()
){
  ggplot_abbildung <- list(...)[[1]] # extract the ggplot object from the list
  
  ggplot_abbildung_built <- plotly_build(ggplot_abbildung)
  tmp_margins <- ggplot_abbildung_built$x$layout$margin
  
  abbildung <- ggplotly(...) |>
    layout(
      title = list(
        font = list(
          family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
          size = get_param(ggplotly_params, "title_size", 20)
        ),
        x = get_param(ggplotly_params, "title_hjust", 0.5)
      ),
      xaxis = list(
        title = list(
          font = list(
            family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
            size = get_param(ggplotly_params, "xaxis_title_size", 19)
          )
        ),
        tickfont = list(
          family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
          size = get_param(ggplotly_params, "xaxis_tickfont_size", 16)
        )
      ),
      yaxis = list(
        title = list(
          font = list(
            family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
            size = get_param(ggplotly_params, "yaxis_title_size", 19)
          ),
          standoff = get_param(ggplotly_params, "yaxis_standoff", 10)
        ),
        tickfont = list(
          family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
          size = get_param(ggplotly_params, "yaxis_tickfont_size", 16)
        )
      ),
      legend = list(
        title = list(
          font = list(
            family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
            size = get_param(ggplotly_params, "legend_title_size", 20)
          )
        ),
        font = list(
          family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
          size = get_param(ggplotly_params, "legend_font_size", 16)
        )
      ),
      margin = list(
        b = get_param(ggplotly_params, "margin_bottom", tmp_margins$b),
        t = get_param(ggplotly_params, "margin_top", tmp_margins$t),
        l = get_param(ggplotly_params, "margin_left", tmp_margins$l),
        r = get_param(ggplotly_params, "margin_right", tmp_margins$r),
        pad = get_param(ggplotly_params, "margin_pad", 0)
      )
    )
  
  if (legende_unten == TRUE) {
    abbildung <- abbildung |>
      layout(
        legend = list(
          orientation = "h",
          entrywidth = get_param(ggplotly_params, "legend_entrywidth", 70),
          yanchor = "bottom",
          y = get_param(ggplotly_params, "legend_y", -0.28),
          xanchor = "center",
          x = get_param(ggplotly_params, "legend_x", 0.5)
        )
      )
  }
  
  if (mehrzeiliger_titel == TRUE) {
    abbildung <- abbildung |>
      layout(
        margin = list(
          t = get_param(ggplotly_params, "margin_top_mehrzeiliger_titel", -8)
        )
      )
  }
  
  abbildung <- config(
    abbildung,
    modeBarButtonsToRemove =
      get_param(
        ggplotly_params,
        "mode_buttons_to_remove",
        list(
          "zoom2d",
          "pan2d",
          "select2d",
          "lasso2d",
          "zoomIn2d",
          "zoomOut2d",
          "autoScale2d",
          "resetViews",
          "toggleHover",
          "toImage",
          "sendDataToCloud",
          "toggleSpikelines"
        )
      )
  )
  
  abbildung <- abbildung |>
    layout(
      font = list(family = "CircularSMCWeb")
    )
  
  if (interactiv == TRUE) {
    # if you want to see the plot with correct fonts in interactive session
    # Note: set to FALSE when rendering Quarto document
    # https://github.com/quarto-dev/quarto-cli/issues/10339
    # quarto process currently only allows disk-based assets
    
    abbildung$dependencies <- c(
      abbildung$dependencies,
      list(
        htmltools::htmlDependency(
          name = "circular-font",
          version = "1.0",
          src = c(href = "https://media.sciencemediacenter.de/static/fonts/circular"),
          stylesheet = "web.css"
        )
      )
    )
  }
  
  abbildung
}


# plotly uses px as units
#' @export get_SMC_ggplotly_default_parameters
get_SMC_ggplotly_default_parameters <- function() {
  list(
    # title (centered)

    title_hjust = 0.5,
    title_size = 20,
    
    # axis 
    xaxis_title_size = 16,
    xaxis_tickfont_size = 14,
    yaxis_title_size = 16,
    yaxis_tickfont_size = 14,
    yaxis_standoff = 0,

    # legend (centered)
    legend_x = 0.5,
    legend_title_size = 16,
    legend_font_size = 16,
    legend_entrywidth = 30,

    # if x-axis label given
    legend_y = -0.22, # x-axis label given
    # if no x-axis label given 
    # legend_y = -0.18,

    
    # caption (centered)
    caption_hjust = 0.5,
    
    # margin
    margin_bottom = 20,
    margin_top = 0,
    margin_right = 0,
    margin_left = 0,
    margin_pad = 0,
    margin_top_mehrzeiliger_titel = -8,

    # font
    font_family = "CircularSMCWeb",

    # misc
    margin_SMC = 8,
    linewidth_in_pt = 0.9,
    pointsize_in_pt = 1.8,
    mode_buttons_to_remove = list(
      "zoom2d",
      "pan2d",
      "select2d",
      "lasso2d",
      "zoomIn2d",
      "zoomOut2d",
      "autoScale2d",
      "resetViews",
      "toggleHover",
      "toImage",
      "sendDataToCloud",
      "toggleSpikelines"
    )
  )
}