#' ggplotly_SMC
#' 
#' A wrapper function for ggplotly that is applied onto a ggplot-figure and
#' adjusts the theme and some parameters, in particular:
#' font sizes, spacing, legend positioning, etc.
#' 
#' @details 
#' This function applies the "CircularSMCWeb" font-family, used on the [SMC Website](https://www.sciencemediacenter.de/), 
#' ensuring that data report figures render correctly on the site. 
#' For interactive sessions, you can preview figures with the website font by setting `interaktiv = TRUE`. 
#' However, due to Quarto's limitation with "disk-based" assets (see [GitHub Issue](https://github.com/quarto-dev/quarto-cli/issues/10339)), 
#' you must set `interaktiv = FALSE` before rendering.
#' 
#' @param ... A ggplot object which is the basis for a plotly object. Also: all parameters that you would usually pass to ggplotly, such as: ... = figure, tooltip = c("x", "y")
#' @param legende_unten boolean, FALSE: Legend to the right of the plot; TRUE: Legend below the plot
#' @param x_axis_label_gegeben boolean, TRUE: x-axis label is given; FALSE: no x-axis label
#' @param mehrzeiliger_titel boolean, TRUE: adjusted spacing between title and plot, if the title spans several lines
#' @param interaktiv boolean, TRUE: add font dependencies to interactive plot (needs to be FALSE when rendering Quarto document)
#' @param ggplotly_params list, list of parameters to adjust the plotly object
#' @return plotly-object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tmp_ggplot <- ggplot(data = mpg, aes(x = displ, y = hwy))
#' tmp_ggplotly <- tmp_ggplot |> ggplotly_SMC()
#' tmp_ggplotly
#' tmp_ggplot |> image_helper("MPG", file.path(tempdir()), plotly = TRUE)
#' @export ggplotly_SMC
ggplotly_SMC <- function(
    ..., legende_unten = TRUE, x_axis_label_gegeben = TRUE, mehrzeiliger_titel = FALSE, interaktiv = FALSE, ggplotly_params = list()
){
  ggplot_abbildung <- list(...)[[1]] # extract the ggplot object from the list
  
  # get the margins of the plotly figure (after conversion from ggplot-figure) 
  # and set these as default (can be overwritten by ggplotly_params later on)
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
            size = get_param(ggplotly_params, "xaxis_title_size", 16)
          )
        ),
        tickfont = list(
          family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
          size = get_param(ggplotly_params, "xaxis_tickfont_size", 14)
        )
      ),
      yaxis = list(
        title = list(
          font = list(
            family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
            size = get_param(ggplotly_params, "yaxis_title_size", 16)
          ),
          standoff = get_param(ggplotly_params, "yaxis_standoff", 10)
        ),
        tickfont = list(
          family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
          size = get_param(ggplotly_params, "yaxis_tickfont_size", 14)
        )
      ),
      legend = list(
        title = list(
          font = list(
            family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
            size = get_param(ggplotly_params, "legend_title_size", 16)
          )
        ),
        font = list(
          family = get_param(ggplotly_params, "font_family", "CircularSMCWeb"),
          size = get_param(ggplotly_params, "legend_font_size", 14)
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
  
  # if x-axis label is not given, there needs to be less whitespace between the legend and the plot
  if (x_axis_label_gegeben == TRUE) {
    legend_y <- get_param(ggplotly_params, "legend_y_with_x_axis_label", -0.28)
  } else {
    legend_y <- get_param(ggplotly_params, "legend_y_without_x_axis_label", -0.18)
  }

  if (mehrzeiliger_titel == TRUE) {
    abbildung <- abbildung |>
      layout(
        margin = list(
          t = get_param(ggplotly_params, "margin_top_mehrzeiliger_titel", -8)
        )
      )
    
    # further shift legend down, if title spans multiple lines
    legend_y <- legend_y - 0.1
  }
  
  if (legende_unten == TRUE) {
    
    abbildung <- abbildung |>
      layout(
        legend = list(
          orientation = "h",
          entrywidth = get_param(ggplotly_params, "legend_entrywidth", 70),
          yanchor = "bottom",
          y = legend_y,
          xanchor = "center",
          x = get_param(ggplotly_params, "legend_x", 0.5)
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
  
  if (interaktiv == TRUE) {
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


#' get_SMC_ggplotly_default_parameters
#' 
#' Function that returns the default parameters that are customized in ggplotly_SMC().
#' This list can be modified and passed to ggplotly_SMC() via the ggplotly_params argument.
#' 
#' Note: ggplot uses pt as units for font sizes, linewidths and point sizes, whereas plotly uses px.
#' To account for this, the function size_in_pt() is used to convert from px to pt. 
#' However, a complete match is difficult to achieve due to the different fonts being used (here: PlusJakartaSans, plotly: embeds Circular from the SMC-Website).
#' 
#' @return List of default parameters for ggplotly_SMC().
#' @examples
#' get_SMC_theme_ggplot_default_parameters()
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
    yaxis_standoff = 10,

    # legend (centered)
    legend_x = 0.5,
    legend_title_size = 16,
    legend_font_size = 14,
    legend_entrywidth = 70,

    # if x-axis label given
    legend_y_with_x_axis_label = -0.28,
    legend_y_without_x_axis_label = -0.18,
    
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