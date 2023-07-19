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
  ..., legende_unten = TRUE, mehrzeiliger_titel = FALSE
){
  # Create a plotly object from the passed ggplot object.
  # Apply all settings passed to the function, such as tooltip.
  abbildung <- ggplotly(...) %>%
    # adjust font size, spacing, etc.
    layout(

      title = list(font = list(size = 25)),
      xaxis = list(title = list(font = list(size = 19)),
                   tickfont = list(size = 16)),
      yaxis = list(title = list(font = list(size = 19)),
                   tickfont = list(size = 16)),
      legend = list(title = list(font = list(size = 20)), 
                    font = list(size = 16)),

      # Spacing between the image elements and the text
      margin = list(
        b = 20, 
        l = 60,
        pad = 10
      ),

      # The margins between the declaratory axis texts/tick text (e.g. 2019, 2020, ...) and the axis.
      yaxis = list(
        title = list(
          standoff = 10
        )
      )
      
    )

  # Position legend below the plot
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
  
  # If the title has two rows:
  # Increase the distance between title and plot
  if(mehrzeiliger_titel == TRUE){
    abbildung <- abbildung %>% 
      layout(
        margin = list(
          t = -8
        )
      )
  }

  # Remove unnecessary buttons from the navbar
  abbildung <- config(abbildung, modeBarButtonsToRemove = 
    list(
      "zoom2d", "pan2d", "select2d", "lasso2d",
      "zoomIn2d", "zoomOut2d", "autoScale2d",
      "toggleSpikelines", "resetViews", "toggleHover",
      "toImage", "sendDataToCloud", "toggleSpikelines"
    )
  )

  # return the plotly object
  abbildung
}
