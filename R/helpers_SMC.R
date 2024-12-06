#' get_param
#' 
#' Helper that allows to get a parameter from a list of parameters or a default value if the parameter is not present.
#' 
#' @param plot_parameters list, list of parameters.
#' @param param_name character, name of the parameter to be retrieved.
#' @param default_value any, default value to be returned if the parameter is not present.
#' @examples
#' get_param(list(margin_SMC = 10), "margin_SMC", 8)
#' @export get_param
get_param <- function(plot_parameters, param_name, default_value) {
    if (param_name %in% names(plot_parameters)) {
        return(plot_parameters[[param_name]])
    } else {
        return(default_value)
    }
}

#' size_in_pt
#' 
#' Conversion between units of font sizes, linewidths and point sizes in ggplot2 and plotly.
#' 
#' ggplot uses pt as units for font sizes, linewidths and point sizes, whereas plotly uses px.
#' 
#' @param size_in_px numeric, size in pixels.
#' @param dpi numeric, dots per inch.
#' @examples
#' size_in_pt(12, dpi = 96)
#' @export size_in_pt
size_in_pt <- function(size_in_px, dpi = 96) {
  # Check if dpi is 0 to avoid division by zero error
  if (dpi == 0) {
    stop("DPI cannot be zero. Please provide a valid DPI value.")
  }
  # Return the converted size in pt
  return(size_in_px * 72 / dpi)
}
