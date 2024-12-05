#' get_param
#' 
#' Helper that allows to get a parameter from a list of parameters or a default value if the parameter is not present.
#' 
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
#' @examples
#' size_in_pt(12, dpi = 96)
#' @export size_in_pt
size_in_pt <- function(size_in_px, dpi = 96) {
    return(size_in_px * 72 / dpi)
}