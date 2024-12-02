# Helper function to extract parameter values from the list with defaults
get_param <- function(plot_parameters, param_name, default_value) {
    if (param_name %in% names(plot_parameters)) {
        return(plot_parameters[[param_name]])
    } else {
        return(default_value)
    }
}

# dpi is set to 300 in the quarto, yet conversion with 96 yields a better fit with plotly figures
#' @export size_in_pt
size_in_pt <- function(size_in_px, dpi = 96) {
    return(size_in_px * 72 / dpi)
}