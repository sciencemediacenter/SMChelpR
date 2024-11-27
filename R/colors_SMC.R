#' Predefined color list for SMC
#'
#' A named list containing color codes.
#'
#' @export colors_SMC_named
colors_SMC_named <- list(
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

#' Predefined color list for SMC
#'
#' An unnamed list containing color codes.
#'
#' @export colors_SMC_unnamed
colors_SMC_unnamed <- unname(unlist(colors_SMC_named))

#' colors_SMC
#' This function returns one or more color codes from the SMC color palette
#' in arbitrary order as a vector or string (single color).
#' @param ColorNames A string with a single color code or a vector with several colors.
#' @param rev  boolean, if TRUE: reverse the return order of the colors.
#' @return An unnamed vector with color codes or a single color code as a string.
#' @export colors_SMC
colors_SMC <- function(ColorNames = NULL, rev = FALSE){
  # Create a vector containg all color codes from the SMC color palette
  Color_Values <- unlist(colors_SMC_named)

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
