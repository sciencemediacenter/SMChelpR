% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SMC_theme_ggplot.R
\name{SMC_theme_ggplot}
\alias{SMC_theme_ggplot}
\title{SMC-Theme for ggplot2}
\usage{
SMC_theme_ggplot(..., theme_params = list())
}
\arguments{
\item{...}{Additional parameters to be passed to theme_update().}

\item{theme_params}{list, list of the customized parameters that can be obtained via \code{\link[=get_SMC_ggplotly_default_parameters]{get_SMC_ggplotly_default_parameters()}}.}
}
\value{
Invisibly returns the current theme.
}
\description{
Core functionality is SMC_theme_ggplot(). It sets all theme parameters for ggplot.
It must be called at the beginning of each R-/Quarto-/R-Markdown document.
SMC_theme_ggplot
}
\details{
Primary function for the creation of Quarto-Documents in the SMC-Theme: SMC_theme_ggplot().
It must be called at the beginning of each R-/Quarto-/R-Markdown document.

Theme inherits from theme_minimal() and then customizes the following parameters:
font family and sizes, background color, grid color, legend and caption position,
margin size (padding), line width and point size.

To generate interactive graphics for the SMC website, we first set the SMC_theme_ggplot(),
then produce a ggplot-figure with it and finally convert to plotly via ggplotly_SMC().
However, not all parameters set in the SMC_theme_ggplot propagate to the resulting plotly figure.
One noteworthy exception are the margins, which are automatically determined during the conversion.
Nevertheless, the parameters set here define the appearance of the png and svg output.
}
\examples{
SMC_theme_ggplot()
}
