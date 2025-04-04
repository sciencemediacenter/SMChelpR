% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_SMC.R
\name{size_in_pt}
\alias{size_in_pt}
\title{size_in_pt}
\usage{
size_in_pt(size_in_px, dpi = 96)
}
\arguments{
\item{size_in_px}{numeric, size in pixels.}

\item{dpi}{numeric, dots per inch.}
}
\description{
Conversion between units of font sizes, linewidths and point sizes in ggplot2 and plotly.
}
\details{
ggplot uses pt as units for font sizes, linewidths and point sizes, whereas plotly uses px.
}
\examples{
size_in_pt(12, dpi = 96)
}
