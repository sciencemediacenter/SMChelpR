% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leaflet_SMC.R
\name{addLegendCustom}
\alias{addLegendCustom}
\title{addLegendCustom}
\usage{
addLegendCustom(
  map,
  position,
  colors,
  labels,
  sizes,
  title = NULL,
  opacity = 0.5
)
}
\arguments{
\item{map}{Leaflet map widget}

\item{position}{character, position of the legend}

\item{colors}{character vector, colors of the circles}

\item{labels}{character vector, labels of the circles}

\item{sizes}{numeric vector, sizes of the circles}

\item{title}{character, title of the legend}

\item{opacity}{numeric, opacity of the circles}
}
\description{
Wrapper function for addLegend() that allows to put circles as legend items.
}
