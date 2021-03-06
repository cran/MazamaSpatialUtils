% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MazamaSpatialUtils.R
\name{simplify}
\alias{simplify}
\title{Simplify SpatialPolygonsDataFrame}
\usage{
simplify(SPDF, keep = 0.05, ...)
}
\arguments{
\item{SPDF}{Object of class SpatialPolygonsDataFrame.}

\item{keep}{Proportion of points to retain (0-1; default 0.05)}

\item{...}{Arguments passed to \code{rmapshaper::ms_simplify()}}
}
\value{
A simplified spatial polygons dataframe.
}
\description{
Simplify a spatial polygons dataframe. This is a convenience
wrapper for \code{rmapshaper::ms_simplify()}
}
\examples{
\dontrun{
FR <- subset(SimpleCountries, countryCode == 'FR')
par(mfrow = c(3, 3), mar = c(1, 1, 3, 1))
for (i in 9:1) {
  keep <- 0.1 * i
  plot(simplify(FR, keep), main=paste0("keep = ", keep))
}
layout(1)
par(mar = c(5,4,4,2)+.1)
}
}
