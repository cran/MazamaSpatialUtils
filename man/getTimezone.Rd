% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTimezone.R
\name{getTimezone}
\alias{getTimezone}
\title{Return Olson timezones at specified locations}
\usage{
getTimezone(
  longitude = NULL,
  latitude = NULL,
  datasetName = "SimpleTimezones",
  countryCodes = NULL,
  allData = FALSE,
  useBuffering = FALSE
)
}
\arguments{
\item{longitude}{Vector of longitudes in decimal degrees East.}

\item{latitude}{Vector of latitudes in decimal degrees North.}

\item{datasetName}{Name of spatial dataset to use.}

\item{countryCodes}{Vector of ISO 3166-1 alpha-2 country codes.}

\item{allData}{Logical specifying whether a full dataframe should be returned.}

\item{useBuffering}{Logical flag specifying the use of location buffering to
find the nearest polygon if no target polygon is found.}
}
\value{
Vector of Olson timezones.
}
\description{
Uses spatial comparison to determine which timezone polygons the
locations fall into and returns the Olson timezone strings for those polygons.

Specification of \code{countryCodes} limits spatial searching to the
specified countries and greatly improves performance.

If \code{allData=TRUE}, additional data is returned.
}
\examples{
library(MazamaSpatialUtils)

longitude <- seq(-120, -60, 5)
latitude <- seq(20, 80, 5)

getTimezone(longitude, latitude)

}
\references{
\url{https://github.com/evansiroky/timezone-boundary-builder}
}
\keyword{locator}
