% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MazamaSpatialUtils.R
\name{dissolve}
\alias{dissolve}
\title{Aggregate shapes in a simple features data frame}
\usage{
dissolve(SFDF, field = NULL, sum_fields = NULL, copy_fields = NULL, ...)
}
\arguments{
\item{SFDF}{Object of class simple features data frame.}

\item{field}{Name of the field to dissolve on.}

\item{sum_fields}{Names of fields to sum.}

\item{copy_fields}{Names of fields to copy. The first instance of each field will be
copied to the aggregated feature}

\item{...}{arguments passed to \code{rmapshaper::ms_dissolve()}}
}
\value{
A simple features dataframe with aggregated shapes.
}
\description{
Aggregate shapes in a simple features dataframe. This is a
convenience wrapper for \code{\link[rmapshaper]{ms_dissolve}}.
}
\examples{
\donttest{
regions <- dissolve(SimpleCountries, field = "UN_region", sum_fields = "area")
plot(regions)
dplyr::glimpse(regions)
}
}
