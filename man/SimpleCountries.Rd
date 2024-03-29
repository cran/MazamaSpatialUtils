% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{SimpleCountries}
\alias{SimpleCountries}
\title{Simplified spatial dataset of country boundaries.}
\format{
A simple features data frame with 246 records and 7 columns of data.
}
\usage{
SimpleCountries
}
\description{
SimpleCountries is a simplified world borders dataset suitable
for global maps and quick spatial searches. This dataset is distributed with
the package and is can be used with \code{getCountry()},
\code{getCountryCode()} and \code{getCountryName()} when restricting searches
to land-based locations.
}
\details{
This dataset is equivalent to TMWorldBordersSimple but with fewer columns of data.
}
\seealso{
convertTMWorldBordersSimple

This dataset was generated on 2022-11-04 by running:

\preformatted{
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial_0.8")

convertTMWorldBorders()

loadSpatialData("NaturalEarthAdm0_05")

columnNames <- c("countryCode", "countryName", "ISO3", "FIPS",
                 "UN_region", "polygonID")
SimpleCountries <- NaturalEarthAdm0_05[, columnNames]
save(SimpleCountries, file = "data/SimpleCountries.rda")
}
}
\keyword{datasets}
