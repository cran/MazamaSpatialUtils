% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertGADM.R
\name{convertGADM}
\alias{convertGADM}
\title{Convert Global Administrative Areas (GADM) SPDF}
\usage{
convertGADM(
  countryCode = NULL,
  admLevel = 0,
  nameOnly = FALSE,
  baseUrl = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp"
)
}
\arguments{
\item{countryCode}{ISO-3166-1 alpha-2 country code}

\item{admLevel}{administrative level to be downloaded}

\item{nameOnly}{Logical specifying whether to only return the name without
creating the file.}

\item{baseUrl}{Base URL for data queries.}
}
\value{
Name of the dataset being created.
}
\description{
Create a SpatialPolygonsDataFrame for Global Administrative Areas.
}
\details{
A pre-generated Global Administrative Areas SpatialPolygonsDataFrame
is downloaded and amended with additional columns of data. The resulting file
will be created in the spatial data directory which is set with
\code{setSpatialDataDir()}.

The \code{@data} slot of each SpatialPolygonsDataFrame is both simplified and
modified to adhere to the \pkg{MazamaSpatialUtils} internal standards.
}
\note{
Unlike other \code{convert~()} functions, no checks, cleanup or
simplification is performed.

From the source documentation:

GADM has always had multiple unique IDs associated with a record. In the
future there will be only be the GID and associated tables that link the GID
to other ID systems such as ISO, FIPS, and HASC. The GID starts with the three
letter ISO 3166-1 alpha-3 country code. If there are subdivisions these are
identified by a number from 1 to n, where n is the number of subdivisions at
level 1. This value is concatenated with the country code, using a dot to
delimit the two. For example, AFG.1, AFG.2, ..., AFG.n. If there are second
level subdivisions, numeric codes are assigned within each first level
subdivision and these are concatenated with the first level identifier, using
a dot as delimiter. For example, AFG.1.1, AFG.1.2, AFG.1.3, ..., and AFG.2.1,
AFG.2.2, .... And so forth for the third, fourth and fifth levels. Finally
there is an underscore followed by a version number appended to the code. For
example, AFG.3_1 and AFG.3.2_1. The GID codes are presistent after version
3.6 (there were errors in the codes in version 3.4). If an area changes, for
example if it splits into two new areas, two new codes will be assigned, and
the old code will not be used any more. The version only changes when there
is a major overhaul of the divisions in a country, for example when a whole
new set of subdivsions is introduced.
}
\references{
\url{https://gadm.org/data.html}

\url{https://gadm.org/metadata.html}
}
\keyword{datagen}
