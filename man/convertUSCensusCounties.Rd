% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertUSCensusCounties.R
\name{convertUSCensusCounties}
\alias{convertUSCensusCounties}
\title{Convert US county borders shapefile}
\usage{
convertUSCensusCounties(nameOnly = FALSE, simplify = TRUE)
}
\arguments{
\item{nameOnly}{Logical specifying whether to only return the name without
creating the file.}

\item{simplify}{Logical specifying whether to create "_05", _02" and "_01"
versions of the file that are simplified to 5\%, 2\% and 1\%.}
}
\value{
Name of the dataset being created.
}
\description{
Create a SpatialPolygonsDataFrame for US counties.
}
\details{
A US county borders shapefile is downloaded and converted to a
SpatialPolygonsDataFrame with additional columns of data. The resulting file
will be created in the spatial data directory which is set with
\code{setSpatialDataDir()}.

The source data is from 2019.
}
\note{
From the source documentation:

The primary legal divisions of most states are termed counties. In Louisiana,
these divisions are known as parishes. In Alaska, which has no counties, the
equivalent entities are the organized boroughs, city and boroughs,
municipalities, and for the unorganized area, census areas. The latter are
delineated cooperatively for statistical purposes by the State of Alaska and
the Census Bureau. In four states (Maryland, Missouri, Nevada, and Virginia),
there are one or more incorporated places that are independent of any county
organization and thus constitute primary divisions of their states. These
incorporated places are known as independent cities and are treated as
equivalent entities for purposes of data presentation. The District of
Columbia and Guam have no primary divisions, and each area is considered an
equivalent entity for purposes of data presentation. The Census Bureau treats
the following entities as equivalents of counties for purposes of data
presentation: Municipios in Puerto Rico, Districts and Islands in American
Samoa, Municipalities in the Commonwealth of the Northern Mariana Islands,
and Islands in the U.S. Virgin Islands. The entire area of the United States,
Puerto Rico, and the Island Areas is covered by counties or equivalent entities.

You can join this file with table data downloaded from American FactFinder by
using the AFFGEOID field in the cartographic boundary file.
}
\references{
\url{https://www2.census.gov/geo/tiger/GENZ2019/}
}
\seealso{
setSpatialDataDir

getUSCounty
}
\keyword{datagen}
