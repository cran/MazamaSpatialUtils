% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertLayer.R
\name{convertLayer}
\alias{convertLayer}
\title{Convert Shapefile Layer to Spatial Polygon Dataframe}
\usage{
convertLayer(dsn = "", layerName = "", encoding = NULL)
}
\arguments{
\item{dsn}{dsn argument to readOGR}

\item{layerName}{layer argument to readOGR}

\item{encoding}{encoding string (.e.g. 'latin1') passed to rgdal::readOGR()}
}
\value{
An object of class \code{SpatialPolygonsDataFrame}
}
\description{
Raw shapefiles are read in using the \code{rgdal::readOGR()} function from the \pkg{rgdal} package.
Spatial data are reprojected onto a standard projection with a CRS class of
\code{"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"} before being returned.

If shapefiles have no projection information they are assumed to 'geographic' coordinates  and will be assigned
\code{"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"} before being returned.
}
\keyword{internal}
