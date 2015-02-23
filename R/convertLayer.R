#' @keywords internal
#' @import rgdal
#' @import sp
#' @export
#' @title Convert Shapefile Layer to Spatial Polygon Dataframe
#' @param dsn dsn argument to readOGR
#' @param layerName layer argument to readOGR
#' @description Raw shapefiles are read in using the \code{readOGR()} function from the \pkg{rgdal} package.
#' Spatial data are reprojected onto a standard projection with \code{"+proj=longlat"} before being returned.
#' @return An object of class \code{SpatialPolygonsDataFrame}
convertLayer <- function(dsn="", layerName="") {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Always require a data directory
  if (is.null(dataDir)) {
    stop('dataDir must be specified and must be a user writable directory', call.=FALSE)
  }
  
  # Switch directories
  oldDir <- getwd()
  setwd(dataDir)
  
  # Load the shapefiles 
  data_projected <- rgdal::readOGR(dsn=dsn, layer=layerName, stringsAsFactors=FALSE)
  
  # Return to user directory
  setwd(oldDir)
  
  # Reproject to "longlat"
  spDF <- sp::spTransform(data_projected, sp::CRS("+proj=longlat"))
  
  return(spDF)  
}

