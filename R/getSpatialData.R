#' @keywords locator
#' @export
#' @title Return Spatial Data Associated with a Set of Locations
#' @param spDF object of class SpatialPolygonsDataFrame
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @description Returns a dataframe or vector of data.
#' @details Any spatial polygon may be passed in.
#' @return Vector or dataframe of data.
getSpatialData <- function(spDF, lon, lat) {
  
  # Sanity check -- same number of lats and lons and datetimes
  if ( length(lon) != length(lat) ) {
    stop(paste("ERROR in getSpatialData:  arguments 'lon' and 'lat' must have the same length."))
  }

  # Create the array of locations and use the same projection as spDF
  location <- sp::SpatialPoints(list(lon, lat))
  location@proj4string <- spDF@proj4string
  
  # Use the 'over' function to find which polygon a location is in and extract data
  locationsDF <- sp::over(location, spDF)
  
  return(locationsDF)
}

