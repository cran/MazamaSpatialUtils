#' @keywords locator
#' @export
#' @import rgdal
#' @title Return Spatial Data Associated with a Set of Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param SPDF object of class SpatialPolygonsDataFrame
#' @param verbose logical flag controlling detailed progress statements
#' @description All locations are first converted to \code{SpatialPoints} objects.
#' The \pkg{sp::over()} function is then used to determine which polygon from \code{SPDF}
#' each location falls in. The dataframe row associated with each polygon is then
#' associated with each ocation.
#' @details For coastal locations it can often happen that the precise coordinates
#' of the location lie outside the boundaries of low resolution SpatialPolygonsDataFrame.
#' To account for this, locations that remain unassociated after the first pass are then
#' buffered to create small circles. All polygons are then checked to see if there is any
#' intersection with the now buffered location. A buffering loop increases buffer size
#' though the following radii until an intersecting polygon is found:  1km, 2km, 5km,
#' 10km, 20km, 50km, 100km, 200km.
#' 
#' If the buffered location is more than 200km away from any polygon, a value of \code{NA}
#' is returned.
#' @return Vector or dataframe of data.
getSpatialData <- function(lon,lat,SPDF,verbose=FALSE) {
  
  # Sanity check -- same number of lats and lons and datetimes
  if ( length(lon) != length(lat) ) {
    stop(paste("ERROR in getSpatialData:  arguments 'lon' and 'lat' must have the same length."))
  }

  # Create the array of locations and use the same projection as SPDF
  location <- sp::SpatialPoints(list(lon, lat), proj4string=SPDF@proj4string)
  
  # Use the 'over' function to find which polygon a location is in and extract data
  locationsDF <- sp::over(location, SPDF)

  # Finds the index of the points where the 'over' function failed to place a coordinate 
  # location in a polygon
  badPointsIndex <- which(is.na(locationsDF$countryCode))
  
  # If NA points are found, increment radius until limit is reached or a country is found
  # If there are no NA points, this block is skipped
  if (length(badPointsIndex) != 0) {
  
    if (verbose) print(paste0(length(badPointsIndex),' points were outside of all polygons -- begin buffering ...'))

    # Sets radius values (in meters) in roughly logarithmic increases
    searchRadii <- c(1000,2000,5000,10000,20000,50000,100000,200000)
    
    # Restructures the given SPDF from a SpatialPolygonsDataFrame to a list of
    # SpatialPolygons.
    # NOTE: We do this restructuring for ease of use later when using the 
    # 'gIntersects' function
    SpatialPolygonsList <- list()
    for (i in 1:length(SPDF)) {
      SPDF_Polygons <- SPDF@polygons[[i]]
      SpatialPolygonsList[i] <- sp::SpatialPolygons(list(SPDF_Polygons), proj4string=SPDF@proj4string)
    }
    
    # Loop over points of interest, trying to find an intersecting polygon 
    for (pointIndex in badPointsIndex) {
      if (verbose) print(paste0('pointIndex = ',pointIndex))
      # Select the individual point we are analyzing
      pointOfInterest <- location[pointIndex]
      for (radius in searchRadii) {
        if (verbose) print(paste0('Using radius=',radius,' to search through ',length(SpatialPolygonsList),' polygons ...'))
        # Switch to a planar projection in order to use 'gBuffer' function
        pointOfInterest <- sp::spTransform(pointOfInterest, sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
        # Buffer the point
        buffer <- rgeos::gBuffer(pointOfInterest, width=radius)
        # Transforms back to geographical coordinates, exiting the loop if -180:180, -90:90 domain boundaries are reached
        buffer <- try(sp::spTransform(buffer, SPDF@proj4string), silent=FALSE)
        if (class(buffer)=="try-error") break      
        radiusIntersectsPolygon <- FALSE
        # Use gIntersects to determine whether each buffered point is contained within each polygon
        for (k in 1:length(SpatialPolygonsList)) {
          if ( rgeos::gIntersects(buffer, SpatialPolygonsList[[k]]) ) {
            locationsDF[pointIndex,] <- SPDF@data[k,]
            radiusIntersectsPolygon <- TRUE
            break
          }
        }
        # Bail out of searchRadii loop
        if (radiusIntersectsPolygon) break
      }
    }
    
  }
  
  return(locationsDF)
}