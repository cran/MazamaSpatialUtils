#' @keywords locator
#' @export
#' @title Return HUCs at Specified Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param SPDF spatial polygons dataset of HUCs
#' @param HUCs vector of Hydrologic Unit Codes
#' @param allData logical specifying whether to return a full dataframe
#' @description Uses spatial comparison to determine which HUC polygons the 
#'     locations fall into and returns the HUC identifier strings for those polygons.
#'     
#'     If \code{allData=TRUE}, additional data is returned.
#' @return Vector of HUC identifiers.
#' @seealso getSpatialData
 

getHUC <- function(lon, lat, SPDF, HUCs=NULL, allData=FALSE) {
  
  # check if longitude and latitude falls in the right range
  if ( min(lon)< -180 || max(lon) > 180 || min(lat) < -90 || max(lat) > 90 ) {
    stop('Longitude or latitude is not specified in the correct range -180:180, -90:90',call.=FALSE)
  }  
  
  # Identify HUC string partial matches to use as a mask 
  if (!is.null(HUCs)){
    HUCMask <- rep(FALSE, nrow(SPDF))
    for (HUC in HUCs){
      regex <- paste0('^', HUC)
      mask <- stringr::str_detect(SPDF@data$HUC, regex)
      HUCMask <- HUCMask | mask
    }
    SPDF <- SPDF[HUCMask,]
  }
  
  # Pull out rows from SPDF@data based on whether a lon-lat point falls into a certain polygon 
  locationsDF <- getSpatialData(lon,lat,SPDF)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    HUC <- locationsDF$HUC
    HUCName <- locationsDF$HUCName
    
    return(HUC)
    
  }
  
  
}
