#' @keywords spatial
#' @export
#' @title Return State Names at Specified Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use -- defaults to 'NaturalEarthAdm1'
#' @param countryCodes vector of country codes, defaults to \code{NULL}
#' @param allData logical specifying whether to return a full dataframe
#' @description Uses spatial comparison to determine which 'state' polygons the 
#'     locations fall into and returns the ISO 3166-2 2-character state code
#'     strings for those polygons.
#'     
#'     Specification of \code{countryCodes} limits spatial searching to the specified
#'     countrieis and greatly improves performance.
#'     
#'     If \code{allData=TRUE} additional data is returned.
#' @return vector of ISO-3166-2 alpha-2 state codes
#' @examples
#' \dontrun{
#' lon <- seq(-140,-90)
#' lat <- seq(20,70)
#' getCountryCode(lon,lat)
#' }
#' @seealso getSpatialData
getState <- function(lon, lat, dataset='NaturalEarthAdm1', countryCodes=NULL, allData=FALSE) {
  
  # Sanity check
  if (!exists(dataset)) {
    stop('Missing database. Please loadSpatialData("',dataset,'")',call.=FALSE)
  }
  
  spDF <- get(dataset)
  
  # Subset by country before searching
  if (!is.null(countryCodes)) spDF <- spDF[spDF$countryCode %in% countryCodes,]
  
  locationsDF <- getSpatialData(spDF,lon,lat)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    stateName <- locationsDF$stateName
    
    # Sanity check -- missing stateCode implies location over water  
    badMask <- is.na(stateName)
    if (sum(badMask) > 0) {
      if(is.null(countryCodes)) {
        warning(paste(sum(badMask),"locations appear to be over international waters and no state can be assigned"))
      } else {
        warning(paste(sum(badMask),"locations appear to be either over international waters or not in given countryCodes and no state can be assigned"))
      }
    }  
    
    return(stateName)
    
  }
  
  
}
