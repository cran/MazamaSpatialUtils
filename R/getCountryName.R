#' @keywords locator
#' @export
#' @title Return Country Names at Specified Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use -- defaults to 'SimpleCountries'
#' @param countryCodes vector of countryCodes -- defaults to \code{NULL}
#' @param allData logical specifying whether a full dataframe should be returned
#' @description Uses spatial comparison to determine which country polygons the 
#'     locations fall into and returns the country name for those polygons.
#'     
#'     If \code{allData=TRUE}, additional data is returned.
#' @return Vector of country names in English.
#' @examples
#' lon <- seq(0,50)
#' lat <- seq(0,50)
#' getCountryName(lon,lat)
#' @references \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @seealso SimpleCountries
#' @seealso getSpatialData
getCountryName <- function(lon, lat, dataset='SimpleCountries', countryCodes=NULL, allData=FALSE) {
  
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
    
    countryName <- locationsDF$countryName
    
    # Sanity check -- missing countryName implies location over water  
    badMask <- is.na(countryName)
    if (sum(badMask) > 0) {
      if(is.null(countryCodes)) {
        warning(paste(sum(badMask),"locations appear to be over international waters and no country can be assigned"))
      } else {
        warning(paste(sum(badMask),"locations appear to be either over international waters or not in given countryCodes and no country can be assigned"))
      }
    }  
    
    return(countryName)
    
  }
  
  
}
