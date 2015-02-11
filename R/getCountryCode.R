#' @keywords spatial
#' @export
#' @title Return Country ISO Codes at Specified Locations
#' @param lon Vector of longitudes in decimal degrees
#' @param lat Vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use -- defaults to 'SimpleCountries'
#' @param countryCodes vector of countryCodes -- defaults to \code{NULL}
#' @param allData logical specifying whether a full dataframe should be returned
#' @description Uses spatial comparison to determine which country polygons the 
#'     locations fall into and returns the country name and country code
#'     strings for those polygons.
#'     
#'     If \code{allData=TRUE} additional data is returned.
#' @details Countries polygons are from \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}.
#' @return vector of country codes
#' @examples
#' lon <- seq(0,50)
#' lat <- seq(0,50)
#' getCountry(lon,lat)
#' @seealso SimpleCountries
#' @seealso getSpatialData
getCountryCode <- function(lon, lat, dataset='SimpleCountries', countryCodes=NULL, allData=FALSE) {
  
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
    
    countryCode <- locationsDF$countryCode
    
    # Sanity check -- missing countryCode implies location over water  
    badMask <- is.na(countryCode)
    if (sum(badMask) > 0) {
      if(is.null(countryCodes)) {
        warning(paste(sum(badMask),"locations appear to be over international waters and no countryCode can be assigned"))
      } else {
        warning(paste(sum(badMask),"locations appear to be either over international waters or not in given countryCodes and no countryCode can be assigned"))
      }
    }  
    
    return(countryCode)
    
  }
  
  
}