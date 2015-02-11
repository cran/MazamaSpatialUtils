#' @keywords spatial
#' @export
#' @title Return US County Name at Specified Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use -- defaults to 'USCensusCounties'
#' @param stateCodes vector of stateCodes used to limit the search -- defaults to \code{NULL}
#' @param allData logical specifying whether a full dataframe should be returned
#' @description Uses spatial comparison to determine which county polygons the 
#'     locations fall into and returns the county name strings for those polygons.
#'     
#'     Specification of \code{stateCodes} limits spatial searching to the specified states
#'     and greatly improves performance.
#'     
#'     If \code{allData=TRUE} additional data is returned.
#' @return vector of counties
#' @examples
#' \dontrun{
#' lon <- seq(-140,-90)
#' lat <- seq(20,70)
#' getCountry(lon,lat)
#' }
#' @references \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @seealso getSpatialData
getUSCounty <- function(lon, lat, dataset='USCensusCounties', stateCodes=NULL, allData=FALSE) {
  
  # Sanity check
  if (!exists(dataset)) {
    stop('Missing database. Please loadSpatialData("',dataset,'")',call.=FALSE)
  }
  
  spDF <- get(dataset)
  
  # Subset by state before searching
  if (!is.null(stateCodes)) spDF <- spDF[spDF$stateCode %in% stateCodes,]
  
  locationsDF <- getSpatialData(spDF,lon,lat)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    name <- locationsDF$countyName
    
    # Sanity check -- missing county implies location outside the US 
    badMask <- is.na(name)
    if (sum(badMask) > 0) {
      if(is.null(stateCodes)) {
        warning(paste(sum(badMask),"locations appear to be over international waters or outside the US and no county can be assigned"))
      } else {
        warning(paste(sum(badMask),"locations appear to be either over international waters or not in given stateCodes and no county can be assigned"))
      }
    }  
    
    return(name)
    
  }
  
  
}