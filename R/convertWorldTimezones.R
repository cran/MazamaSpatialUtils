#' @keywords datagen
#' @export
#' @title Create Timezone Dataset
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A world timezone shapefile is downloaded from \url{http://efele.net/maps/tz/world/}
#' and converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @note
#' The following list of timezones have polygons but the associated rows in the dataframe have no data.
#' These timezones also have no \code{countryCode} assigned. We hope to rectify this in a future release.
#' \preformatted{
#' > WorldTimezones@@data$timezone[is.na(WorldTimezones$countryCode)]
#' [1] "Europe/Zagreb"         "Europe/Vatican"        "America/Coral_Harbour" "Arctic/Longyearbyen"
#' [5] "uninhabited"           "America/Kralendijk"    "Europe/Jersey"         "Europe/Bratislava"
#' [9] "America/St_Barthelemy" "Europe/Ljubljana"      "Europe/Mariehamn"      "Europe/Podgorica" 
#' [13] "Europe/Isle_of_Man"    "Europe/Guernsey"       "Europe/San_Marino"     "Europe/Skopje"   
#' [17] "Europe/Sarajevo"       "America/Lower_Princes" "America/Marigot"       "Africa/Juba"
#' }       
#' @return Name of the dataset being created.
#' @seealso setSpatialDataDir
#' @seealso convertWikipediaTimezoneTable
convertWorldTimezones <- function(nameOnly=FALSE) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'WorldTimezones'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for world timezones
  url <- "http://efele.net/maps/tz/world/tz_world.zip"
  
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  unzip(filePath,exdir=dataDir)
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste(dataDir,'world',sep='/')
  spDF <- convertLayer(dsn=dsnPath,layerName='tz_world')
  
  # Rename "TZID" to "timezone"
  names(spDF@data) <- c('timezone')
  
  # Now get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()
  
  # Merge the additional data onto the @data slot of the spDF
  spDF@data <- dplyr::left_join(spDF@data, wikipediaTimezoneTable)
  
  # Group polygons with the same identifier
  spDF <- organizePolygons(spDF, uniqueID='timezone')

  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))  
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}
