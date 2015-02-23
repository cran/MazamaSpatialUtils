#' @keywords datagen
#' @export
#' @title Create Timezone Dataset
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A world timezone shapefile is downloaded from \url{http://efele.net/maps/tz/world/}
#' and converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
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

