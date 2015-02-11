#' @keywords spatial, shapefile
#' @export
#' @title Create Timezone Dataset
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param ... additional arguments (unused)
#' @description A world timezone shapefile is downloaded from \url{http://efele.net/maps/tz/world/}
#' and converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting file wil be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @return name of the dataset being created
#' @seealso setSpatialDataDir
#' @seealso convertWikipediaTimezoneTable

convertWorldTimezones <- function(nameOnly=FALSE, ...) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'WorldTimezones'
  fileName <- paste0(datasetName,'.RData')
  
  if (nameOnly) return(datasetName)
  
  # Obtain source data
  url <- "http://efele.net/maps/tz/world/tz_world.zip"
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  unzip(filePath,exdir=dataDir)
  
  # NOTE:  The 'world' directory has been created
  dsnPath <- paste(dataDir,'world',sep='/')
  
  # Convert shapefile into SpatialPolygonsDataFrame
  spDF <- convertLayer(dsn='world',layerName='tz_world')
  
  # Rename "TZID" to "timezone"
  names(spDF@data) <- c('timezone')
  
  # Now get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()
  
  # Merge the additional data onto the @data slot of the spDF
  spDF@data <- dplyr::left_join(spDF@data, wikipediaTimezoneTable)

  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste(dataDir,fileName,sep='/'))
  
  # For this 'special' dataset we will create an extra datatable
  TimezoneTable <- spDF@data[!duplicated(spDF@data),]
  save(list=c("TimezoneTable"),file=paste0(dataDir,'/TimezoneTable.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}

