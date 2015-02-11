#' @keywords spatial, shapefile
#' @export
#' @title Convert World Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param ... additional arguments (unused)
#' @description A world borders shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file wil be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @return name of the dataset being created
#' @references \url{http://thematicmapping.org/downloads/}
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode

convertTMWorldBorders <- function(nameOnly=FALSE, ...) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()
    
  # Specify the name of the dataset and file being created
  datasetName <- 'TMWorldBorders'
  fileName <- paste0(datasetName,'.RData')
  
  if (nameOnly) return(datasetName)
  
  # Obtain source data
  url <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip"
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  unzip(filePath,exdir=paste0(dataDir,'/world'))
  
  # NOTE:  The 'world' directory has been created
  dsnPath <- paste(dataDir,'world',sep='/')
  
  # Convert shapefile into SpatialPolygonsDataFrame
  spDF <- convertLayer(dsn='world',layerName='TM_WORLD_BORDERS-0.3')

  # NOTE:  Add MazamaSpatialUtils uniform columns, leaving the originals in place
  spDF@data$countryCode <- spDF@data$ISO2
  spDF@data$countryName <- spDF@data$NAME
  spDF@data$longitude <- spDF@data$LON
  spDF@data$latitude <- spDF@data$LAT
  
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}

