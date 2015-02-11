#' @keywords spatial, shapefile
#' @export
#' @title Convert World Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param ... additional arguments (unused)
#' @description A world borders shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file wil be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @details
#' This shapefile is a greatly simplified version of the TMWorldBorders shapefile and is especially suited
#' for spacial searches. This is the default dataset used in \code{getCountry()} and \code{getCountryCode()}.
#' Users may wish to use a higher resolution dataset when plotting.
#' @return name of the dataset being created
#' @references \url{http://thematicmapping.org/downloads/}
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode

convertTMWorldBordersSimple <- function(nameOnly=FALSE, ...) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()
    
  # Specify the name of the dataset and file being created
  datasetName <- 'TMWorldBordersSimple'
  fileName <- paste0(datasetName,'.RData')
  
  if (nameOnly) return(datasetName)
  
  # Obtain source data
  url <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  unzip(filePath,exdir=paste0(dataDir,'/world'))
  
  # NOTE:  The 'world' directory has been created
  dsnPath <- paste(dataDir,'world',sep='/')
  
  # Convert shapefile into SpatialPolygonsDataFrame
  spDF <- convertLayer(dsn='world',layerName='TM_WORLD_BORDERS_SIMPL-0.3')

  # NOTE:  Add MazamaSpatialUtils uniform columns, leaving the originals in place
  spDF@data$countryCode <- spDF@data$ISO2
  spDF@data$countryName <- spDF@data$NAME
  spDF@data$longitude <- spDF@data$LON
  spDF@data$latitude <- spDF@data$LAT
  
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # For this 'special' dataset we will create an extra datatable
  CountryTable <- spDF@data
  save(list=c("CountryTable"),file=paste0(dataDir,'/CountryTable.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}

