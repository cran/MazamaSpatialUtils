#' @keywords datagen
#' @export
#' @title Convert World Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A world borders shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file wil be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @details
#' This shapefile is a greatly simplified version of the TMWorldBorders shapefile and is especially suited
#' for spatial searches. This is the default dataset used in \code{getCountry()} and \code{getCountryCode()}.
#' Users may wish to use a higher resolution dataset when plotting.
#' @return Name of the dataset being created.
#' @references \url{http://thematicmapping.org/downloads/}
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode
convertTMWorldBordersSimple <- function(nameOnly=FALSE) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()
    
  # Specify the name of the dataset and file being created
  datasetName <- 'TMWorldBordersSimple'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for TM world borders
  url <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"
  
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  unzip(filePath,exdir=paste0(dataDir,'/world'))
    
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'world' directory has been created
  dsnPath <- paste(dataDir,'world',sep='/')
  spDF <- convertLayer(dsn=dsnPath,layerName='TM_WORLD_BORDERS_SIMPL-0.3')

  #   > names(spDF)
  #   [1] "FIPS"      "ISO2"      "ISO3"      "UN"        "NAME"      "AREA"      "POP2005"   "REGION"    "SUBREGION" "LON"       "LAT"
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  names(spDF) <- c('FIPS','countryCode','ISO3','UN_country','countryName','area','population2005','UN_region','UN_subregion','longitude','latitude')
  
  # Rationalize units:
  # * SI  
  # NOTE:  Bizzarely, area seems to be in units of (10 km^2). Convert these to m^2
  spDF$area <- spDF$area * 1e7
  
  # Group polygons with the same identifier
  ###spDF <- organizePolygons(spDF, uniqueID='countryCode', sumColumns=c('area','population2005'))
  # NOTE:  This dataset already has grouped polygons
  
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}

