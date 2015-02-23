#' @keywords datagen
#' @export
#' @import rgeos
#' @title Convert USGS Hydrologic Unit Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A hydrologic unit shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame. The resulting file will be created in the data directory 
#' which can be set with \code{setSpatialDataDir()}.
#' @details The HUC250k dataset consists of 8-digit HUCs.
#' @return Name of the dataset being created.
#' @references \url{http://water.usgs.gov/GIS/metadata/usgswrd/XML/huc250k.xml}
#' @seealso setSpatialDataDir
convertUSGSHUC8 <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USGSHUC8'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for the USGS Hydrologic Units
  url <- 'http://water.usgs.gov/GIS/dsdl/huc250k_shp.zip'
  
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  unzip(filePath,exdir=dataDir)
        
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste0(dataDir, '/huc250k_shp')
  shpName <- 'huc250k'
  spDF <- convertLayer(dsn=dsnPath,layerName=shpName)

  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  
  #   names(spDF)
  #   [1] "AREA"       "PERIMETER"  "HUC250K_"   "HUC250K_ID" "HUC_CODE"   "HUC_NAME"   "REG"       
  #   [8] "SUB"        "ACC"        "CAT"       
  
  # Subset this dataframe to include only obviously useful columns
  usefulColumns <- c('AREA','HUC_CODE','HUC_NAME')
  spDF <- spDF[,usefulColumns]
  names(spDF) <- c('area','HUC','HUCName')

  # Group polygons with duplicated hydrologic unit codes
  spDF <- organizePolygons(spDF, uniqueID='HUC', sumColumns='area')

  # NOTE:  Area appears to be in units of m^2
  # NOTE:  Puyallup = 2566209748 ~= 2455 km^2 from Wikipedia
  
  # Calculate centroids to help add more metadata
  centroids <- rgeos::gCentroid(spDF, byid=TRUE)
  lon <- coordinates(centroids)[,1]
  lat <- coordinates(centroids)[,2]
  
  # Add more standard columns
  spDF$longitude <- lon
  spDF$latitude <- lat  
  spDF$countryCode <- 'US'
  spDF$countryName <- 'United States'
  # NOTE: Using getStateCode() with centroids causes 37 locations be in a body of water resulting in 'NA'
  suppressWarnings(spDF$stateCode <- getStateCode(lon, lat))
  spDF$stateName <- codeToState(spDF$stateCode, spDF$countryCode)
   
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName, '.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}

