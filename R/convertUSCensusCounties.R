#' @keywords datagen
#' @export
#' @import rvest
#' @title Convert US County Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A US county borders shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @return Large SpatialPolygonsDataFrame.
#' @references \url{http://www2.census.gov/geo/tiger/GENZ2013}
convertUSCensusCounties <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCounties'
    
  if (nameOnly) return(datasetName)

  # Build appropriate request URL for US County Borders
  url = "http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_county_20m.zip"
  
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  unzip(filePath,exdir=paste0(dataDir, "/counties"))
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste(dataDir,'counties',sep='/')
  spDF <- convertLayer(dsn=dsnPath,layerName="cb_2013_us_county_20m")
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  #   > names(spDF)
  #   [1] "STATEFP"  "COUNTYFP" "COUNTYNS" "AFFGEOID" "GEOID"    "NAME"     "LSAD"     "ALAND"    "AWATER"  
  
  
  # Get STATEFP conversion table from wikipedia. We need this to find state names and codes
  # from STATEFP values.
  # URL of STATEFP conversions
  url <- "http://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code"

  # Get the raw html from the url
  wikiDoc <- rvest::html(url)
  
  # Get a list of tables in the document
  tables <- rvest::html_nodes(wikiDoc, "table")
  
  # Assume the relevant list is the first table and parse that into a dataframe
  StateTable <- rvest::html_table(tables[[1]])
  
  # Given a row of country data, use the wikipedia table to find state code and name
  extractState <- function(row, col) {
    state <- StateTable[StateTable['Numeric code']==as.numeric(row['STATEFP']),]
    return(toString(state[col]))
  }
  
  # Uniform naming
  spDF$countryCode <- 'US'
  spDF$countryName <- 'United States'
  spDF$stateCode <- apply(spDF@data, 1, extractState, col='Alpha code')
  spDF$stateName <- apply(spDF@data, 1, extractState, col="Name")
  spDF$countyName <- spDF$NAME
  
  # TODO:  Need to include LSAD to differentiate, e.g. "St. Louis City" from "St. Louis County"
  # TODO:  1) get LSAD levels and include LSAD as a factor
  # TODO:  2) add stateCode_countyName_LSAD as another column
  # TODO:  3) organizePolygons(spDF, uniqueID=stateCode_countyName_LSAD, sumColumns=c('areaLand','areaWater))
  
  # TODO:  replace column indices with column names
  spDF <- spDF[,c(6, 8, 9, 10, 11, 12, 13, 14, 2)]
  names(spDF) <- c('name', 'areaLand', 'areaWater', 'countryCode', 'countryName', 
                   'stateCode', 'stateName', 'countyName', 'countyFIPS')

#   # Group polygons with the same identifier
#   spDF <- organizePolygons(spDF, uniqueID='timezone')

# TODO:  subset/reorganize column names
  
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName,".RData"))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}
