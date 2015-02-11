#' @keywords spatial, shapefile
#' @export
#' @title Convert US County Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param ... additional arguments (unused)
#' @description A US county borders shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file wil be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @return Large SpatialPolygonsDataFrame
#' @references \url{http://www2.census.gov/geo/tiger/GENZ2013}


convertUSCensusCounties <- function(nameOnly=FALSE, ...) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Always require a data directory
  if (is.null(dataDir)) {
    stop('dataDir must be specified and must be a user writable directory', call.=FALSE)
  }
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCounties'
  fileName <- paste0(datasetName,'.RData')
    
  if (nameOnly) return(datasetName)

  # Specify URL
  url = "http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_county_20m.zip"
  
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  unzip(filePath,exdir=paste0(dataDir, "/counties"))
  
  # Create temporary directory to open data in
  dsnPath <- paste(dataDir,'counties',sep='/')
  
  # Convert shapefile into SpatialPolygonsDataFrame
  spDF <- convertLayer(dsn='counties',layerName="cb_2013_us_county_20m")
  
  
  # Get STATEFP conversion table from wikipedia. We need this to find state names and codes
  # from on STATEFP values.
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
  
  # NOTE:  Add MazamaSpatialUtils uniform columns, leaving the originals in place
  spDF@data$stateCode <- apply(spDF@data, 1, extractState, col='Alpha code')
  spDF@data$stateName <- apply(spDF@data, 1, extractState, col="Name")
  spDF@data$countyName <- spDF@data$NAME
  
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName,".RData"))
  
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}
