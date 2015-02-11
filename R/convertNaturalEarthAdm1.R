#' @keywords spatial, shapefile
#' @export
#' @title Convert Level 1 (State) Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param ... additional arguments (unused)
#' @description Returns a SpatialPolygonsDataFrame for a 1st level administrative divisions
#' @description A state border shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file wil be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' @details Within the \pkg{MazamaSpatialUtils} package the phrase 'state' refers to administrative
#' divisions beneath the level of the country or nation. This makes sense in the United 'States'. In
#' other countries this level is known as 'province', 'territory' or some other term.
#' @return name of the dataset being created
#' @references \url{http//www.naturalearthdata.com/download}
#' @seealso setSpatialDataDir
#' @seealso getState, getStateCode

convertNaturalEarthAdm1 <- function(nameOnly=FALSE, ...) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'NaturalEarthAdm1'
  fileName <- paste0(datasetName,'.RData')
    
  if (nameOnly) return(datasetName)
  
  # Specify administrative levels for URL
  adm <- 1
  level <- "states_provinces"

  # Build appropriate request URL for natural earth data
  url = paste0("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_",
               adm, "_",
               level, ".zip")
  
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  unzip(filePath,exdir=paste0(dataDir, "/adm"))
  
  dsnPath <- paste(dataDir,'adm',sep='/')
  
  # Convert shapefile into SpatialPolygonsDataFrame
  shpName <- paste("ne", "10m_admin", adm, level, sep="_")
  spDF <- convertLayer(dsn='adm',layerName=shpName)
  
  # Extract state code from a code_hasc formated string
  extractStateCode <- function(x) {
    split <- strsplit(x, ".", fixed=TRUE)[[1]]
    if (length(split) > 1) {
      return(split[[2]])
    } else {
      return(NA)
    }
  }
  
  # Uniform naming
  spDF$countryCode <- spDF$iso_a2
  spDF$stateCode <- unlist(lapply(spDF$code_hasc, extractStateCode))
  spDF$stateName <- spDF$name

  # Use NA instead of -99 and -90 for missing values
  spDF@data[spDF@data == -99] <- NA
  spDF@data[spDF@data == -90] <- NA
  
  # Remove columns that are more than 80% NA
  spDF <- spDF[,colSums(is.na(spDF@data))<(nrow(spDF)*0.8)]
  
  # Save dataframe as table in data directory
  table <- spDF@data

  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName,".RData"))
  
  # For this 'special' dataset we will create an extra datatable
  StateTable <- spDF@data
  save(list=c("StateTable"),file=paste0(dataDir,'/StateTable.RData'))
  
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}
