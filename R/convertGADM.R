#' @keywords datagen
#' @export
#' @import stringr
#' @title Convert and Regularize Data from the GADM Database
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param countryCode ISO-3166-1 alpha-2 country code
#' @param admLevel administrative level to be downloaded
#' @description A SpatialPolygonsDataFrame file is downloaded from the GADM database with 
#' additional columns of data added. The resulting file will be created in the data directory 
#' which can be set with \code{setSpatialDataDir()}. Dataset and file names are generated like this:
#' 
#' \code{paste0('gadm_', countryCode, '_', admLevel)}
#' 
#' Level 0 will return the national outline. Level 1 will give state/province boundaries. etc.
#' @note Not all countries have the same number of levels. Many just have two levels while France has five.
#' @return Name of the dataset being created.
#' @references \url{http://www.gadm.org/country}.
#' @examples
#' \dontrun{
#' convertGADM('DE', 1)
#' }
convertGADM <- function(countryCode=NULL, admLevel=0, nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
    
  # Specify the name of the dataset and file being created
  datasetName <- paste0('GADM_', countryCode, '_', admLevel)
  
  if (nameOnly) return(datasetName)

  # Convert 2-character codes into ISO3
  if (str_length(countryCode) == 2) {
    ISO3 <- codeToCode(countryCode)
  } else {
    stop('The countryCode parameter "',countryCode,'" is not an ISO-3166-1 alpha-2 country code.',call.=FALSE)
  }
    
  # Check if the dataset already exists
  filePath <- paste0(dataDir, '/', datasetName,'.RData')
  if(file.exists(filePath)) {
    datasetName <- get(load(filePath))
    invisible(datasetName)
  }
  
  # Build appropriate request URL for the GADM Database
  url <- paste0('http://biogeo.ucdavis.edu/data/gadm2/R/',
                ISO3, '_adm',
                admLevel, '.RData')

  # Get the data
  # NOTE:  The url() function converts the url string into a 'connection' that can be loaded.
  spDF <- get(load(url(url)))
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  
  if (admLevel == 0) {
    
    #     > names(spDF)
    #     [1] "PID"           "ID_0"          "ISO"           "NAME_ENGLISH"  "NAME_ISO"      "NAME_FAO"      "NAME_LOCAL"   
    #     [8] "NAME_OBSOLETE" "NAME_VARIANTS" "NAME_NONLATIN" "NAME_FRENCH"   "NAME_SPANISH"  "NAME_RUSSIAN"  "NAME_ARABIC"  
    #     [15] "NAME_CHINESE"  "WASPARTOF"     "CONTAINS"      "SOVEREIGN"     "ISO2"          "WWW"           "FIPS"         
    #     [22] "ISON"          "VALIDFR"       "VALIDTO"       "EUmember"     
    
    # NOTE:  Lots of useful potentially useful information here. We will just add the core identifiers
    spDF$ISO3 <- spDF$ISO
    spDF$countryCode <- spDF$ISO2
    spDF$countryName <- spDF$NAME_ENGLISH
    
  } else {
    
    #     > names(spDF)
    #     [1] "PID"       "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"    "NL_NAME_1" "VARNAME_1" "TYPE_1"   
    #     [10] "ENGTYPE_1"
    
    # NOTE:  Lots of useful potentially useful information here. We will just add the core identifiers
    spDF$ISO3 <- spDF$ISO
    spDF$countryCode <- codeToCode(spDF$ISO)
    spDF$countryName <- spDF$NAME_0
    ### spDF$stateCode <- 
    spDF$stateName <- spDF$NAME_1
    
    # NOTE:  A regular patterm emerges beyond level 1
    #     > names(spDF@data)
    #     [1] "PID"       "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"    "ID_2"      "NAME_2"    "ID_3"      "NAME_3"    "NL_NAME_3"
    #     [12] "VARNAME_3" "TYPE_3"    "ENGTYPE_3"
    
    if (admLevel >= 2) {
      spDF$countyName <- spDF$NAME_2
    }
    
  }

  # TODO:
  #   # Group polygons with the same identifier
  #   spDF <- organizePolygons(spDF, uniqueID='timezone')
  
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName,'.RData'))
    
  invisible(datasetName)
}

