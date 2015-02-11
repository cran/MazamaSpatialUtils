#' @docType package
#' @name MazamaSpatialUtils
#' @title Mazama Science spatial data and utility functions.
#' @description This package contains code to convert various spatial datasets into .RData files 
#' with uniformly named identifiers including:
#' \itemize{
#' \item{ countryCode -- ISO 3166-1 alpha-2}
#' \item{ stateCode -- ISO 3166-2 alpha-2}
#' \item{ timezone -- Olson timezone}
#' }
#' Utility functions allow users to determine the country, state, county and timezones
#' associated with a set of locations, e.g. environmental monitoring sites.
#' 
#' The uniformity of identifiers in the spatial datasets also makes it easy to generate maps
#' with data from any dataset that uses standard ISO codes for countries or states.
NULL


#' @docType data
#' @keywords datasets
#' @name SimpleCountries
#' @title World Country Polygons
#' @format A SpatialPolygonsDataFrame with 246 elements and 7 columns of data.
#' @description SimpleCountries is a simplified world borders dataset suitable for global maps
#' and quick spatial searches. This dataset is distributed with the package and is used by
#' default whenever a dataset with country polygons is required.
#' @details This dataset is equivalent to TMWorldBordersSimple but with fewer columns of data.
#' @seealso convertTMWorldBordersSimple
NULL


#' @docType data
#' @keywords datasets
#' @name SimpleTimezones
#' @title World Timezone Polygons
#' @format A SpatialPolygonsDataFrame with 1106 elements and 6 columns of data.
#' @description SimpleTimezones is a simplified world timezones dataset suitable for global maps
#' and quick spatial searches. This dataset is distributed with the package and is used by
#' default whenever a dataset with timezone polygons is required.
#' @details This dataset is a simplified version of WorldTimezones.  It was simplified with
#' \url{http://mapshaper.org}.
#' @seealso convertWorldTimezones
NULL


#' @docType data
#' @keywords datasets
#' @name CountryTable
#' @title Dataframe of World Countries
#' @format A Dataframe with 246 rows and 15 columns
#' @description Data associated with the TM_WORLD_BORDERS_SMPL dataset obtained
#' from \url{http://thematicmapping.org/downloads/}. Core data include:
#' \itemize{
#' \item{countryCode}
#' \item{countryName}
#' \item{latitude}
#' \item{longitude}
#' }
#' 
#' Additional variables can be seen with \code{names(CountryTable)}
#' @details CountryTable.RData is created during initialization and is stored
#' in the directory specified with \code{setSpatialDataDir()}.
#' @seealso setSpatialDataDir
NULL


#' @docType data
#' @keywords datasets
#' @name StateTable
#' @title Dataframe of World States
#' @format A Dataframe with 4647 rows and 52 columns
#' @description Data associated with the ne_10m_admin_1 dataset obtained
#' from \url{http//www.naturalearthdata.com/download}. Core data include:
#' \itemize{
#' \item{countryCode}
#' \item{stateCode}
#' \item{stateName}
#' }
#' 
#' Additional variables can be seen with \code{names(StateTable)}
#' @details StateTable.RData is created during initialization and is stored
#' in the directory specified with \code{setSpatialDataDir()}.
NULL


#' @docType data
#' @keywords datasets
#' @name TimezoneTable
#' @title Dataframe of World Timezones
#' @format A Dataframe with 408 rows and 6 columns
#' @description Data associated with the tz_world dataset obtained
#' from \url{http://efele.net/maps/tz/world/}. Core data include:
#' \itemize{
#' \item{timezone}
#' \item{countryCode}
#' \item{latitude}
#' \item{longitude}
#' }
#' 
#' Additional variables can be seen with \code{names(TimezoneTable)}
#' @details TimezoneTable.RData is created during initialization and is stored
#' in the directory specified with \code{setSpatialDataDir()}.
NULL


# ----- Internal Package State -------------------------------------------------

#' @docType data
#' @keywords spatial
#' @name SpatialDataDir
#' @title Directory for Spatial Data
#' @format absolute path string
#' @description This package maintains an internal directory location which users can set
#' using \code{setSpatialDataDir()}.  All package functions use this directory whenever datasets
#' are created or loaded.
#' 
#' The default setting when the package is loaded is \code{getwd()}
#' @seealso getSpatialDataDir
#' @seealso setSpatialDataDir
spatialEnv <- new.env(parent = emptyenv())
spatialEnv$dataDir <- getwd()

#' @keywords spatial
#' @export
#' @title Get Package Data Directory
#' @description Returns the package data directory where spatial data are located.
#' @return absolute path string
#' @seealso dataDir
#' @seealso setSpatialDataDir
getSpatialDataDir <- function() {
  spatialEnv$dataDir
}

#' @keywords spatial
#' @export
#' @title Set Package Data Directory
#' @param dataDir directory where spatial datasets are created
#' @description Sets the package data directory where spatial data are located.
#' If the directory does not exist, it will be created.
#' @return silently returns previous value of data directory
#' @seealso SpatialDataDir
#' @seealso getSpatialDataDir
setSpatialDataDir <- function(dataDir) {
  old <- spatialEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    spatialEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setSpatialDataDir(",dataDir,")."))
  })     
  invisible(old)
}


# ----- Code <-> Name conversion functions  ------------------------------------

#' @keywords spatial
#' @export
#' @title Convert Country Codes to Country Names
#' @param countryCodes vector of country codes to be converted
#' @description Converts a vecctor of ISO 3166-1 alpha-2 codes to the corresponding English names.
#' @return vector of English country names or NA
codeToCountry <- function(countryCodes) {
  countryTable <- MazamaSpatialUtils::SimpleCountries@data
  # Create a vector of countryNames identified by countryCode
  allNames <- countryTable$countryName
  names(allNames) <- countryTable$countryCode
  return(as.character(allNames[countryCodes]))
}

#' @keywords spatial
#' @export
#' @title Convert Country Names to Country Codes
#' @param countryNames vector of country names to be converted
#' @description Converts a vector of English country names to the corresponding ISO 3166-1 alpha-2 codes.
#' @return vector of ISO 3166-1 alpha-2 codes or NA
countryToCode <- function(countryNames) {
  countryTable <- MazamaSpatialUtils::SimpleCountries@data
  # Create a vector of countryCodes identified by countryName
  allCodes <- countryTable$countryCode
  names(allCodes) <- countryTable$countryName
  return(as.character(allCodes[countryNames]))
}

#' @keywords spatial
#' @export
#' @title Convert State Codes to State Names
#' @param stateCodes vector of state codes to be converted
#' @param countryCodes ISO-3166-1 alpha-2 country codes the state might be found in
#' @param dataset name of dataset containing state-level identifiers -- defaults to 'StateTable'
#' @description Converts a vector of ISO 3166-2 alpha-2 state codes to the corresponding English names.
#' @details For this function to work, you must first run \code{initializeSpatialData()} to
#' download, convert and install the necessary spatial data.
#' @return vector of English state names or NA
#' @seealso convertNaturalEarthAdm1
codeToState <- function(stateCodes, countryCodes=NULL, dataset='StateTable') {
  # Sanity check
  if (!exists(dataset)) {
    stop('Missing database. Please loadSpatialData("',dataset,'")',call.=FALSE)
  }
  DF <- get(dataset)
  # Remove NA state codes
  stateTable <- DF[!is.na(DF$stateCode),]
  # Filter by countryCodes to make searching faster
  if (!is.null(countryCodes)) stateTable <- stateTable[stateTable$countryCode %in% countryCodes,]
  # Create a vector of state names identified by state code
  allStates <- stateTable$stateName
  names(allStates) <- stateTable$stateCode
  return(as.character(allStates[stateCodes]))
}

#' @keywords spatial
#' @export
#' @title Convert State Names to State Codes
#' @param stateNames state names to be converted
#' @param countryCodes ISO 3166-2 alpha-2 countryCodes the state might be found in
#' @param dataset name of dataset containing state-level identifiers -- defaults to 'StateTable'
#' @description Converts a vector of state names to a ISO 3166-2 two character state codes.
#' @details For this function to work, you must first run \code{initializeSpatialData()} to
#' download, convert and install the necessary spatial data.
#' @return vector of ISO 3166-2 codes or NA
#' @seealso convertNaturalEarthAdm1
stateToCode <- function(stateNames, countryCodes=NULL, dataset='StateTable') {
  # Sanity check
  if (!exists(dataset)) {
    stop('Missing database. Please loadSpatialData("',dataset,'")',call.=FALSE)
  }
  DF <- get(dataset)
  # Remove NA state codes
  stateTable <- DF[!is.na(DF$stateCode),]
  # Filter by countryCodes to make searching faster
  if (!is.null(countryCodes)) stateTable <- stateTable[stateTable$countryCode %in% countryCodes,]
  # Create a vector of state codes identified by name
  allCodes <- stateTable$stateCode
  names(allCodes) <- stateTable$stateName
  return(as.character(allCodes[stateNames]))
}


# ----- Initialization----------------------------------------------------------

#' @keywords spatial
#' @export
#' @title Install Core Datasets
#' @description Three core datasets must be installed before the functionality
#' in \pkg{MazamaSpatialUtils} becomes useful. Running \code{initializeSpatialData()} will
#' install these datasets in the the directory specified by \code{setSpatialDataDir()}
#' @return nothing
#' @seealso setSpatialDataDir
initializeSpatialData <- function() {
  # Install and load Country, State and Timezone datasets
  installSpatialData('TMWorldBordersSimple')
  loadSpatialData('TMWorldBordersSimple')
  installSpatialData('NaturalEarthAdm1')
  loadSpatialData('NaturalEarthAdm1')
  installSpatialData('USCensusCounties')
  loadSpatialData('USCensusCounties')
  installSpatialData('WorldTimezones')
  loadSpatialData('WorldTimezones')
  # Load the associated data tables
  loadSpatialData('CountryTable')
  loadSpatialData('StateTable')
  loadSpatialData('TimezoneTable')
}

