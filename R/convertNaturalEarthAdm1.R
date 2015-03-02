#' @keywords datagen
#' @export
#' @title Convert Level 1 (State) Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a SpatialPolygonsDataFrame for a 1st level administrative divisions
#' @details A state border shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the package \code{SpatialDataDir} which can be set with \code{setSpatialDataDir()}.
#' 
#' Within the \pkg{MazamaSpatialUtils} package the phrase 'state' refers to administrative
#' divisions beneath the level of the country or nation. This makes sense in the United 'States'. In
#' other countries this level is known as 'province', 'territory' or some other term.
#' @return Name of the dataset being created.
#' @references \url{http//www.naturalearthdata.com/download}
#' @references \url{http://www.statoids.com/ihasc.html}
#' @seealso setSpatialDataDir
#' @seealso getState, getStateCode
convertNaturalEarthAdm1 <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'NaturalEarthAdm1'
    
  if (nameOnly) return(datasetName)
  
  # Specify administrative levels for URL
  adm <- 1
  level <- "states_provinces"

  # Build appropriate request URL for Natural Earth data
  url <- paste0("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_",
                adm, "_",
                level, ".zip")
  
  filePath <- paste(dataDir,basename(url),sep='/')
  download.file(url,filePath)
  unzip(filePath,exdir=paste0(dataDir, "/adm"))
    
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste(dataDir,'adm',sep='/')
  shpName <- paste("ne", "10m_admin", adm, level, sep="_")
  spDF <- convertLayer(dsn=dsnPath,layerName=shpName)

  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  #   > names(spDF@data)
  #   [1] "adm1_code"  "OBJECTID_1" "diss_me"    "adm1_cod_1" "iso_3166_2" "wikipedia"  "iso_a2"     "adm0_sr"    "name"      
  #   [10] "name_alt"   "name_local" "type"       "type_en"    "code_local" "code_hasc"  "note"       "hasc_maybe" "region"    
  #   [19] "region_cod" "provnum_ne" "gadm_level" "check_me"   "scalerank"  "datarank"   "abbrev"     "postal"     "area_sqkm" 
  #   [28] "sameascity" "labelrank"  "featurecla" "name_len"   "mapcolor9"  "mapcolor13" "fips"       "fips_alt"   "woe_id"    
  #   [37] "woe_label"  "woe_name"   "latitude"   "longitude"  "sov_a3"     "adm0_a3"    "adm0_label" "admin"      "geonunit"  
  #   [46] "gu_a3"      "gn_id"      "gn_name"    "gns_id"     "gns_name"   "gn_level"   "gn_region"  "gn_a1_code" "region_sub"
  #   [55] "sub_code"   "gns_level"  "gns_lang"   "gns_adm1"   "gns_region"
  
  
  # NOTE:  Lots of useful potentially useful information here. At this point we will just add the core identifiers
  spDF$countryCode <- spDF$iso_a2
  ### countryCode <- str_split_fixed(spDF$code_hasc,'\\.',5)[,1] # alternative way to get countryCode
  spDF$stateCode <- str_split_fixed(spDF$code_hasc,'\\.',5)[,2]
  spDF$countryName <- MazamaSpatialUtils::codeToCountry(spDF$countryCode)
  spDF$stateName <- spDF$name
  
  # Rationalize units:
  # * SI  
  spDF$area <- spDF$area_sqkm * 1e6  

  # Use NA instead of -99 and -90 for missing values
  spDF@data[spDF@data == -99] <- NA
  spDF@data[spDF@data == -90] <- NA
  
  # Subset this dataframe to include only obviously useful columns
  usefulColumns <- c('countryCode','countryName','stateCode','stateName','latitude','longitude','area',
                     'postal','code_hasc','fips','gns_lang','gns_adm1')
  # Remove columns that are more than 80% NA
  spDF <- spDF[,usefulColumns]
  
  # Assign a name and save the data
  assign(datasetName,spDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  invisible(datasetName)
}

