# library(stringr)

context("environment dataDir")

test_that("setSpatialDataDir and getSpatialDataDir work correctly", {
  setSpatialDataDir("~")
  expect_equal(path.expand("~"), getSpatialDataDir())
  setSpatialDataDir(getwd())
  expect_equal(getwd(), getSpatialDataDir())
})



# installSpatialData('TMWorldBordersSimple')
# loadSpatialData('TMWorldBordersSimple')
# installSpatialData('NaturalEarthAdm1')
# loadSpatialData('NaturalEarthAdm1')
# installSpatialData('USCensusCounties')
# loadSpatialData('USCensusCounties')
# installSpatialData('WorldTimezones')
# loadSpatialData('WorldTimezones')
# 
# context("'convert' functions")
# 
# test_that("convertTMWorldBordersSimple downloads the correct data", {
#   convertTMWorldBordersSimple()
#   exists <- file.exists(paste0(getSpatialDataDir(), "/TMWorldBordersSimple.RData"))
#   expect_equal(exists, TRUE)
# })
# 
# test_that("convertNaturalEarthAdm1 downloads the correct data", {
#   convertNaturalEarthAdm1()
#   exists =  file.exists(paste0(getSpatialDataDir(), "/NaturalEarthAdm1.RData"))
#   expect_equal(exists, TRUE)
# })
# 
# test_that("convertUSCensusCounties downloads the correct data", {
#   convertUSCensusCounties()
#   exists <- file.exists(paste0(getSpatialDataDir(), "/USCensusCounties.RData"))
#   expect_equal(exists, TRUE)
# })
# 
# test_that("convertWorldTimezones downloads the correct data", {
#   convertWorldTimezones()
#   exists <- file.exists(paste0(getSpatialDataDir(), "/WorldTimezones.RData"))
#   expect_equal(exists, TRUE)
# })
# 
# 
# 
# context("loadSpatialData tests")
# 
# test_that("errors are handled correctly", {
#   expect_error(loadSpatialData())
#   expect_error(loadSpatialData("notafile12345"))
# })
# 
# test_that("correct object names are loaded in environment", {
#   expect_match(loadSpatialData("TMWorldBordersSimple"), "TMWorldBordersSimple")
#   expect_match(loadSpatialData("NaturalEarthAdm1"), "NaturalEarthAdm1")
#   expect_match(loadSpatialData("WorldTimezones"), "WorldTimezones")
#   expect_match(loadSpatialData("USCensusCounties"), "USCensusCounties")
# })
# 
# test_that("objects are of class SpatialPolygonsDataFrame", {
#   expect_is(TMWorldBordersSimple, "SpatialPolygonsDataFrame")
#   expect_is(NaturalEarthAdm1, "SpatialPolygonsDataFrame")
#   expect_is(WorldTimezones, "SpatialPolygonsDataFrame")
#   expect_is(USCensusCounties, "SpatialPolygonsDataFrame")
# })
# 
# test_that("each spDF has columns labels that follow Mazama Science standards", {
#   expect_equal(c("countryCode", "longitude", "latitude") %in% colnames(TMWorldBordersSimple@data),
#                c(TRUE,TRUE,TRUE))
#   expect_equal(c("countryCode", "stateName", "stateCode") %in% colnames(NaturalEarthAdm1@data),
#                c(TRUE,TRUE,TRUE))
#   expect_equal(c("timezone", "UTC_offset", "countryCode", "longitude", "latitude") %in% colnames(WorldTimezones@data),
#                c(TRUE,TRUE,TRUE,TRUE,TRUE))
#   expect_equal(c("stateCode", "stateName", "countyName") %in% colnames(USCensusCounties@data),
#                c(TRUE,TRUE,TRUE))
# })



context("'get' function test")

test_that("get functions handle errors correctly", {
  
  expect_error(getCountryCode(), 
               'argument "lon" is missing, with no default')
  expect_error(getCountryCode("a", "b"), 
               'cannot retrieve coordinates from non-numeric elements')
  expect_warning(getCountryCode(0,0), 
                 "1 locations appear to be over international waters and no countryCode can be assigned")
  expect_warning(getCountryCode(c(0,0), c(0,45)), 
                 "1 locations appear to be over international waters and no countryCode can be assigned")
  expect_warning(getCountryCode(c(0,0), c(0,100)), 
                 "2 locations appear to be over international waters and no countryCode can be assigned")
  
})

test_that("get functions return correct name", {
  
  expect_match(getCountryCode(2, 47), "FR")
  expect_match(getCountryCode(-80, 40), "US")  
  expect_match(getCountryCode(c(120,-17), c(-1.5,15)), "[IN SN]")
  
#   expect_match(getStateCode(2, 47), "IN")
#   expect_match(getStateCode(-80, 40), "PA")  
#   expect_match(getStateCode(c(120,-17), c(-1.5,15)), "[ST TH]")
  
  expect_match(getTimezone(2, 47), "Europe/Paris")
  expect_match(getTimezone(-80, 40), "America/New_York")  
  expect_match(getTimezone(c(120,-17), c(-1.5,15)), "[Asia/Makassar Africa/Dakar]")
  
#   expect_match(getUSCounty(-122, 47), "Pierce")
#   expect_match(getUSCounty(-80, 40), "Washington")  
#   expect_match(getUSCounty(c(-100,-100), c(40,45)), "[Norton Potter]")
  
})

test_that("subsetting with countryCodes works", {
  
  expect_match(getCountryCode(2, 47), "FR")
  expect_match(getCountryCode(2, 47, countryCodes=c("FR")), "FR")
  expect_match(getCountryCode(2, 47, countryCodes="FR"), "FR")
  expect_warning(getCountryCode(2, 47, countryCodes=c("US")), 
                 "1 locations appear to be either over international waters or not in given countryCodes and no countryCode can be assigned")
  
#   expect_match(getStateCode(2, 47), "IN")
#   expect_match(getStateCode(2, 47, countryCodes=c("FR")), "IN")
#   expect_match(getStateCode(2, 47, countryCodes="FR"), "IN")
#   expect_warning(getStateCode(2, 47, countryCodes=c("US")), 
#                  "1 locations appear to be either over international waters or not in given countryCodes and no stateCode can be assigned")
  
  expect_match(getTimezone(2, 47), "Europe/Paris")
  expect_match(getTimezone(2, 47, countryCodes=c("FR")), "Europe/Paris")
  expect_match(getTimezone(2, 47, countryCodes="FR"), "Europe/Paris")
  expect_warning(getTimezone(2, 47, countryCodes=c("US")), 
                 "1 locations appear to be either over international waters or not in given countryCodes and no timezone can be assigned")
  
#   expect_match(getUSCounty(-122, 47), "Pierce")
#   expect_match(getUSCounty(-122, 47, stateCodes=c("WA")), "Pierce")
#   expect_match(getUSCounty(-122, 47, stateCodes="WA"), "Pierce")
#   expect_warning(getUSCounty(-122, 47, stateCodes=c("OR")), 
#                  "1 locations appear to be either over international waters or not in given stateCodes and no county can be assigned")
  
})

test_that("allData returns are correct dimension and type", {
  
  expect_is(getCountryCode(2, 47, allData=TRUE), "data.frame")
  expect_equal(dim(getCountryCode(2, 47, allData=TRUE)), c(1,7))
  expect_is(getCountryCode(c(120,-17), c(-1.5,15), allData=TRUE), "data.frame")
  expect_equal(dim(getCountryCode(c(120,-17), c(-1.5,15), allData=TRUE)), c(2,7))
  
#   expect_is(getStateCode(2, 47, allData=TRUE), "data.frame")
#   expect_equal(dim(getStateCode(2, 47, allData=TRUE)), c(1,52))
#   expect_is(getStateCode(c(120,-17), c(-1.5,15), allData=TRUE), "data.frame")
#   expect_equal(dim(getStateCode(c(120,-17), c(-1.5,15), allData=TRUE)), c(2,52))
  
  expect_is(getTimezone(2, 47, allData=TRUE), "data.frame")
  expect_equal(dim(getTimezone(2, 47, allData=TRUE)), c(1,6))
  expect_is(getTimezone(c(120,-17), c(-1.5,15), allData=TRUE), "data.frame")
  expect_equal(dim(getTimezone(c(120,-17), c(-1.5,15), allData=TRUE)), c(2,6))
  
#   expect_is(getUSCounty(-122, 47, allData=TRUE), "data.frame")
#   expect_equal(dim(getUSCounty(-122, 47, allData=TRUE)), c(1,12))
#   expect_is(getUSCounty(c(-100,-100), c(40,45), allData=TRUE), "data.frame")
#   expect_equal(dim(getUSCounty(c(-100,-100), c(40,45), allData=TRUE)), c(2,12))
  
})
