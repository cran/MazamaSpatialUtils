#' @keywords datagen
#' @export
#'
#' @title Subset pre-formatted HUC files into smaller groupings.
#'
#' @param SPDF a spatial polygons dataframe created using the convertUSGSHUC
#' function
#' @param parentHUCs Character vector specifying one or more containing HUCs.
#' @param stateCodes Character vector specifying one or more containing states.
#' @param allStateCodes Similar to stateCode, but will also include HUCs who
#' touch the state but whose centroid is in a different state.
#'
#' @description A SpatialPolygons Dataframe is broken into smaller pieces based
#' on HUC code or state. The SpatialPolygons Dataframe must have the required
#' fields 'stateCode', 'HUC', and 'allStateCodes' and is intended to come from
#' the \code{convertUSGSHUC()} function. The difference between stateCode and
#' allStateCodes is that stateCode has just one two-digit ISO code while
#' allStateCodes can have more than one. This allows the subset to include
#' HUCs where part of the watershed is in the specified state even though the
#' centroid is in a different state.
#'
#' @return a SpatialPolygons Dataframe subsetted to the appropriate specifications.
#'

subsetHUC <- function(
  SPDF = NULL,
  parentHUCs = NULL,
  stateCodes = NULL,
  allStateCodes = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(SPDF)

  # Check that names of SPDF have required fields
  requiredFields <- c('stateCode', 'HUC', 'allStateCodes')
  missingFields <- setdiff(requiredFields, names(SPDF))
  if ( length(missingFields) > 0 ) {
    stop(paste0('Missing fields in SPDF: ', missingFields))
  }

  # ----- Subset ---------------------------------------------------------------

  # Identify HUC string partial matches to use as a mask
  if ( !is.null(parentHUCs) ) {
    HUCMask <- rep(FALSE, nrow(SPDF))
    for (HUC in parentHUCs){
      regex <- paste0('^', HUC)
      mask <- stringr::str_detect(SPDF@data$HUC, regex)
      HUCMask <- HUCMask | mask
    }
    SPDF <- SPDF[HUCMask,]
  }

  # Subset HUC by stateCode
  if ( !is.null(stateCodes) ) {
    stateMask <- rep(FALSE, nrow(SPDF))
    for (stateCode in stateCodes) {
      stateMask <- stateMask | (SPDF@data$stateCode == stateCode)
    }
    stateMask <- stateMask & !is.na(stateMask)
    SPDF <- SPDF[stateMask,]
  }

  # SubsetHUC by allstateCodes
  if ( !is.null(allStateCodes) ) {
    allStateCodesMask <- rep(FALSE, nrow(SPDF))
    for (stateCode in allStateCodes) {
      allStateCodesMask <- allStateCodesMask | stringr::str_detect(SPDF@data$allStateCodes, stateCode)
      # Handle NAs in allStateCodes (e.g. Northerm Mariana Islands in HUC4)
      allStateCodesMask[is.na(allStateCodesMask)] <- FALSE
    }
    SPDF <- SPDF[allStateCodesMask,]
  }

  # ----- Return ---------------------------------------------------------------

  return(SPDF)

}

