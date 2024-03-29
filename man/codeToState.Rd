% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MazamaSpatialUtils.R
\name{codeToState}
\alias{codeToState}
\title{Convert state codes to state nnames}
\usage{
codeToState(stateCodes, countryCodes = NULL, dataset = "NaturalEarthAdm1")
}
\arguments{
\item{stateCodes}{Vector of state codes.}

\item{countryCodes}{Vector of ISO-3166-1 alpha-2 country codes the state might be found in.}

\item{dataset}{Name of dataset containing state-level identifiers.}
}
\value{
A vector of English state names or NA.
}
\description{
Converts a vector of ISO 3166-2 alpha-2 state codes to the
corresponding English names.
}
\details{
For this function to work, you must install and load the
"NaturalEarthAdm1" dataset.
}
\seealso{
convertNaturalEarthAdm1
}
