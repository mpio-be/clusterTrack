% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{pesa56511}
\alias{pesa56511}
\title{ARGOS satellite tracking data for an individual Pectoral Sandpiper}
\format{
A data.table with 882 rows and 4 columns:
\describe{
  \item{latitude}{Numeric. Latitude in decimal degrees (WGS84).}
  \item{longitude}{Numeric. Longitude in decimal degrees (WGS84).}
  \item{locationDate}{POSIXct. Timestamp of location fix (UTC).}
  \item{locationClass}{Character. ARGOS location quality class (e.g., B, 0, A).}
}
}
\source{
Kempenaers, Bart, and Mihai Valcu. "Breeding site sampling across the Arctic 
         by individual males of a polygynous shorebird." Nature 541.7638 (2017): 528-531.
}
\usage{
pesa56511
}
\description{
A dataset containing ARGOS locations for an individual male Pectoral Sandpiper
}
\examples{
data(pesa56511)
head(pesa56511)
}
\keyword{datasets}
