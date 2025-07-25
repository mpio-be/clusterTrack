% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{lbdo66862}
\alias{lbdo66862}
\title{ARGOS satellite tracking data for an individual Long-billed dowitcher}
\format{
A data.table with 2501 rows and 4 columns:
\describe{
  \item{latitude}{Numeric. Latitude in decimal degrees (WGS84).}
  \item{longitude}{Numeric. Longitude in decimal degrees (WGS84).}
  \item{locationDate}{POSIXct. Timestamp of location fix (UTC).}
  \item{locationClass}{Character. ARGOS location quality class (e.g., B, 0, A).}
}
}
\source{
Kwon, Eunbi, Mihai Valcu, and Bart Kempenaers. “Strong Wintering Site Fidelity Contrasts 
         with Exploratory Breeding Site Sampling in a Socially Monogamous Shorebird.” 
         Movement Ecology, vol. 13, no. 1, 2025, p. 49, https://doi.org/10.1186/s40462-025-00580-3.
}
\usage{
lbdo66862
}
\description{
A dataset containing ARGOS locations for an individual male Long-billed dowitcher.
}
\examples{
data(lbdo66862)
head(lbdo66862)
}
\keyword{datasets}
