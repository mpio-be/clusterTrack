#' ARGOS satellite tracking data for an individual Pectoral Sandpiper
#'
#' A dataset containing ARGOS locations for an individual male Pectoral Sandpiper
#' tracked during the 2014 breeding season.
#'
#' @format A data.table with 882 rows and 4 columns:
#' \describe{
#'   \item{latitude}{Numeric. Latitude in decimal degrees (WGS84).}
#'   \item{longitude}{Numeric. Longitude in decimal degrees (WGS84).}
#'   \item{locationDate}{POSIXct. Timestamp of location fix (UTC).}
#'   \item{locationClass}{Character. ARGOS location quality class (e.g., B, 0, A).}
#' }
#'
#' @source  Department of Ornithology, Max Planck Institute for Biological Intelligence.
#'
#' @examples
#' data(pesa56511)
#' head(pesa56511)
"pesa56511"
