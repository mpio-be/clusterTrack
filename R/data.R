

#' as_tdbscan
#'
#' @param x             DT
#' @param coords        default "longitude","latitude"
#' @param crs          default 4326.

#' @note 
#' For now just a thin wrapper on st_as_sf

#' @examples
#' data(zbird)
#' x = as_tdbscan(zbird)

as_tdbscan <- function(x, coords = c("longitude","latitude"),time = "time", crs = 4326) {

  setnames(x, time, "timestamp")
  setorder(x, timestamp)  

  st_as_sf(x, coords = coords, crs = crs)


  }