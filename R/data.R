

#' as_tdbscan
#'
#' @param x             DT
#' @param coords        default "longitude","latitude"
#' @param crs          default 4326.

#' @note
#' For now just a thin wrapper on st_as_sf
#'
#' @export
#' @md
#'
#' @examples
#' data(zbird)
#' x = as_tdbscan(zbird)

as_tdbscan <- function(x, coords = c("longitude","latitude"),time = "time", crs = 4326) {

  o=  copy(x)  

  setnames(o, time, "timestamp")
  setorder(o, timestamp)

  st_as_sf(o, coords = coords, crs = crs)


  }
