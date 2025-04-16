#' @export
as_ctdf <- function(x, ...) {
  UseMethod("as_ctdf")
}

#' @export
as_ctdf.default <- function(x, ...) {
  stop("No method for objects of class ", class(x))
}

#' @export
plot.ctdf <- function(x, ..., col = "black", pch = 16) {
  plot(sf::st_geometry(x), col = col, pch = pch, ...)
}

#' Coerce a data.table to clusterTrack data format
#'
#' Converts a `data.table` with spatial coordinates and a timestamp column
#' to a standardized `sf`-based format used internally by the clusterTrack package.
#'
#' @param x       A `data.table` object.
#' @param coords  Character vector of length 2 specifying the coordinate column names.
#'                Defaults to `c("longitude", "latitude")`.
#' @param time    Name of the time column. Will be renamed to `"timestamp"` internally.
#' @param crs     Coordinate reference system. Default is EPSG 4326.
#' @param ...     Currently unused
#'
#' @return An object of class `ctdf`, which inherits from `sf`.
#'
#' @note
#' This is currently a thin wrapper around `st_as_sf()`, but standardizes timestamp naming and ordering.
#'
#' @examples
#' data(zbird)
#' x <- as_ctdf(zbird)
#'
#' @export

as_ctdf <- function(x, coords = c("longitude","latitude"),time = "time", crs = 4326, ...) {

  o=  copy(x)  

  setnames(o, time, "timestamp")
  setorder(o, timestamp)

  o_sf = st_as_sf(o, coords = coords, crs = crs)

  class(o_sf) <- c("ctdf", class(o_sf))
  o_sf


  }
