#' @export
as_ctdf <- function(x, ...) {
  UseMethod("as_ctdf")
}

#' @export
as_ctdf.default <- function(x, ...) {
  stop("No method for objects of class ", class(x))
}

#' @export
plot.ctdf <- function(x, by = c("filter"), pch = 16) {
  
  colpal = function(n) {
    terrain.colors(n)[seq_len(n)]
  }

  tr = track_segments(x)

  tr2 = st_set_geometry(tr, "segment")

  plot(st_geometry(tr2), col = "#706b6b")

  if (missing(by)) {
    plot(st_geometry(tr), pch = pch, add = TRUE)
  }

  if (by == "filter") {
    tr$.filter = factor(tr$.filter)
    plot(tr[".filter"], pal = colpal, pch = pch, add = TRUE)
  }

  plot(x[1, .(location)] |> st_as_sf(), col = "red", size = 3, pch = 16, add = TRUE)
}

#' Coerce an object to clusterTrack data format
#'
#' Converts an object with spatial coordinates and a timestamp column
#' to a standardized `sf/data.table`-based format used internally by the clusterTrack package.
#'
#' @param x       A `data.table` object.
#' @param coords  Character vector of length 2 specifying the coordinate column names.
#'                Defaults to `c("longitude", "latitude")`.
#' @param time    Name of the time column. Will be renamed to `"timestamp"` internally.
#' @param crs     Coordinate reference system. Default is NA. Upstream methods will warn when unprojected
#'                coordinates found (#TODO)
#' @param project_to  passed to `st_transform()`. 
#'                
#' @param ...     Currently unused
#' 

#' @return An object of class `ctdf`, which inherits from `sf`.
#'
#' @note
#' This is currently a thin wrapper around `st_as_sf()`, but standardizes timestamp naming, ordering,
#' and geometry column name (`"location"`). A colum `filter`, and with all values set to `FALSE` is added as well.
#' This column will be updated by upstream methods.
#' 
#' @examples
#' data(zbird)
#' x = as_ctdf(zbird)
#' plot(x)
#'
#' @export

as_ctdf <- function(x, coords = c("longitude","latitude"),time = "time", crs = NA, project_to, ...) {

  reserved = intersect(names(x), c(".filter", ".id"))
  
  if (length(reserved) > 0) {
    warning(
      sprintf(
        "as_ctdf(): input contains reserved column name%s: %s which may be overwritten here or by upstream methods.",
        if (length(reserved) > 1) "s" else "",
        paste(reserved, collapse = ", ")
      )
    )
  }

  o = copy(x)  
  setnames(o, time, "timestamp")
  setorder(o, timestamp)
  
  o[, .id := .I]
  o[, .filter := FALSE]

  o = st_as_sf(o, coords = coords, crs = crs)

  if(!missing(project_to)) {
    o = st_transform(o, project_to)
  }  

  st_geometry(o) = "location"

  setDT(o)


  class(o) <- c("ctdf", class(o))
  o


  }
