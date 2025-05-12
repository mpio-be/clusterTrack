
#' Filter by Segment Intersection
#'
#' Flags segments that are isolated (or isolated within their immediate neighbors).
#'
#' @param ctdf An sf object whose geometries are line‐segments in sequence.
#' @param strict  If TRUE (the default), only segments with both neighbors also isolated are kept.
#'                If FALSE, any isolated segment is kept.
#' #' @param overwrite Logical scalar; if \code{FALSE} (default), new filter flags
#'                      are updated with any existing \code{filter} column values; if \code{TRUE},
#'                      the \code{filter} column is replaced entirely.
#' @return Invisibly returns the input \code{ctdf} object whose \code{filter} was updated by reference.
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2)
#' filter_intersection(ctdf)
#' plot(ctdf, by = 'filter')
#' 
filter_intersection <- function(ctdf, overwrite = FALSE, strict = TRUE) {

  # TODO: do not filter if intersection >= deltaT

  if (!inherits(ctdf, "ctdf")) {
    stop("filter_intersection() only works on objects of class 'ctdf'")
  }


  segs = ctdf |>
        as_ctdf_track() |> 
        st_set_geometry("segment")

  # add one element because segs is missing the first entry.

  ints = st_intersects(segs)
  
  int_list = c(0, ints)
  
  d = as.data.table(ctdf)
  d[, n_int := lengths(int_list)]
  d[, nonint := (n_int == 3)]
  d[, prev_ok := shift(nonint,    1, type = "lag",   fill = FALSE)]
  d[, next_ok := shift(nonint,   -1, type = "lead",  fill = FALSE)]

  if (strict) {
    out = d[, nonint & prev_ok & next_ok]
  } else {
    out = d[, nonint]
  }
  if(!overwrite) {
    out = ctdf$.filter | out
  }
  ctdf[, .filter := out]

}
