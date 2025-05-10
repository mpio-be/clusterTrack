
#' @export
#' @examples
#' data(zbird)
#' ctdf = as_ctdf(zbird )
#' filter_intersection(ctdf)
#' plot(ctdf, by = 'filter')
#' 
segment_track <- function(ctdf ) {

  # TODO: do not filter if intersection >= deltaT

  if (!inherits(ctdf, "ctdf")) {
    stop("filter_intersection() only works on objects of class 'ctdf'")
  }


  segs = ctdf |>
    track_segments() |>
    st_set_geometry("segment")
  
  ints = st_intersects(segs) 


  setDT(segs)

  segs[, nints := lengths(ints)]

  segs[, nints_run := rleid(nints)]



}