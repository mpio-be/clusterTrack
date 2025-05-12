
#' segment ctdf
#' segment_ctdf

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2 )
#' filter_intersection(ctdf)
#' plot(ctdf, by = 'filter')
#' 
segment_ctdf <- function(ctdf) {


  if (!inherits(ctdf, "ctdf")) {
    stop("filter_intersection() only works on objects of class 'ctdf'")
  }


  segs = ctdf |>
    as_ctdf_track() |>
    st_set_geometry("segment")
  
  ints = st_intersects(segs) 


  setDT(segs)

  segs[, nints := lengths(ints)]

  segs[, nints_run := rleid(nints)]



}