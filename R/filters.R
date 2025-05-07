
#' Top-Down Noise Pruning for Tracks
#'
#' A generic wrapper which flags segments by cutting out
#'
#' @param x   An object of class **ctdf** (from [as_ctdf()])
#' @param method    Character; which pruning strategy to apply. One of:
#'                  - `"intersection"` : remove non-intersecting segments (and optionally their neighbors)  
#'                  - `"speed"`        : a simple speed filter. 
#' @param ...       Additional arguments passed to the chosen methods:
#'
#' @return A  logical vector the same length as x.
#'
#' @details  
#' This function implements a **top-down** noise-pruning step, splitting
#' away low-density or isolated portions of your track graph before any
#' downstream clustering.  
#'
#' @seealso  
#' - [as_ctdf()]       for converting `data.table` → `ctdf`  
#' - [track_segments()] for building an `sf` of segments from a `ctdf`  
#' - `fsr_intersection()`,  
#'
#' @examples
#' library(clusterTrack)
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time="locationDate")
#'
#' 
#' @export
filter_ctdf <- function(ctdf,method = c("intersection", "density_gap"),...) {
  
    if (!inherits(ctdf, "ctdf")) {
    stop("filter_sparse_regions() only works on objects of class 'ctdf'")
  }

  
  method <- match.arg(method)
  switch(
    method,
    intersection   = fsr_intersection(ctdf, ...),
    density_gap    = fsr_speed(ctdf, ...),
    stop("Unknown method: ", method)
  )

  # TODO: it should be by reference and update a filter column in the ctdf


}



#' Top-Down Noise Pruning by Segment Intersection
#'
#' Marks segments that are isolated (or isolated within their immediate neighbors).
#'
#' @param ctdf An sf object whose geometries are line‐segments in sequence.
#' @param strict  If TRUE, only segments with both neighbors also isolated are kept.
#'                If FALSE, any isolated segment is kept.
#' @return A  logical vector the same length as ctdf.
#' @export
#' @examples
#' library(clusterTrack)
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time="locationDate")
#' ctdf$filter = fsr_intersection(ctdf, strict = FALSE)
#' plot(ctdf["filter"])
#'
fsr_intersection <- function(ctdf, strict = TRUE) {


  segs = ctdf |>
        track_segments() |> 
        st_set_geometry("segment")

  # add one element because segs is missing the first entry.
  int_list = c(0, st_intersects(segs))
  
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
  
  out
}
