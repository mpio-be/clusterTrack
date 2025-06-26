
#' @export
print.clusterTrack <- function(x, ...) {

  cat("<clusters:", uniqueN(x$cluster)-1, ">\n\n")

  NextMethod("print",
    topn      = 3,
    nrows     = 10,
    print.keys= FALSE,
    ...)


}

#' @export
plot.clusterTrack <- function(x) {
  pal = topo.colors(n = uniqueN(x$cluster))
  cols = pal[match(x$cluster, sort(unique(x$cluster)))]

  plot(st_geometry(x$location), col = cols)
}


#' Cluster movement tracks
#'
#' Performs spatial-temporal clustering on a `ctdf` object by identifying movement segments, then isolating their stop portions of a track and then and grouping them into clusters.
#'
#'
#' This is a high-level wrapper function that applies a pipeline of segmentation, clustering, and stitching steps on a movement track stored in a `ctdf` object.
#'
#'
#' @param ctdf A `ctdf` data frame (see [as_ctdf()]) representing a single movement track, modified by reference.
#' @param deltaT Numeric. Passed to [slice_cdf()] The maximum temporal gap (in days) allowed between intersecting segments. Default is 1 day.
#' @param thereshold Numeric. ...


#' @return NULL. The function modifies `ctdf` by reference, adding or updating the column \code{cluster}, which assigns a cluster ID to each row (point).
#'

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(ctdf)
#' 
#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(ctdf)
#' 
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(ctdf)
#' 
#' data(lbdo66862)
#' ctdf = as_ctdf(lbdo66862, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(ctdf)
#' 
#' 


cluster_track <- function(ctdf,deltaT = 1, threshold = 0.75, method = "quantile", overlap_threshold = 0) {

  ctdf |>
  slice_ctdf(deltaT = deltaT) |>
  cluster_segments(threshold = threshold, method = method) |>
  stitch_cluster(overlap_threshold = overlap_threshold)

  ctdf # otherwise we get a NULL return by assignment

}