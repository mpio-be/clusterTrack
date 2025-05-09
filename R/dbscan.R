#' Cluster movement tracks using DBSCAN
#'
#' Perform clustering on a `ctdf` object to identify use-sites.
#'
#' @param ctdf A `ctdf` object (from [as_ctdf()]).
#' @param minPts Integer; minimum number of neighbors (including self) to form a core point. Default = 5.
#' @param borderPoints Logical; if `FALSE`, treat all border points as noise (DBSCAN*). Default = TRUE.
#' @param ... Passed to **dbscan::dbscan()**, currently unused.
#'
#' @return The input `ctdf`, invisibly, with a new integer column `cluster_id` of cluster labels (0 = noise).
#'
#' @export
#' @examples 
#' library(clusterTrack)
#' data(zbird)
#' ctdf = as_ctdf(zbird, time= 'locationDate')
#' 
#' filter_intersection(ctdf, overwrite = TRUE )
#' prune_delaunay_edges(ctdf, 0.5, 0.5)
#' cluster_track(ctdf)
#' plot(ctdf, by = 'cluster')
#' 
cluster_track <- function(ctdf, minPts = 5, borderPoints = TRUE, ...) {

  frnn = list(id = ctdf$neighbors, eps = 0)   # eps is ignored when id is supplied
  class(frnn) = c("frNN","NN")

  o = dbscan::dbscan(x = frnn, minPts = minPts, borderPoints = borderPoints, ...)

  set(ctdf, j = "cluster_id", value = o$cluster)


  # sanity check & message
  ncl = nrow(ctdf[cluster_id>0, .N, cluster_id])
  if (ncl == 0) {
    warning("No clusters found!")
  } else {
    message(sprintf("%d cluster%s detected!", ncl, if (ncl > 1L) "s" else ""))
  }



}