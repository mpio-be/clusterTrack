
#' @export
print.clusterTrack <- function(x, ...) {

  cat("<clusters:", uniqueN(x$cluster), ">\n\n")

  NextMethod("print",
    topn      = 3,
    nrows     = 10,
    print.keys= FALSE,
    ...)


}

#' @export
plot.clusterTrack <- function(x ) {
  
  pal = topo.colors(n = uniqueN(x$cluster) )
  cols = pal[match(x$cluster, sort(unique(x$cluster)))]

  plot(st_geometry(x$location), col = cols)

}


#' Cluster movement tracks
#'
#' Perform clustering on a `ctdf` object, identifying spatial-temporal segments and grouping them into clusters.
#'
#' @param ctdf A `ctdf` data frame.
#' @return NULL. The function updates the 'cluster' column of `ctdf` by reference.
#' @details
#' This function is composed of three main components:
#' - \code{slice_ctdf()} segments the track based on spatial and temporal criteria.
#' - \code{cluster_segments()} clusters these segments into preliminary groups.
#' - \code{stitch_cluster()} stitches clusters based on overlap criteria.
#'
#' See individual components for detailed implementation.
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(ctdf)
#' 
#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(deltaT =1)
#' 
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(ctdf, deltaT =1)
#' 
#' data(lbdo66862)
#' ctdf = as_ctdf(lbdo66862, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' cluster_track(ctdf,deltaT =1)
#' 
#' 


cluster_track <- function(ctdf,deltaT = 1, threshold = 0.75, method = "quantile", overlap_threshold = 0) {

  ctdf |>
  slice_ctdf(deltaT = deltaT) |>
  cluster_segments(threshold = threshold, method = method) |>
  stitch_cluster(overlap_threshold = overlap_threshold)

  ctdf # otherwise we get a NULL return by assignment

}