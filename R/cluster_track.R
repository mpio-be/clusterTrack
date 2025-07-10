
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
#' Performs spatiotemporal clustering on a ctdf by segmenting movement, identifying stops, and applying DBSCAN-like clustering.
#'
#'
#' This is a high-level wrapper function that applies a pipeline of segmentation, clustering, and stitching steps on a movement track stored in a `ctdf` object.
#'
#'
#' @param ctdf   A `ctdf` data frame (see [as_ctdf()]) representing a single movement track .
#' @param deltaT Numeric; maximum allowable gap (in days) between segment endpoints to consider them continuous. 
#'               Default to 1 day. Passed to [slice()]) .
#' @param nmin   Integer. Segments or tessellations with fewer than nmin points yield no clusters.
#'               Default to 3. Passed to [cluster_segments()].
#' @param threshold Numeric. If `method = "sd"`, interpreted as the probability for
#'                  the area quantile used in pruning. Passed to [cluster_segments()].
#'                  If `method = "quantile"`, interpreted as the multiplier of the standard deviation
#'                  on log‐areas used in pruning. Passed to [cluster_segments()].
#' @param method Character, one of `"sd"` or `"quantile"`. Determines the pruning
#'               strategy in the tessellation step:  
#'               - `"sd"`: prune cells with area < quantile(polygon_area, probs = threshold)  
#'               - `"quantile"`: prune cells with log(area) ≤ mean(log(area)) + threshold * sd(log(area)).
#'               Passed to [cluster_segments()].
#' @param time_contiguity Logical; if `TRUE`, missing cluster IDs are forward‐filled.
#'                        and backward‐filled within each segment to enforce temporal continuity. 
#'                        Default to `FALSE`.
#'                        Passed to [cluster_segments()].
#' #' @param overlap_threshold Numeric between 0 and 1; minimum area‐overlap ratio
#'                          required to merge adjacent clusters. Default to 0.1.
#'                          Clusters with overlap > threshold are combined.
#'                          Passed to [stitch_cluster()]


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


cluster_track <- function(ctdf,deltaT = 1, nmin = 3, threshold = 0.90, method = "quantile", time_contiguity = FALSE, overlap_threshold = 0.1) {

  ctdf |>
  slice_ctdf(deltaT = deltaT) |>
  cluster_segments(nmin = nmin, threshold = threshold, method = method, time_contiguity = time_contiguity) |>
  stitch_cluster(overlap_threshold = overlap_threshold)

  ctdf # otherwise we get a NULL return by assignment

}