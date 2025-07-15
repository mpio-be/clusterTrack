#' Cluster segments of a ctdf  
#'
#' Compute cluster IDs for each point in a `ctdf` by segment, using Dirichlet
#' tessellation followed by area-based pruning, spatial adjacency and graph-based clustering.
#' Optionally enforce temporal contiguity of clusters within each segment.
#'
#' @param ctdf A `ctdf` object, must contain a `.segment` column; see [slice_ctdf()],.
#' @param nmin Integer. Segments or tessellations with fewer than nmin points yield no clusters.
#'   Default to 3.
#' @param threshold Numeric. If `method = "sd"`, interpreted as the probability for
#'   the area quantile used in pruning.
#'   If `method = "quantile"`, interpreted as the multiplier of the standard deviation
#'   on log‐areas used in pruning.
#' @param method Character, one of `"sd"` or `"quantile"`. Determines the pruning
#'   strategy in the tessellation step:  
#'   - `"sd"`: prune cells with area < quantile(polygon_area, probs = threshold)  
#'   - `"quantile"`: prune cells with log(area) ≤ mean(log(area)) + threshold * sd(log(area))
#' @param time_contiguity Logical; if `TRUE`, missing cluster IDs are forward‐filled
#'   and backward‐filled within each segment to enforce temporal continuity. Default to `FALSE`.
#' @param progress_bar Logical; whether to display a progress bar during execution. Defaults to `TRUE`.
#'
#' @return Invisibly returns `NULL`. The input `ctdf` is modified by reference,
#'  updating the column `cluster`; `0` indicates unclustered points.
#'
#' @details For each unique segment in `ctdf`, points are tessellated via
#'   [tessellate_ctdf()], pruned via [prune_tesselation()], and adjacency
#'   neighborhoods are computed with `poly2nb()`. These neighborhoods are
#'   converted to an undirected graph, and clusters are identified as connected
#'   components.  
#'
#' @export
#' @examples
#' data(toy_ctdf_k3)
#' ctdf = as_ctdf(toy_ctdf_k3, s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf )
#'
#' 
cluster_segments <- function(ctdf, nmin = 3, threshold = 0.9, method = "quantile", 
                            time_contiguity = FALSE, progress_bar = TRUE) {

  s = ctdf[!is.na(.segment)]
  s[, n := .N, .segment]


  segs = s$.segment |> unique()
  
  if (progress_bar) {
    pb = txtProgressBar(min = min(segs), max = max(segs), style = 1, char = "░")
  }
  o = foreach(si = segs) %do% {
    if (progress_bar) {
      setTxtProgressBar(pb, si)
    }

    x = s[.segment == si]
    oi = cluster_tessellation(x, nmin = nmin, threshold = threshold, method = method)
    oi = oi[cluster > 0]
    oi[, .segment := si]
    oi
  }

  if (progress_bar) {
    close(pb)
  }

  o = rbindlist(o)
  o[, cluster := .GRP, by = .(.segment, cluster) ]
  o[, cluster := as.integer(cluster)]


  o = merge(ctdf[, .(.id)], o[, .(.id, cluster)], by = ".id",  all.x = TRUE, sort = FALSE)

  if(time_contiguity) {
    o[, cluster := {
      f = nafill(cluster,    type = "locf")   
      b = nafill(cluster,    type = "nocb")   
      fifelse(f == b,f, cluster)                        
    }]
  }

  o[is.na(cluster), cluster := 0]

  set(ctdf, j = "cluster", value = o$cluster)
  

}