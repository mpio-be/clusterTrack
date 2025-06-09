

#' Cluster movement tracks
#'
#' Perform clustering on a `ctdf` object to identify use-sites. Uses a hybrid approach combining Dirichlet-based polygon pruning and DBSCAN clustering to detect spatially and temporally coherent movement clusters ("use-sites").
#'
#' @param ctdf A `ctdf` object (from [as_ctdf()]), representing sequential movement points with timestamps and locations.
#' @return A `ctdf` object containing only points assigned to clusters, with an additional integer column `cluster` indicating the cluster ID for each point (consecutive values starting from 1). Cluster membership reflects spatial-temporal groupings of points.
#'
#'
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_segments(ctdf )
#' map(clust)
#'
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_segments(ctdf)
#' map(clust)
#'
#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf) 
#' clust = cluster_segments(ctdf) 
#' map(clust)
#' 
cluster_segments <- function(ctdf) {

  s = ctdf[!is.na(.segment)]
  s[, n := .N, .segment]


  segs = s$.segment |> unique()
  
  pb = txtProgressBar(min =  min(segs), max = max(segs), style = 1, char = "█")
  o = foreach(si = segs) %do% {
    setTxtProgressBar(pb, si)

    x = s[.segment == si]
    oi = cluster_tessellation(x, threshold = 0.75, method = "quantile")
    oi = oi[cluster > 0]
    oi[, .segment := si]
    oi
  }
  close(pb)

  o = rbindlist(o)
  o[, cluster := .GRP, by = .(.segment, cluster) ]
  o[, cluster := as.integer(cluster)]


  o = merge(ctdf, o[, .(.id, cluster)], by = ".id", suffixes = c("", "temp"), all.x = TRUE, sort = FALSE)

  class(o) <- c("clusterTrack", class(o))

  o[is.na(cluster), cluster := 0]
  
  o
  

}