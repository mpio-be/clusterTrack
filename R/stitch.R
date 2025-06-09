#' stitch_cluster
#' stitch a cluster
#'
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_segments(ctdf )
#' map(clust)

#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf) 
#' clust = cluster_segments(ctdf)
#' clust = stitch_cluster(clust)
#' map(clust)
#' 
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_segments(ctdf)
#' clust =  stitch_cluster(clust)
#' map(clust)



stitch_cluster <- function(clust,  overlap_threshold = 0) {
  stopifnot(!is.unsorted(clust$timestamp))

  o = clust[cluster>0, .(hull = st_union(location) |> st_convex_hull()), by = cluster] |> setDT()
  
  o[, next_hull := hull[c(2:.N, NA_integer_)] ]
  
  o[, overlap := .st_area_overlap_ratio(hull, next_hull), by = cluster]
  o[.N, overlap:= 0]
  o[, is_overlap := overlap > overlap_threshold]

  o[ , stitch_id := rleid(is_overlap)]
  o[(!is_overlap), stitch_id := NA]
  
  o[, stitch_id := fcoalesce(stitch_id, shift(stitch_id))]

  o[, grp_key := fifelse(
      !is.na(stitch_id),
      paste0("s", stitch_id),  
      paste0("c", cluster)       
  )]

  o[, cluster_stitched := .GRP, by = grp_key]

  o = merge(clust, o[, .(cluster, cluster_stitched)], by = "cluster", all.x = TRUE, sort = FALSE)

  o[, cluster := cluster_stitched]
  o[, cluster_stitched := NULL]

  o[is.na(cluster), cluster := 0]
  o
}