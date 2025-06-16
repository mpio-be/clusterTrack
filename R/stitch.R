
.stitch <- function(ctdf,  overlap_threshold = 0) {

  o = ctdf[cluster > 0, .(hull = st_union(location) |> st_convex_hull()), by = cluster] |> setDT()
  
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

  o = merge(ctdf[, .(.id, cluster)], o[, .(cluster, cluster_stitched)], by = "cluster", all.x = TRUE, sort = FALSE)
  o[, cluster := cluster_stitched]
  o[, cluster_stitched := NULL]
  o[is.na(cluster), cluster := 0]

  set(ctdf, j = "cluster", value = o$cluster)
}


#' stitch_cluster
#' stitch a cluster
#'
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf)
#' stitch_cluster(ctdf)
#' map(ctdf)

#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf) 
#' cluster_segments(ctdf)
#' stitch_cluster(ctdf)
#' map(ctdf)
#' 
#' data(lbdo66862)
#' ctdf = as_ctdf(lbdo66862, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf)
#' stitch_cluster(ctdf)
#' map(ctdf)



stitch_cluster <- function(ctdf,  overlap_threshold = 0) {
  
  .check_ctdf(ctdf)

    
  repeat {
    n_prev = max(ctdf$cluster)
    .stitch(ctdf, overlap_threshold = overlap_threshold)
    if (max(ctdf$cluster) == n_prev) break
  }

}