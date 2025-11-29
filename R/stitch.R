.stitch <- function(ctdf, overlap_threshold = 0) {
  o = ctdf[
    cluster > 0,
    .(hull = st_union(location) |> st_convex_hull()),
    by = cluster
  ]

  o[, next_hull := hull[c(2:.N, NA_integer_)]]
  o[, overlap := .st_area_overlap_ratio(hull, next_hull), by = cluster]
  o[.N, overlap := 0]
  o[, is_overlap := overlap > overlap_threshold]

  o[, stitch_id := rleid(is_overlap)]
  o[(!is_overlap), stitch_id := NA]

  o[, stitch_id := fcoalesce(stitch_id, shift(stitch_id))]

  o[, grp_key := fifelse(!is.na(stitch_id), paste0("s", stitch_id), paste0("c", cluster))]

  o[, cluster_stitched := .GRP, by = grp_key]

  ctdf[o, on = "cluster", cluster := cluster_stitched]
}

#' Stitch clusters by spatial overlap across segments
#'
#' Iteratively merge spatial clusters in a CTDF based on the area overlap of
#' their convex hulls. Clusters whose hulls overlap above a specified ratio
#' are combined into a single cluster ID.
#'
#' @param ctdf A CTDF object. Must contain an updated `cluster` column.
#' @param overlap_threshold Numeric between 0 and 1; minimum area‐overlap ratio
#'                          required to merge adjacent clusters.
#'                          Clusters with overlap > threshold are combined.
#'#' @return The input CTDF, with an updated (in-place) cluster` column.
#'
#' @export
#' @examples
#' data(toy_ctdf_k3)
#' ctdf = as_ctdf(toy_ctdf_k3, s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf)
#' cluster_stitch(ctdf)

#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf)
#' cluster_stitch(ctdf)
#'
#' data(lbdo66862)
#' ctdf = as_ctdf(lbdo66862, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf)
#' cluster_stitch(ctdf)

cluster_stitch <- function(ctdf, overlap_threshold = 0.1) {
  .check_ctdf(ctdf)

  repeat {
    n_prev = max(ctdf$cluster)
    .stitch(ctdf, overlap_threshold = overlap_threshold)
    if (max(ctdf$cluster) == n_prev) break
  }
}
