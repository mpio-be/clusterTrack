#' stitch_cluster
#' stitch a cluster
#'
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_segments(ctdf )

#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf) 
#' clust = cluster_segments(ctdf) 
#' map(clust)



stitch_cluster <- function(clust,  overlap_threshold = 0.1) {

  x = dat[!is.na(.segment), .(hull = st_union(location) |> st_convex_hull()), by = .segment]

  go_stitch <- function(i,j){
    ai  = st_area(x[i, hull])  
    aj  = st_area(x[j, hull])
    aij = st_intersection(x[i, hull], x[j, hull]) |> st_area()
    aij = if(length(aij)==0) aij = 0

    overlap_ratio = as.numeric(aij / pmin(ai, aj))
    overlap_ratio >= overlap_threshold
  }

  repeat {
    segs = x$.segment
    if (length(segs) < 2) break
    merged = FALSE
    
    for (k in seq_len(length(segs) - 1) ) {
      if ( go_stitch(k, k + 1) ) {
        
        si = segs[k]
        sj = segs[k + 1]
        clust[.segment == sj, .segment := si]
        merged_hull = clust[.segment == si,st_union(location) |> st_convex_hull()]
        x = rbind(
          x[.segment != sj],
          data.table::data.table(
            .segment = si,
            hull = merged_hull
          )
        )
        merged = TRUE; break
      }
    }
    if (!merged) break
  }




}