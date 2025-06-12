

#' Cluster movement tracks
#'
#'
#' @param ctdf A `ctdf`.
#' @return NULL.  The `ctdf` object is updated in place. 
#'
#'
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf )
#' map(ctdf)
#'
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' cluster_segments(ctdf)
#' map(ctdf)
#'
#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf) 
#' cluster_segments(ctdf) 
#' map(ctdf)
#' 
cluster_segments <- function(ctdf, threshold = 0.75, method = "quantile") {

  s = ctdf[!is.na(.segment)]
  s[, n := .N, .segment]


  segs = s$.segment |> unique()
  
  pb = txtProgressBar(min =  min(segs), max = max(segs), style = 1, char = "░")
  o = foreach(si = segs) %do% {
    setTxtProgressBar(pb, si)

    x = s[.segment == si]
    oi = cluster_tessellation(x, threshold = threshold, method = method)
    oi = oi[cluster > 0]
    oi[, .segment := si]
    oi
  }
  close(pb)

  o = rbindlist(o)
  o[, cluster := .GRP, by = .(.segment, cluster) ]
  o[, cluster := as.integer(cluster)]


  o = merge(ctdf[, .(.id)], o[, .(.id, cluster)], by = ".id",  all.x = TRUE, sort = FALSE)

  o[, cluster := {
    f = nafill(cluster,    type = "locf")   
    b = nafill(cluster,    type = "nocb")   
    fifelse(f == b,f, cluster)                        
  }]

  o[is.na(cluster), cluster := 0]

  set(ctdf, j = "cluster", value = o$cluster)
  

}