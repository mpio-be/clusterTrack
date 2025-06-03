
#' @rdname cluster_track
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

dbscan_tessellation <- function(x, nmin = 5) {
 
  tess = tessellate_ctdf(x) |>
    prune_tesselation(q = 0.95)

  nb = poly2nb(tess, queen = TRUE)

  min_pts = sapply(nb, length) |>
    median() |>
    round()
  if (min_pts < 2) min_pts = min_pts + 1

  if (nrow(tess) >  max(c(5, min_pts))) { # dbscan crashes otherwise

    frnn = list(id = nb, eps = 0) # eps is ignored when id is supplied
    class(frnn) = c("frNN", "NN")

    cl = dbscan::dbscan(
      x            = frnn,
      minPts       = 5,
      borderPoints = FALSE
    )$cluster
  } else {
    cl = 0
  }

  o = mutate(tess, cluster = cl)

  #' print(min_pts)
  #' mapview::mapview(o['cluster'])

  data.table(o)[, .(.id, cluster)]
}



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
#' ctdf = slice_ctdf(ctdf, 24, 2)
#' clust = cluster_track(ctdf )
#' map(clust)
#'
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#'  ctdf = slice_ctdf(ctdf)
#' clust = cluster_track(ctdf)
#' map(clust)
#'
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' ctdf = slice_ctdf(ctdf) 
#' clust = cluster_track(ctdf) 
#' map(clust)
#' 
cluster_track <- function(ctdf ) {

  s = ctdf[!is.na(.segment)] 

  segs = s$.segment |> unique()

  handlers(global = TRUE)
  p = progressor(steps = length(segs))

  o = foreach(si = segs, .errorhandling = 'pass') %do% {
    # p()
    x = s[.segment == si]
    print(si)
    oi = dbscan_tessellation(x)
    oi = oi[cluster > 0]
    oi[, cluster := paste(si, cluster)]
    oi
  }

  e = sapply(o, FUN = inherits, what = "error")
  
  if (length(segs[e]) > 0) {
    message("segments ", paste(segs[e], collapse = ", "), " did not return a cluster!")
  }

  o = o[!e]

  o = rbindlist(o)

  o[, cluster := as.factor(cluster) |> as.integer()]

  o = merge(ctdf, o[, .(.id, cluster)], by = ".id", suffixes = c("", "temp"), all.x = TRUE, sort = FALSE)

  class(o) <- c("clusterTrack", class(o))

  o[is.na(cluster), cluster := 0]
  
  return(o)

  # feedback
  ncl = nrow(o[cluster>0, .N, cluster])
  if (ncl == 0) {warning("No clusters found!")} 
  
  message(sprintf("%d cluster%s detected!", ncl, if (ncl > 1L) "s" else ""))
  

  

}