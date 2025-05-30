
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

ct_dbscan_Dirichlet <- function(ctdf, borderPoints = TRUE) {
  nb = prune_dirichlet_polygons(ctdf)

  min_pts = sapply(nb, length) |>
    median() |>
    round()
  if (min_pts < 3) min_pts = 5 # back to default


  frnn = list(id = nb, eps = 0) # eps is ignored when id is supplied
  class(frnn) = c("frNN", "NN")

  o = dbscan::dbscan(x = frnn, minPts = min_pts, borderPoints = borderPoints)

  if (!all(o$cluster == 0)) {
    nam_nb = names(nb) # subset.nb does not work on a named list
    names(nb) = NULL
    nbs = subset(nb, o$cluster > 0)
    nam_nbs = nam_nb[o$cluster > 0]
    names(nbs) = nam_nbs
    clusters = o$cluster[o$cluster > 0]
    o = data.table(.id = names(nbs) |> as.integer(), cluster = clusters)
    o = merge(o, ctdf[, .(.id)])
  } else {
    o = data.table(.id = integer(0), cluster = integer(0))
  }

  o
}

ct_hdbscan <- function(ctdf, borderPoints = TRUE) {
  x = st_coordinates(ctdf$location)
  cl = hdbscan(x, minPts = 5)

  o = data.table(.id = ctdf$.id, cluster = cl$cluster)
  o
}

ct_dtscan <- function(ctdf, borderPoints = TRUE) {


  x = st_coordinates(ctdf$location)
  cl = dtscan::dtscan(x, min_pts = 5, max_closeness = 5000)

  o = data.table(.id = ctdf$.id, cluster = cl)
  o
}






#' Cluster movement tracks
#'
#' Perform clustering on a `ctdf` object to identify use-sites. Uses a hybrid approach combining Dirichlet-based polygon pruning and DBSCAN clustering to detect spatially and temporally coherent movement clusters ("use-sites").
#'
#' @param ctdf A `ctdf` object (from [as_ctdf()]), representing sequential movement points with timestamps and locations.
#' @param borderPoints Logical; if `FALSE`, treat all border points as noise (DBSCAN*). Default = TRUE.
#'
#' @return A `ctdf` object containing only points assigned to clusters, with an additional integer column `cluster` indicating the cluster ID for each point (consecutive values starting from 1). Cluster membership reflects spatial-temporal groupings of points.
#'
#'
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_track(ctdf)
#' clusterTrack.Vis::map(clust)
#'
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_track(ctdf)
#' clusterTrack.Vis::map(clust)
#'
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf, deltaT = 72, slice_method = function(x) quantile(x, 0.99))
#' clust = cluster_track(ctdf)
#' clusterTrack.Vis::map(clust)
#' 
cluster_track <- function(ctdf, borderPoints = TRUE) {

  s = ctdf[ (!.filter)]
  segs = s$.segment |> unique()

  handlers(global = TRUE)
  p = progressor(steps = length(segs))

  o = foreach(si = segs, .errorhandling = 'pass') %do% {
    p()
    x = ctdf[.segment == si & !.filter]
    
    # oi = ct_dbscan_Dirichlet(ctdf = x, borderPoints = borderPoints)
    oi = ct_dtscan(x)
    
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

  # sanity check
  ncl = nrow(o[cluster>0, .N, cluster])
  if (ncl == 0) {
    warning("No clusters found!")
  } else {
    message(sprintf("%d cluster%s detected!", ncl, if (ncl > 1L) "s" else ""))
  }

  o = merge(ctdf, o[, .(.id, cluster)], by = ".id", suffixes = c("", "temp"), all.x = TRUE, sort = FALSE)

  class(o) <- c("clusterTrack", class(o))

  o[is.na(cluster), cluster := 0]
  o


  

}