
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

  plot(x$nb, st_coordinates(x$location), col = cols)

}



#' Cluster movement tracks
#'
#' Perform clustering on a `ctdf` object to identify use-sites. Uses a hybrid approach combining Dirichlet-based polygon pruning and DBSCAN clustering to detect spatially and temporally coherent movement clusters ("use-sites").
#'
#' @param ctdf A `ctdf` object (from [as_ctdf()]), representing sequential movement points with timestamps and locations.
#' @param sd Numeric; threshold on the scaled log-area of Dirichlet polygons. Polygons with scaled log-area below this threshold are retained for neighbor calculation; expressed in standard deviations from the mean of the log-area.
#' Default = 1.
#' @param minPts Integer; minimum number of neighbors (including self) to form a core point in DBSCAN. Default = 5.
#' @param borderPoints Logical; if `FALSE`, treat all border points as noise (DBSCAN*). Default = TRUE.
#'
#' @return A `ctdf` object containing only points assigned to clusters, with an additional integer column `cluster` indicating the cluster ID for each point (consecutive values starting from 1). Cluster membership reflects spatial-temporal groupings of points.
#'
#'
#' @export
#' @examples 
#' library(clusterTrack)
#' data(zbird)
#' ctdf = as_ctdf(zbird )
#' filter_intersection(ctdf, overwrite = TRUE)

#' o = cluster_track(ctdf, sd = 1)
#' plot(o)
#' 
cluster_track <- function(ctdf, sd = 1, minPts = 5, borderPoints = TRUE) {

  nb = prune_dirichlet_polygons(ctdf, sd = sd)


  frnn = list(id = nb, eps = 0) # eps is ignored when id is supplied
  class(frnn) = c("frNN","NN")

  o = dbscan::dbscan(x = frnn, minPts = minPts, borderPoints = borderPoints)
  
  nam_nb = names(nb) # subset.nb does not work on a named list
  names(nb) = NULL
  nbs = subset(nb, o$cluster > 0)
  nam_nbs = nam_nb[o$cluster > 0]
  names(nbs) = nam_nbs

  clusters = o$cluster[o$cluster > 0]


  o = data.table(.id = names(nbs) |> as.integer(), cluster = clusters, nb = nbs)
  
 
  o = merge(o, ctdf[,.(.id, timestamp, location)])


  # sanity check
  ncl = nrow(o[cluster>0, .N, cluster])
  if (ncl == 0) {
    warning("No clusters found!")
  } else {
    message(sprintf("%d cluster%s detected!", ncl, if (ncl > 1L) "s" else ""))
  }

  class(o) <- c("clusterTrack", class(o))

  o

}