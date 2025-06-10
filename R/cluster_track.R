
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

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth") |>cluster_track()
#' map(ctdf)


#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth") |>cluster_track()
#' map(ctdf)
#' 
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")|>cluster_track()
#' map(ctdf)
cluster_track <- function(ctdf,deltaT = 30, threshold = 0.75, method = "quantile", overlap_threshold = 0) {

  ctdf |>
  slice_ctdf(deltaT = deltaT) |>
  cluster_segments(threshold = threshold, method = method) |>
  stitch_cluster(overlap_threshold = overlap_threshold)



}