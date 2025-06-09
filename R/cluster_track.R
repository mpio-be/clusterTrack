
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
#' x = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth") |>cluster_track()
#' map(x)


#' data(pesa56511)
#' x  = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth") |>cluster_track()
#' map(x)
#' 
#' data(lbdo66867)
#' x = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")|>cluster_track()
#' map(x)
cluster_track <- function(ctdf) {

  ctdf |>
  slice_ctdf() |>
  cluster_segments() |>
  stitch_cluster()



}