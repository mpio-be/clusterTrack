
#' Tesselate a ctdf
#'
#' This function computes Dirichlet (Voronoi) polygons from a `ctdf` object 
#'
#' @param ctdf A `ctdf` data frame.
#'
#' @return A sf object.
#'
#' @note Dirichlet polygons are computed via the `deldir` package.
#'
#'
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2,crs = 4326, project_to = "+proj=eqearth")
#' x = tessellate_ctdf(ctdf)
#' s = prune_tesselation(x, threshold = 1, method = 'sd')
#' 
tessellate_ctdf <- function(ctdf) {

  dat = data.table(st_as_sf(ctdf) |> st_coordinates() )
  
  dip = deldir(dat) |> tile.list()

  dip = lapply(dip, function(x) {
    o = x[c("x", "y")] |>
      data.frame() |>
      data.table()
    o = rbind(o, o[1, .(x, y)]) |> as.matrix()
    data.table(geometry = st_polygon(list(o)) |> st_sfc() )
  }) |> rbindlist()

  if (nrow(dip) != nrow(ctdf)) {
    stop(paste("deldir::deldir() dropped", nrow(ctdf) - nrow(dip), "rows due to identical coordinates!"))
  }

  dip = cbind(dip, ctdf[, .(.id)])

  st_as_sf(dip) |>
    st_make_valid() |>
    mutate(A = st_area(geometry))

}


#' @export
prune_tesselation <- function(x, threshold = 1, method = c("sd", "quantile")) {
    
  prune =
    if (method == "sd")       x$A < quantile(x$A, probs = threshold) else
    if (method == "quantile") {
      logA = log(x$A)
      logA <= (mean(logA) + threshold * sd(logA))
    }      

  dplyr::filter(x, prune)
  
}

#' @export
cluster_tessellation <- function(x, nmin = 3,threshold = 1, method = "sd") {

  onull = data.table(.id = integer(), cluster = integer())
  if (nrow(x) < nmin) return(onull)

  tess = tessellate_ctdf(x) |> prune_tesselation(threshold = threshold, method = method)
  if (nrow(tess) < nmin) return(onull)

  nb = poly2nb(tess, queen = TRUE) |> suppressWarnings()
  g = graph_from_adj_list(nb, mode = "all") |>  as_undirected()
  tess$cluster = components(g)$membership
  
  # ggplot(tess)+geom_sf(aes(fill=factor(cluster)))+geom_sf_text(aes(label=.id), size = 2)+ggtitle(x$.segment[1])
  # ggsave(glue("~/Desktop/temp/{paste(range(tess$.id), collapse='_')}.png"))

  dt = data.table(tess)
  dt[, n := .N, by = cluster]
  res = dt[n > nmin, .(.id, cluster)]
  if (nrow(res) < nmin) return(onull)
  return(res)
  


}