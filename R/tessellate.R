

#' Tesselate a ctdf
#'
#' This function computes Dirichlet (Voronoi) polygons from a `ctdf` object 
#'
#' @param ctdf A `ctdf` data frame (typically created with [as_ctdf()]).
#'
#' @return A sf object.
#'
#' @note Dirichlet polygons are computed via the `deldir` package.
#'
#' @seealso [as_ctdf()] 
#'
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2,crs = 4326, project_to = "+proj=eqearth")
#' x = tessellate_ctdf(ctdf)
#' s = prune_tesselation(x, q = 0.90)
#' poly2nb(s, queen = TRUE)
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
prune_tesselation <- function(x, q = 0.90) {


  dplyr::filter(
    x,
    A < quantile(A, probs = q)
  )



}
