#' Prune Dirichlet polygons based on area
#'
#' This function computes Dirichlet (Voronoi) polygons from a `ctdf` object and removes 
#' those with unusually small areas (after log-scaling and centering), optionally using 
#' Queen contiguity to define neighbors.
#'
#' @param ctdf A `ctdf` data frame (typically created with [as_ctdf()]).
#' @param sd Numeric multiplier controlling the area threshold for pruning (default = 1). 
#'           Polygons with log-area below this threshold (relative to the mean) are considered for pruning.
#' @param queen Logical. If `TRUE` (default), uses Queen contiguity in neighbor detection 
#'   (shared point or edge). If `FALSE`, uses Rook contiguity (shared edge only).
#'
#' @return A named list of class `nb` (from the `spdep` package), where each name corresponds 
#'   to an `.id` in the input `ctdf` and each element is an integer vector of neighbor indices.
#'
#' @seealso [as_ctdf()], [filter_intersection()], [spdep::poly2nb()]
#'
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2)
#' filter_intersection(ctdf)
#' nb = prune_dirichlet_polygons(ctdf, sd = 1, transform = sqrt)
#' plot(nb, st_coordinates(ctdf[.id%in%names(nb), location]))
#' 
prune_dirichlet_polygons <- function(ctdf, sd = 1, queen = TRUE, transform = log) {

  dat = data.table(st_as_sf(ctdf) |> st_coordinates() )
  
  dip = deldir(dat) |> tile.list()

  dip = lapply(dip, function(x) {
    o = x[c("x", "y")] |>
      data.frame() |>
      data.table()
    o = rbind(o, o[1, .(x, y)]) |> as.matrix()
    data.table(geometry = st_polygon(list(o)) |> st_sfc() )
  }) |> rbindlist()

  dip = cbind(dip, ctdf[, .(.id)])
  dip[, A := st_area(geometry)]
  dip[, A := transform(A)]
  dip[, A := scale(A)]

  dips = dip[A < sd ]

  nb = 
    dips |>  
    st_as_sf() |>
    st_make_valid() |>
    poly2nb(queen = queen)
  
  names(nb) <- dips$.id

  nb


}