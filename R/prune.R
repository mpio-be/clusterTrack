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
#' @note Dirichlet polygons are computed via the `deldir` package. Only polygons with 
#'   `.filter == FALSE` and small log-area (relative to the sample) are retained before 
#'   neighbor construction. This is typically used to identify spatially isolated or dense 
#'   regions in tracking data.
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
#' nb = prune_dirichlet_polygons(ctdf, sd = 1)
#' plot(nb, st_coordinates(ctdf[.id%in%names(nb), location]))
#' 
prune_dirichlet_polygons <- function(ctdf, sd = 1, queen = TRUE) {

  dat = data.table(st_as_sf(ctdf) |> st_coordinates() )
  
  dip = deldir(dat) |> tile.list()

  dip = lapply(dip, function(x) {
    o = x[c("x", "y")] |>
      data.frame() |>
      data.table()
    o = rbind(o, o[1, .(x, y)]) |> as.matrix()
    data.table(geometry = st_polygon(list(o)) |> st_sfc(), any_bp = any(x$bp))
  }) |> rbindlist()

  dip = cbind(dip, ctdf[, .(.id, .filter)])
  dip[, A := scale(st_area(geometry) |> log())]

  dips = dip[A < sd & (!.filter)]

  nb = 
    dips |>  
    st_as_sf() |>
    st_make_valid() |>
    poly2nb(queen = queen)
  
  names(nb) <- dips$.id

  nb


}