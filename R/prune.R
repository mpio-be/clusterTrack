
#' Find the cutoff that maximizes expected classification into component 1
.cutoff_by_mixfit <- function(v) {

  x <- v

  fm = try(mixR::mixfit(x, ncomp = 2, family = "lnorm"), silent = TRUE)
  print(fm)
  
  if(inherits(fm, "try-error")) o = Inf else {

  prop     = fm$pi       
  mean_log = fm$mulog    
  sd_log   = fm$sdlog   


  post1 <- function(x) {
    p1 = prop[1] * stats::dlnorm(x, meanlog = mean_log[1], sdlog = sd_log[1])
    p2 = prop[2] * stats::dlnorm(x, meanlog = mean_log[2], sdlog = sd_log[2])
    p1 / (p1 + p2)
  }

  o = uniroot(function(x) post1(x) - 0.5, lower = min(v), upper = max(v) )$root

  }

  o

}





#' Prune Dirichlet polygons based on area
#'
#' This function computes Dirichlet (Voronoi) polygons from a `ctdf` object and removes 
#' those with unusually small areas (after log-scaling and centering), optionally using 
#' Queen contiguity to define neighbors.
#'
#' @param ctdf A `ctdf` data frame (typically created with [as_ctdf()]).
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
#' @seealso [as_ctdf()],  [spdep::poly2nb()]
#'
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2,crs = 4326, project_to = "+proj=eqearth")
#' nb = prune_dirichlet_polygons(ctdf )
#' 
prune_dirichlet_polygons <- function(ctdf,  queen = TRUE) {

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
  
  cutoff = .cutoff_by_mixfit(dip$A)

  dips = dip[A <= cutoff]



  nb = 
    dips |>  
    st_as_sf() |>
    st_make_valid() |>
    poly2nb(queen = queen)
  
  names(nb) <- dips$.id


  nb


}