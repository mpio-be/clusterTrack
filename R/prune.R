

#'
#' @param ctdf       A `ctdf` data frame.
#' @param  sd  Numeric multiplier (default = 1).
#'
#' @return A named `neighbors` list. Names correspond with the id in the source `ctdf`.
#' @note TODO: add references
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(zbird)
#' ctdf = as_ctdf(zbird)
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