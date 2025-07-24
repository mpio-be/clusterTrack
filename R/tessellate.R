
#' ctdf = as_ctdf(toy_ctdf_k3,s_srs = 4326, t_srs = "+proj=eqearth") |> slice_ctdf()
#' x = ctdf[.segment == 1]
.tesselate <- function(x) {

  p = st_as_sf(x[, .(.id, location)]) |>
    st_make_valid()

  tess = st_combine(p) |>
    st_voronoi(point_order = TRUE, dTolerance = 1e-6) |>
    st_collection_extract("POLYGON")

  env = st_union(p) |>
    st_concave_hull(ratio = 0) |>
    st_buffer(dist = sqrt(median(st_area(tess)) / pi))

  #'  plot(tess); plot(env, add = TRUE, border = 2, lwd = 2)

  tess = st_intersection(tess, env)
  tess = st_cast(tess, "POLYGON")
  
  st_set_geometry(p, st_geometry(tess) )

}

.isolate_clusters <- function(tess) {
  nb = poly2nb(tess, queen = TRUE) |> suppressWarnings()
  g = graph_from_adj_list(nb, mode = "all") |>  as_undirected()
  components(g)$membership
}


#' Tesselate a ctdf
#'
#' This function computes Dirichlet (Voronoi) polygons on each segment 
#' of a `ctdf` object
#'
#' @param ctdf A `ctdf` data frame.
#'
#' @return un updated ctdf object.
#'
#'
#'
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(toy_ctdf_k3)
#' ctdf = as_ctdf(toy_ctdf_k3,s_srs = 4326, t_srs = "+proj=eqearth") |> slice_ctdf()
#' tessellate_ctdf(ctdf)
#' 
tessellate_ctdf <- function(ctdf) {

  o = ctdf[!is.na(.segment), .tesselate(.SD), .segment]

  o = merge(ctdf[, .(.id)], o[, .(.id, location)], by = ".id",  all.x = TRUE, sort = FALSE)

  set(ctdf, j = "tesselation", value = o$location)
  
}
