#' ctdf = as_ctdf(mini_ruff,s_srs = 4326, t_srs = "+proj=eqearth") |> slice_ctdf()
#' x = ctdf[.putative_cluster == 1]
.tesselate <- function(x) {
  p = st_as_sf(x[, .(.id, location)]) |>
    st_make_valid()

  tess = st_combine(p) |>
    st_voronoi(point_order = TRUE, dTolerance = 1e-4) |>
    st_collection_extract("POLYGON")

  # env = st_union(p) |>
  #   st_concave_hull(ratio = 0.5) |>
  #   # TODO: ratio = 0.5 should be documented
  #   st_buffer(dist = (sqrt(median(st_area(tess)) / pi)))

  # #'  plot(tess); plot(env, add = TRUE, border = 2, lwd = 2)
  # #'  plot(env); plot(tess, add = TRUE, border = 2, lwd = 0.5)

  # tess = st_intersection(tess, env)
  tess = st_cast(tess, "MULTIPOLYGON") |> st_geometry()

  st_set_geometry(p, tess)
}

.isolate_clusters <- function(tess) {
  nb = poly2nb(tess, queen = TRUE) |> suppressWarnings()
  g = graph_from_adj_list(nb, mode = "all") |> as_undirected()
  components(g)$membership
}


#' Tesselate a ctdf
#'
#' This function computes Dirichlet (Voronoi) polygons on each putative_cluster
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
#' data(mini_ruff)
#' ctdf = as_ctdf(mini_ruff,s_srs = 4326, t_srs = "+proj=eqearth") |> slice_ctdf()
#' tessellate_ctdf(ctdf)
#'
tessellate_ctdf <- function(ctdf) {
  if (nrow(ctdf[!is.na(.putative_cluster)]) == 0) {
    warning("No valid putative clusters found!")
    return(NULL)
  }

  out = ctdf[!is.na(.putative_cluster), .tesselate(.SD), .putative_cluster]
  setkey(out, .id)

  ctdf[out, .tesselation := i.location]
}
