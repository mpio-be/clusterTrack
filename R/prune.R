
#' Prune Delaunay Triangulation and Extract Pruned Edges
#'
#' Given a `ctdf`  this function performs:
#' 1. Delaunay triangulation via `deldir`.
#' 2. Global-effect removal: discards triangles whose area or longest side 
#'    exceeds (mean + global_sd * sd).
#' 3. Local-effect removal: among survivors, discards triangles whose longest 
#'    side exceeds (mean + local_sd * sd) of remaining edges.
#' 4. Builds the adjacency edge list of remaining triangles.
#'
#' @param ctdf       A `ctdf` data frame.
#' @param global_sd  Numeric multiplier for the global threshold (default = 1).
#' @param local_sd   Numeric multiplier for the local threshold (default = 1).
#'
#' @return Invisibly returns the input \code{ctdf} object which got a new \code{neighbors} column.
#' @note TODO: add reference
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(zbird)
#' ctdf = as_ctdf(zbird)
#' filter_intersection(ctdf)
#' prune_delaunay_edges(ctdf)
#' plot(ctdf, by = 'filter')
#' 
prune_delaunay_edges <- function(ctdf,  global_sd = 1, local_sd  = 1) {
  
  dat = st_as_sf(ctdf) |> st_coordinates() |> data.table()
  
  # Delaunay triangulation
  del = deldir(dat)
  dat[, id := .I]
  
  edges_dt = as.data.table(del$delsgs)[
    , .(from = ind1, to = ind2)
  ]
  
  # Build full graph  
  g_del = graph_from_data_frame(
    d = edges_dt, 
    directed = FALSE,
    vertices = dat[, .(name = id)]
  )

  # Extract cliques as Delaunay triangles
  tris = cliques(g_del, min = 3, max = 3)
  
  tris = tris |> lapply(function(x) (as.matrix(x) |> t() |> data.table())) |>
    rbindlist(use.names=FALSE)
  setnames(tris, c("id1", "id2", "id3"))

  tris[,
    triangles := {
      pts = dat[c(id1,id2,id3), .(X, Y)]
      p = rbind(pts, pts[1, ]) |> as.matrix() |> list() |> st_polygon() |> list() 
    },
    by = .(id1,id2,id3)
  ]

  tris[, areas := st_area(triangles), by = .I]

  tris[, max_edge := .tri2lines(triangles) |> st_length() |> max(), by = .I]

  # Global‐effect removal
  tris[, keep := FALSE]
  thr_area = tris[, mean(areas) + sd(areas) * global_sd]
  thr_len = tris[, mean(max_edge) + sd(max_edge) * global_sd ]

  tris[areas <= thr_area & max_edge <= thr_len, keep := TRUE]


  # Local‐effect removal
  x = tris[(keep), .(local_edges = .tri2lines(triangles) |> st_length()), by = .I]

  local_thr_len = x[, mean(local_edges) + sd(local_edges) * local_sd]

  tris[, keep := FALSE]
  tris[max_edge <= local_thr_len, keep := TRUE]

  # Pruned adjacency graph
  triss = tris[(keep)]

  edge_list = triss[, .(
      u = c(id1, id2, id3),
      v = c(id2, id3, id1)
  ), by = .(id1, id2, id3)]

  # Output is an 2-column edge dt
  edge_dt = edge_list[, .(
      from = pmin(u, v),
      to   = pmax(u, v)
  )] |> unique()


  # remove filterded id-s
  bad_ids   = dat[ctdf$filter, id]

  edge_dt = edge_dt[ 
    !(from %in% bad_ids | to %in% bad_ids)
  ]


  # build graph
  g_pruned = graph_from_data_frame(
    d = edge_dt[, .(from, to)],
    directed = FALSE,
    vertices = data.frame(name = dat$id)
  )

  nbrs_vs = igraph::as_adj_list(g_pruned, mode="all")
  nbrs = lapply(nbrs_vs, as.integer)

  stopifnot(length(nbrs) == nrow(ctdf))

  set(ctdf, j = "neighbors", value = nbrs)



}



prune_dirichlet_polygons <- function(ctdf) {


}