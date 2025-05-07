"#################################################################################
#                            --- Code Description ---                            #
#                                    IN WORK                                     #
#################################################################################"


#' Prune Delaunay Triangulation and Extract Pruned Edges
#'
#' Given a `ctdf`  this function performs:
#' 1. Delaunay triangulation via `deldir`.
#' 2. Global-effect removal: discards triangles whose area or longest side 
#'    exceeds (mean + global_sd_mult * sd).
#' 3. Local-effect removal: among survivors, discards triangles whose longest 
#'    side exceeds (mean + local_sd_mult * sd) of remaining edges.
#' 4. Builds the adjacency edge list of remaining triangles.
#'
#' @param ctdf            A `ctdf` data frame.
#'                        `"location"` of point features and an integer `id`.
#' @param global_sd_mult  Numeric multiplier for the global threshold (default = 1).
#' @param local_sd_mult   Numeric multiplier for the local threshold (default = 1).
#'
#' @return A `data.table` with columns `from` and `to`, giving the pruned 
#'         undirected edges connecting point `id`s.
#' @note TODO: add reference
#'
#' @export
#' @examples
#' library(clusterTrack)
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time="locationDate")
#' o = prune_delaunay_edges(ctdf)
#' 
prune_delaunay_edges <- function(ctdf,  global_sd_mult = 1, local_sd_mult  = 1) {
  
  dat = data.table(st_coordinates(ctdf))
  
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
  thr_area = tris[, mean(areas) + sd(areas)]
  thr_len = tris[, mean(max_edge) + sd(max_edge)]

  tris[areas <= thr_area & max_edge <= thr_len, keep := TRUE]


  # Local‐effect removal
  x = tris[(keep), .(local_edges = .tri2lines(triangles) |> st_length()), by = .I]

  local_thr_len = x[, mean(local_edges) + sd(local_edges)]

  tris[, keep := FALSE]
  tris[max_edge <= local_thr_len, keep := TRUE]

  # Pruned adjacency graph
  triss = tris[(keep)]

  edge_list = triss[, .(
      u = c(id1, id2, id3),
      v = c(id2, id3, id1)
  ), by = .(id1, id2, id3)]

  # Output is an 2-column edge dt
  out = edge_list[, .(
      from = pmin(u, v),
      to   = pmax(u, v)
  )] |> unique()

  out

}



prune_dirichlet_polygons <- function(ctdf) {

# TODO
}