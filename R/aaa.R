
#' @import sf data.table  dbscan viridisLite
#' @import deldir  deldir 
#' @importFrom dplyr      mutate ungroup rowwise lag filter select rename
#' @importFrom igraph     groups cliques E components
#' @importFrom igraph     graph_from_edgelist  graph_from_data_frame subgraph.edges set_edge_attr
#' @importFrom forcats    fct_inorder
#' @importFrom geodist geodist
#' @importFrom units set_units

NULL


utils::globalVariables(c('isCluster', 'datetime', 'tenure'))
NULL



# undocumented functions

.tri2lines <- function(x) {
  p = st_coordinates(x)
  segs = seq_len(nrow(p)-1) |> lapply(\(i) st_linestring(p[i + 0:1, 1:2]))
  st_sfc(segs)
  }
