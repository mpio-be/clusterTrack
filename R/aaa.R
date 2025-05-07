
#' @import sf data.table  dbscan 
#' @import deldir  deldir 
#' @importFrom dplyr      mutate ungroup rowwise lag filter select rename
#' @importFrom igraph     groups graph_from_edgelist  components subgraph.edges E set_edge_attr
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
