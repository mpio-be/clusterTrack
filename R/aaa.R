
#' @import foreach 
#' @import sf data.table  

#' @import ggplot2 glue
#' 
#' @importFrom igraph graph_from_edgelist set_edge_attr subgraph_from_edges E groups
#' @importFrom igraph graph_from_adj_list as_undirected components
#' 
#' @importFrom dbscan     hdbscan
#' @importFrom deldir     deldir tile.list
#' @importFrom spdep      poly2nb 
#' @importFrom forcats    fct_inorder
#' @importFrom units      set_units
#' @importFrom dplyr      mutate ungroup rowwise lag filter select rename
#' @importFrom forcats    fct_inorder
#' @importFrom signal     sgolayfilt
NULL


utils::globalVariables(c('isCluster', 'datetime', 'tenure'))
NULL



# undocumented functions

.st_area_overlap_ratio <- function(i, j) {
  ai = st_area(i) |> as.numeric()
  aj = st_area(j) |> as.numeric()
  aij = st_intersection(i,j) |> st_area() |> as.numeric()
  if (length(aij) == 0) aij = 0  

  as.numeric(aij / pmin(ai, aj))
}
