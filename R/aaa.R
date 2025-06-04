
#' @import foreach 
#' @import sf data.table  
#' @import ggplot2 glue

#' @importFrom igraph     graph_from_adj_list clusters
#' @importFrom igraph     groups cliques E components
#' @importFrom igraph     graph_from_edgelist  graph_from_data_frame subgraph.edges set_edge_attr
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
