
#' @import sf data.table  dbscan
#' @import foreach
#' @import deldir  deldir 
#' @importFrom spdep poly2nb plot.nb subset.nb
#' @importFrom igraph     groups cliques E components
#' @importFrom igraph     graph_from_edgelist  graph_from_data_frame subgraph.edges set_edge_attr
#' @importFrom forcats    fct_inorder
#' @importFrom units set_units
#' @importFrom dplyr      mutate ungroup rowwise lag filter select rename

NULL


utils::globalVariables(c('isCluster', 'datetime', 'tenure'))
NULL



# undocumented functions
