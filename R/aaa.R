
#' @import sf data.table  dbscan
#' @importFrom dplyr      mutate ungroup rowwise lead filter
#' @importFrom igraph     groups graph_from_edgelist  components subgraph.edges E set_edge_attr
#' @importFrom forcats    fct_inorder
#' @importFrom geodist geodist

NULL


utils::globalVariables(c('isCluster', 'datetime', 'tenure'))
NULL
