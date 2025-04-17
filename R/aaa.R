
#' @import sf data.table  dbscan
#' @importFrom dplyr      mutate ungroup rowwise lag filter select rename
#' @importFrom igraph     groups graph_from_edgelist  components subgraph.edges E set_edge_attr
#' @importFrom forcats    fct_inorder
#' @importFrom geodist geodist
#' @importFrom units set_units

NULL


utils::globalVariables(c('isCluster', 'datetime', 'tenure'))
NULL
