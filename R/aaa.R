
#' @import foreach 
#' @import sf data.table  
#' @import ggplot2 glue

#' @import igraph
# @importFrom igraph   TODO

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
