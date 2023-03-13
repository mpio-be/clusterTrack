
#' tdbscan
#'
#' @param track        A [projected][sp::is.projected()]  [Track][trajectories::Track()].
#' @param eps          size of the epsilon neighborhood (see [dbscan::dbscan()]  ).
#' @param minPts       number of minimum points in the eps region (for core points).
#'                     Default is 5 points (see [dbscan::dbscan()]  ).
#' @param borderPoints Logical; default to FALSE (see [dbscan::dbscan()]  ). 
#' @param maxLag       maximum relative temporal lag (see notes). Default to 6. 
#' @param minTenure    minimum time difference, in hours, between the last and the first entry a cluster. 
#'                     Clusters with  values smaller than minTenure are discarded. 
#' @note 
#' When maxLag is set to `maxLag>N` output is the same as for [dbscan][dbscan::dbscan()].
#'
# @import trajectories
#' @importFrom data.table  data.table setnames .N :=
#' @importFrom dbscan      frNN
#' @importFrom igraph      groups graph_from_edgelist  components subgraph.edges E set_edge_attr
#' @importFrom forcats     fct_inorder
#' @importFrom sp          coordinates is.projected
#' @importFrom broom       tidy
#'
#' @export
#' @md
#'
#' @examples
#' require(tdbscan)
#' require(data.table)
#' require(magrittr)
#' require(ggplot2)
#' require(ggrepel)
#' 
#' # Pectoral Sandpiper
#' data(pesa56511)
#' z = tdbscan(pesa56511, eps =6600, minPts   = 8, maxLag = 6, borderPoints = TRUE )
#' 
#' o = cbind(sp::coordinates(pesa56511), z)
#' ggplot(o, aes(longitude/10000,latitude/10000,color = factor(clustID))) +
#' geom_path(aes(color= NULL), col = 'grey', size = .5) + 
#' geom_point( alpha = .5, size = 2)
#' 
#' 
#' # Set minTenure
#' z = tdbscan(pesa56511, eps =6600, minPts   = 8, maxLag = 6, borderPoints = TRUE, minTenure= 24 )
#' o = cbind(sp::coordinates(pesa56511), z)
#' ggplot(o, aes(longitude/10000,latitude/10000,color = factor(clustID))) +
#' geom_path(aes(color= NULL), col = 'grey', size = .5) + 
#' geom_point( alpha = .5, size = 2)
#' 
#' # z bird
#' data(zbird)
#' z = tdbscan(zbird, eps =12, minPts   = 5, maxLag = 5, borderPoints = TRUE )
#' z = z[, clustID := factor(clustID)]
#' 
#' o = data.frame(zbird) %>% data.table
#' o = merge(z, o, by.x = 'id', by.y = 'sp.ID')
#' 
#' clustLab = o[!is.na(clustID), .(x = mean(x), y = mean(y), 
#' 	arrival = min(time), departure = max(time)  ), by = clustID]
#' clustLab[, tenure := difftime(departure, arrival, units = 'hours')]
#' clustLab[, lab := paste("ID: ", clustID, ",", tenure, "hours")]
#' 
#' ggplot(o, aes(y,x, color = clustID ) )+
#'	 geom_path(aes(color= NULL), col = 'grey', size = .5) + 
#'	 geom_point( alpha = .5, size = 2) + 
#'	 geom_label_repel(data = clustLab, aes(y,x, label = lab), alpha = 0.7, size = 3)


tdbscan  = function(track, eps, minPts = 5, borderPoints = FALSE , maxLag = 6, minTenure) {

	checkClust=clustID=id=iscore=n=ngb=tc=y=NULL  # due to NSE notes in R CMD check
	`.` = function(...) NULL


	stopifnot( inherits(track, 'Track') )
	stopifnot( sp::is.projected(track) )

	x = data.table(sp::coordinates(track))
	setnames(x, c('x', 'y') )


	# Find the Fixed Radius Nearest Neighbors
	k =  dbscan::frNN(x, eps = eps,  sort = FALSE)

	ids = sapply(k$dist, length); ids = rep(1:nrow(x), times = ids)
	z = data.table(id = ids, ngb = unlist(k$id), dist = unlist(k$dist) )

	# Define eps neighborhoods
	z[, n :=  .N + 1, by = id] # +1 because a pt is always in its own eps neighborhood

	z[, isCluster := n >= minPts]

	# border points search
	if(borderPoints)
		z[ !(isCluster), isCluster :=  z[!(isCluster), ngb] %in% z[(isCluster), id] ]
	

	z = z[(isCluster)][,isCluster := NULL]

	z[, tc := abs(id-ngb)] # used latter for temporal contiguity


	# Identify clusters
	z[, c("i1", "i2") := list(pmin(id, ngb), pmax(id, ngb))] # id-s should be unique
	z = unique(z, by = c('i1', 'i2') )[, ':=' (i1 = NULL, i2 = NULL)]

	z[, id := as.character(id)] # so it will be  interpreted as symbolic vertex name by graph_from_edgelist
	z[, ngb := as.character(ngb)]

	g = graph_from_edgelist( z[, .(id, ngb)] %>% as.matrix, directed = FALSE)

	# set graph attributes and subset
	g = set_edge_attr(g, 'tc', value = z$tc)

	g = subgraph.edges(g, E(g)[tc <= maxLag])

	gr = components(g) %>% groups

	ids = sapply(gr, length); ids = rep(names(ids), times = ids)%>% as.integer
	o = data.table(id = unlist(gr), clustID = ids)
	o[, id := as.numeric(id)]

	# add o to the original data
	x[, id := 1:.N]

	o = merge(x, o, by = 'id', sort = FALSE, all.x = TRUE)

	# run dbscan on each cluster to check if they still hold
	o[!is.na(clustID), checkClust := dbscan::dbscan( cbind(x,y), eps = eps, minPts = minPts)$cluster != 0, by = clustID]
	o[!(checkClust), clustID := NA]

	# minTenure
	if(!missing(minTenure)) {
		o[, datetime := tidy(track@time)$index ]

		o[!is.na(clustID), tenure := difftime(max(datetime), min(datetime), units= 'hours'), by = clustID]
		o[tenure < minTenure, clustID := NA]
	}

	# cleanup & re-order clust
	x[, id := NULL]
	o[, clustID  := factor(clustID) %>% fct_inorder %>% as.integer]
	o[, .(id, clustID)]

	}



