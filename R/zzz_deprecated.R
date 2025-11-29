# TODO: deprecate before new major release!

#' @export
as_tdbscan <- function(
  x,
  coords = c("longitude", "latitude"),
  time = "time",
  crs = 4326
) {
  o = copy(x)

  setnames(o, time, "timestamp")
  setorder(o, timestamp)

  st_as_sf(o, coords = coords, crs = crs)
}


#' tdbscan
#'
#' @param track        A sf object (for now).
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
#'
#' @export
#' @md
#'
#' @examples
#' data(pesa56511)
#' x = as_tdbscan(pesa56511, time = "locationDate", s_srs = 4326)
#' x = st_transform(x, '+proj=eqearth')
#' z = tdbscan(track=x, eps =6600 , minPts   = 8, maxLag = 6, borderPoints = TRUE )
#'
#' ggplot(z ) +geom_sf(aes(color = factor(clustID) ))
#'
#'
#' # Set minTenure
#' z = tdbscan(x, eps =6600, minPts   = 8, maxLag = 6, borderPoints = TRUE, minTenure= 24 )
#' ggplot(z ) +geom_sf(aes(color = factor(clustID) ))
#'

tdbscan <- function(
  track,
  eps,
  minPts = 5,
  borderPoints = FALSE,
  maxLag = 6,
  minTenure
) {
  checkClust = clustID = id = iscore = n = ngb = tc = y = NULL # due to NSE notes in R CMD check
  `.` = function(...) NULL

  stopifnot(inherits(track, 'sf'))

  x = st_coordinates(track) |> data.table()
  setnames(x, c('x', 'y'))

  # Find the Fixed Radius Nearest Neighbors
  k = dbscan::frNN(x, eps = eps, sort = FALSE)

  ids = sapply(k$dist, length)
  ids = rep(1:nrow(x), times = ids)
  z = data.table(id = ids, ngb = unlist(k$id), dist = unlist(k$dist))

  # Define eps neighborhoods
  z[, n := .N + 1, by = id] # +1 because a pt is always in its own eps neighborhood

  z[, isCluster := n >= minPts]

  # border points search
  if (borderPoints) {
    z[!(isCluster), isCluster := z[!(isCluster), ngb] %in% z[(isCluster), id]]
  }

  z = z[(isCluster)][, isCluster := NULL]

  z[, tc := abs(id - ngb)] # used latter for temporal contiguity

  # Identify clusters
  z[, c("i1", "i2") := list(pmin(id, ngb), pmax(id, ngb))] # id-s should be unique
  z = unique(z, by = c('i1', 'i2'))[, ':='(i1 = NULL, i2 = NULL)]

  z[, id := as.character(id)] # so it will be  interpreted as symbolic vertex name by graph_from_edgelist
  z[, ngb := as.character(ngb)]

  g = graph_from_edgelist(z[, .(id, ngb)] |> as.matrix(), directed = FALSE)

  # set graph attributes and subset
  g = set_edge_attr(g, 'tc', value = z$tc)

  g = subgraph_from_edges(g, E(g)[tc <= maxLag])

  gr = components(g) |> groups()

  ids = sapply(gr, length)
  ids = rep(names(ids), times = ids) |> as.integer()
  o = data.table(id = unlist(gr), clustID = ids)
  o[, id := as.numeric(id)]

  # add o to the original data
  x[, id := 1:.N]

  o = merge(x, o, by = 'id', sort = FALSE, all.x = TRUE)

  # run dbscan on each cluster to check if they still hold
  o[
    !is.na(clustID),
    checkClust := dbscan::dbscan(
      cbind(x, y),
      eps = eps,
      minPts = minPts
    )$cluster !=
      0,
    by = clustID
  ]
  o[!(checkClust), clustID := NA]

  # minTenure
  if (!missing(minTenure)) {
    o[, datetime := track$timestamp]

    o[
      !is.na(clustID),
      tenure := difftime(max(datetime), min(datetime), units = 'hours'),
      by = clustID
    ]
    o[tenure < minTenure, clustID := NA]
  }

  # cleanup & re-order clust
  x[, id := NULL]
  o[, clustID := factor(clustID) |> fct_inorder() |> as.integer()]
  o[, .(id, clustID)]

  track$clustID = o$clustID
  track
}
