
#' @rdname cluster_track
#' @export
print.clusterTrack <- function(x, ...) {

  cat("<clusters:", uniqueN(x$cluster), ">\n\n")

  NextMethod("print",
    topn      = 3,
    nrows     = 10,
    print.keys= FALSE,
    ...)


}

#' @export
plot.clusterTrack <- function(x ) {
  
  pal = topo.colors(n = uniqueN(x$cluster) )
  cols = pal[match(x$cluster, sort(unique(x$cluster)))]

  plot(st_geometry(x$location), col = cols)

}

dbscan_tessellation <- function(x, nmin = 5) {
 
  tess = tessellate_ctdf(x) |>
    prune_tesselation(q = 0.9)

  nb = poly2nb(tess, queen = TRUE)

  min_pts = sapply(nb, length) |>
    median() |>
    round()
  if (min_pts < 2) min_pts = min_pts + 1

  if (nrow(tess) > max(c(5, min_pts))) { # dbscan crashes otherwise

    frnn = list(id = nb, eps = 0) # eps is ignored when id is supplied
    class(frnn) = c("frNN", "NN")

    cl = dbscan::dbscan(
      x            = frnn,
      minPts       = min_pts,
      borderPoints = FALSE
    )$cluster
  } else {
    cl = 0
  }



  o = mutate(tess, cluster = cl)

  #' print(min_pts)
  #' mapview::mapview(o['cluster'])

  data.table(o)[, .(.id, cluster)]
}

dbscan_tessellation2 <- function(x, nmin = 5) {
  
  tess = tessellate_ctdf(x) # |> prune_tesselation(q = 0.5)
  
  dd = st_centroid(tess) |> st_distance()
  
  #  find its 4th‐smallest nonzero distance (kNN distance for k = 4)
  dd[diag(TRUE, nrow(dd))] = Inf
  knn4 = apply(dd, 1, function(z) sort(z, partial = 4)[4])  

  knn_sort = sort(knn4)  

  detect_elbow = function(x_vec) {
    n = length(x_vec)
    # endpoints A=(1, x_vec[1]) and B=(n, x_vec[n])
    A = c(1,     x_vec[1])
    B = c(n,     x_vec[n])
    # line coefficients for "a*x + b*y + c = 0"
    a = B[2] - A[2]
    b = A[1] - B[1]
    c = B[1]*A[2] - A[1]*B[2]
    idx = seq_len(n)
    # perpendicular distance from each (idx[i], x_vec[i]) to line AB
    d = abs(a * idx + b * x_vec + c) / sqrt(a^2 + b^2)
    list(
      eps_value = x_vec[which.max(d)],
      eps_index = which.max(d)
    )
  }

  elbow = detect_elbow(knn_sort)



  # dd = st_centroid(tess) |> st_distance()
  # dd =dd[dd>0]

  # eps = quantile(dd[dd>0], probs = 0.3)
  # eps = quantile(dd, probs = 0.95)

  cl = dbscan::dbscan(
    x            =  st_centroid(tess) |> st_coordinates(),
    eps          = elbow$eps_value,
    minPts       = 5,
    borderPoints = TRUE
  )$cluster

  # if(all(cl == 0)) cl = 1
  

  o = mutate(tess, cluster = cl)


  data.table(o)[, .(.id, cluster)]

}



#' Cluster movement tracks
#'
#' Perform clustering on a `ctdf` object to identify use-sites. Uses a hybrid approach combining Dirichlet-based polygon pruning and DBSCAN clustering to detect spatially and temporally coherent movement clusters ("use-sites").
#'
#' @param ctdf A `ctdf` object (from [as_ctdf()]), representing sequential movement points with timestamps and locations.
#' @return A `ctdf` object containing only points assigned to clusters, with an additional integer column `cluster` indicating the cluster ID for each point (consecutive values starting from 1). Cluster membership reflects spatial-temporal groupings of points.
#'
#'
#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf, 12)
#' clust = cluster_track(ctdf)
#' clusterTrack.Vis::map(clust)
#'
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_track(ctdf)
#' clusterTrack.Vis::map(clust)
#'
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf,  slice_method = function(x)(quantile(x, .5))) 
#' clust = cluster_track(ctdf)
#' clusterTrack.Vis::map(clust)
#' 
cluster_track <- function(ctdf, nmin = 5) {

  s = ctdf[(!.filter)]
  s[, n := .N, by = .segment]
  s = s[n>= nmin]

  segs = s$.segment |> unique()

  handlers(global = TRUE)
  p = progressor(steps = length(segs))

  o = foreach(si = segs, .errorhandling = 'pass') %do% {
    p()
    x = s[.segment == si]

    oi = dbscan_tessellation2(x)
    oi = oi[cluster > 0]
    oi[, cluster := paste(si, cluster)]
    oi
  }

  e = sapply(o, FUN = inherits, what = "error")
  
  if (length(segs[e]) > 0) {
    message("segments ", paste(segs[e], collapse = ", "), " did not return a cluster!")
  }

  o = o[!e]

  o = rbindlist(o)

  o[, cluster := as.factor(cluster) |> as.integer()]

  o = merge(ctdf, o[, .(.id, cluster)], by = ".id", suffixes = c("", "temp"), all.x = TRUE, sort = FALSE)

  class(o) <- c("clusterTrack", class(o))

  o[is.na(cluster), cluster := 0]
  
  return(o)

  # feedback
  ncl = nrow(o[cluster>0, .N, cluster])
  if (ncl == 0) {warning("No clusters found!")} 
  
  message(sprintf("%d cluster%s detected!", ncl, if (ncl > 1L) "s" else ""))
  

  

}