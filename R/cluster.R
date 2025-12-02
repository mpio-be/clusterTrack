#' Cluster segments of a ctdf
#'
#' Compute cluster IDs for each point in a `ctdf` by segment, using Dirichlet
#' tessellation followed by area-based pruning, spatial adjacency and graph-based clustering.
#' Optionally enforce temporal contiguity of clusters within each segment.
#'
#' @param ctdf A `ctdf` object, must contain a `.putative_cluster` column; see [slice_ctdf()],.
#' @param nmin Integer. Segments or tessellations with fewer than nmin points yield no clusters.
#'   Default to 3.
#' @param threshold Numeric. The multiplier of the standard deviation on log‐areas used in pruning.
#' @param progress_bar Logical; whether to display a progress bar during execution. Defaults to `TRUE`.
#'
#' @return Invisibly returns `NULL`. The input `ctdf` is modified by reference,
#'  updating the column `cluster`; `0` indicates unclustered points.
#'
#' @details For each unique segment in `ctdf`, points are tessellated via
#'   [tessellate_ctdf()], pruned via [prune_tesselation()], and adjacency
#'   neighborhoods are computed with `poly2nb()`. These neighborhoods are
#'   converted to an undirected graph, and clusters are identified as connected
#'   components.
#'
#' @export
#' @examples
#' data(toy_ctdf_k3)
#' ctdf = as_ctdf(toy_ctdf_k3)
#' slice_ctdf(ctdf)
#' tessellate_ctdf(ctdf )
#' cluster_segments(ctdf)
#'
cluster_segments <- function(
  ctdf,
  nmin = 3,
  threshold = 1,
  time_contiguity = FALSE
) {
  # prune on log(Area)
  x = ctdf[!is.na(.putative_cluster), .(.id, .putative_cluster, .tesselation)]

  #'   x = ctdf[.putative_cluster == 1]

  x[, A := st_sfc(.tesselation) |> st_area() |> as.numeric()]

  x[, zA := scale(log(A)) |> as.numeric(), by = .putative_cluster]

  x[, keep := zA < threshold]

  #' ggplot()+geom_sf(data=x[, .(keep, .tesselation)]|>st_as_sf(), aes(color = keep))
  #' tinyplot(~ix|keep, data=x, type = type_histogram(breaks = 30))

  x = x[(keep)]

  # isolate clusters and assign clusters ID-s

  x[,
    cluster := .isolate_clusters(st_sfc(.tesselation)),
    by = .putative_cluster
  ]

  x[, cluster := .GRP, by = .(.putative_cluster, cluster)]

  # subset by min-N
  x[, n := .N, cluster]
  x = x[n > nmin]
  x[, cluster := .GRP, by = cluster] # re-asign id
  x[, cluster := as.integer(cluster)]

  # update ctdf

  # TODO: use join
  o = merge(
    ctdf[, .(.id)],
    x[, .(.id, cluster)],
    by = ".id",
    all.x = TRUE,
    sort = FALSE
  )

  o[is.na(cluster), cluster := 0]

  set(ctdf, j = "cluster", value = o$cluster)
}
