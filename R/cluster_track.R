.time_contiguity <- function(ctdf) {
  ctdf[,
    cluster := {
      f = nafill(cluster, type = "locf")
      b = nafill(cluster, type = "nocb")
      fifelse(f == b, f, cluster)
    }
  ]
}


#' @export
print.clusterTrack <- function(x, ...) {
  cat("<clusters:", uniqueN(x$cluster) - 1, ">\n\n")

  NextMethod("print", topn = 3, nrows = 10, print.keys = FALSE, ...)
}

#' @export
plot.clusterTrack <- function(x) {
  pal = topo.colors(n = uniqueN(x$cluster))
  cols = pal[match(x$cluster, sort(unique(x$cluster)))]

  plot(st_geometry(x$location), col = cols)
}

#' Cluster movement tracks
#'
#' Performs spatiotemporal clustering on a ctdf by segmenting movement, identifying stops, and applying DBSCAN-like clustering.
#'
#'
#' This is a high-level wrapper function that applies a pipeline of segmentation, clustering, and stitching steps on a movement track stored in a `ctdf` object.
#'
#'
#' @param ctdf   A `ctdf` data frame (see [as_ctdf()]) representing a single movement track .
#' @param deltaT Numeric; maximum allowable gap (in days) between segment endpoints to consider them non-intersecting.
#'               Default to 1 day. Passed to [slice()]) .
#' @param nmin   Integer. Segments or tessellations with fewer than nmin points yield no clusters.
#'               Default to 3. Passed to [cluster_segments()].
#' @param threshold Numeric. the multiplier of the standard deviation
#'                  on log‐areas used in pruning. Passed to [cluster_segments()].
#' @param time_contiguity Logical; if `TRUE`, missing cluster IDs (usually spatial outliers) are  filled
#'                        within each cluster to enforce temporal continuity.
#'                        Default to `FALSE`.
#'                        Passed to [cluster_segments()].
#' @param overlap_threshold Numeric between 0 and 1; minimum area‐overlap ratio
#'                          required to merge adjacent clusters. Default to 0.1.
#'                          Clusters with overlap > threshold are combined.
#'                          Passed to [cluster_stitch()]
#' @return NULL.
#' The function modifies `ctdf` by reference, adding or updating the column \code{cluster},
#' which assigns a cluster ID to each row (point).
#' Clustering parameters are stored as an attribute: `attr(ctdf, "cluster_params")`.
#'

#' @export
#' @examples
#' data(toy_ctdf_k3)
#' ctdf = as_ctdf(toy_ctdf_k3) |> cluster_track()
#'
#' \dontrun{
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#'
#'
#' data(ruff143789)
#' ctdf = as_ctdf(ruff143789, time = "locationDate") |> cluster_track()
#'
#' data(lbdo66862)
#' ctdf = as_ctdf(lbdo66862, time = "locationDate") |> cluster_track()
#'
#'
#' }

cluster_track <- function(
  ctdf,
  deltaT = 1,
  nmin = 5,
  threshold = 2,
  time_contiguity = FALSE,
  overlap_threshold = 0.1
) {
  options(datatable.showProgress = FALSE)
  cli_progress_bar("", type = "tasks", total = 4)

  cli_progress_output("Track segmentation...")
  slice_ctdf(ctdf, deltaT = deltaT)
  cli_progress_update()

  cli_progress_output("Tessellating points by segment..")
  tessellate_ctdf(ctdf)
  cli_progress_update()

  cli_progress_output("Within-segment clustering...")
  cluster_segments(
    ctdf,
    nmin = nmin,
    threshold = threshold,
    time_contiguity = time_contiguity
  )
  cli_progress_update()

  cli_progress_output("Cluster stitching ...")
  cluster_stitch(ctdf, overlap_threshold = overlap_threshold)
  cli_progress_update()

  if (time_contiguity) {
    .time_contiguity(ctdf)
  }
  ctdf[is.na(cluster), cluster := 0]

  # collect parameters to save
  cluster_params = list(
    deltaT = deltaT,
    nmin = nmin,
    threshold = threshold,
    time_contiguity = time_contiguity,
    overlap_threshold = overlap_threshold
  )

  setattr(ctdf, "cluster_params", cluster_params)

  cli_progress_done()

  ctdf
}
