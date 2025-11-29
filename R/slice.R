.has_clusters <- function(s) {
  N = 5

  if (nrow(s) <= N) {
    return(FALSE)
  }

  MIN_PTS = ceiling(sqrt(nrow(s)))

  o = hdbscan(st_coordinates(s$location), minPts = MIN_PTS)

  res = length(o$cluster_scores) > 1

  return(res)
}


.prepare_segs <- function(ctdf, deltaT) {
  ctdf[, let(.move_seg = NA, .seg_id = NA)]

  segs =
    ctdf |>
    as_ctdf_track() |>
    mutate(len = st_length(track) |> set_units("km") |> as.numeric())

  crosses = st_crosses(segs)

  setDT(segs)

  pruned_crosses = lapply(seq_along(crosses), function(i) {
    j = crosses[[i]]

    dfs = difftime(segs$start[j], segs$stop[i], units = "days") |> abs()
    j[dfs <= deltaT]
  })

  segs[, n_crosses := lengths(pruned_crosses)]

  segs[, any_cross := n_crosses > 0]

  segs[, good_seg_id := rleid(any_cross)]

  segs[(any_cross), good_seg_id := NA]

  segs[!is.na(good_seg_id), good_seg_len := sum(len), by = good_seg_id]

  segs[, move_seg := max(good_seg_len, na.rm = TRUE) == good_seg_len]

  segs[is.na(move_seg), move_seg := FALSE]

  segs[, seg_id := rleid(move_seg)]

  setkey(segs, .id)

  ctdf[segs, .move_seg := i.move_seg]
  ctdf[segs, .seg_id := i.seg_id]
}


.split_by_maxlen <- function(ctdf, deltaT) {
  .prepare_segs(ctdf, deltaT = deltaT)

  split(ctdf[.move_seg == 0], by = ".seg_id")
}

#' Segment and filter a CTDF by temporal continuity and spatial clustering
#'
#' Recursively splits a CTDF into continuous bouts. The split stops when any bout  has one
#' cluster (via HDBSCAN).
#'
#' @param ctdf A CTDF object.
#' @param deltaT Numeric; maximum allowable gap (in days) between segment
#'   endpoints to consider them continuous.
#' @param progress_bar Logical; whether to display a progress bar during execution. Defaults to `TRUE`.
#' @return The input CTDF, updated (in-place) with an integer
#'   \code{.putative_cluster} column indicating bout membership.
#'
#' @export
#' @examples
#' data(toy_ctdf_k3)
#' ctdf = as_ctdf(toy_ctdf_k3, s_srs = 4326, t_srs = "+proj=eqearth")
#' ctdf = slice_ctdf(ctdf)
#'
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf )

slice_ctdf <- function(ctdf, deltaT = 1) {
  .check_ctdf(ctdf)

  ctdf[, .putative_cluster := NA]

  # Initialize
  result = list()
  queue = .split_by_maxlen(ctdf = ctdf, deltaT = deltaT)

  i = 1

  while (i <= length(queue)) {
    current = queue[[i]]

    if (current |> .has_clusters()) {
      new_chunks = .split_by_maxlen(ctdf = current, deltaT = deltaT)
      queue = c(queue, new_chunks)
    } else {
      # We need .prepare_segs  to remove the longest residual movement bit
      if (nrow(current) > 1) {
        .prepare_segs(ctdf = current, deltaT = deltaT)
        current = current[.move_seg == 0]
        if (nrow(current) > 1) {
          result = c(result, list(current))
        }
      }
    }

    i = i + 1
  }

  # assign segment id
  for (i in seq_along(result)) {
    result[[i]][, .putative_cluster := i]
  }

  if (length(result) == 0) {
    warning("No valid segments found; assigning all rows to cluster 0.")
    set(ctdf, j = ".putative_cluster", value = 0)
    return(invisible(ctdf))
  }

  # TODO: x-check this: remove all .putative_cluster n < 4 (n = 4 == one possible intersection)

  n_by_seg = sapply(result, nrow)

  result = result[n_by_seg > 3]

  out = rbindlist(result)

  setorder(out, .id)
  out[,
    putative_cluster := factor(.putative_cluster) |>
      fct_inorder() |>
      as.numeric()
  ]
  out = out[, .(.id, putative_cluster)]
  setkey(out, .id)

  ctdf[out, .putative_cluster := i.putative_cluster]
}
