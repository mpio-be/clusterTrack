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
  segs =
    ctdf |>
    as_ctdf_track(check = FALSE) |>
    mutate(len = st_length(track) |> set_units("km") |> as.numeric())

  crosses = st_crosses(segs)

  setDT(segs)

  pruned_crosses = lapply(seq_along(crosses), function(i) {
    j = crosses[[i]]

    dfs = difftime(segs$start[j], segs$stop[i], units = "days") |> abs()
    j[dfs <= deltaT]
  })

  segs[, n_crosses := lengths(pruned_crosses)]

  segs[, cross := n_crosses > 0]

  segs[, good_seg_id := rleid(cross)]

  segs[(cross), good_seg_id := NA]

  segs[!is.na(good_seg_id), good_seg_len := sum(len), by = good_seg_id]

  segs[, split_seg := max(good_seg_len, na.rm = TRUE) == good_seg_len]

  segs[is.na(split_seg), split_seg := FALSE]

  segs[, split_seg_id := rleid(split_seg)]

  segs
}


.split_by_maxlen <- function(segs, ctdf) {
  out = segs[!(split_seg), .(.id, split_seg, split_seg_id)]

  out = merge(ctdf, out, by = ".id")

  out = split(out, by = 'split_seg_id')

  for (i in seq_along(out)) {
    out[[i]][, let(split_seg_id = NULL, split_seg = NULL)]
  }

  out
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

  X = copy(ctdf)
  X[, .putative_cluster := NA]

  # Initialize
  result = list()
  queue = .prepare_segs(X, deltaT = deltaT) |> .split_by_maxlen(ctdf = X)

  total_n = nrow(X)
  i = 1
  processed_n = 0

  while (i <= length(queue)) {
    current = queue[[i]]

    if (current |> .has_clusters()) {
      new_chunks = .prepare_segs(current, deltaT = deltaT) |>
        .split_by_maxlen(ctdf = current)
      queue = c(queue, new_chunks)
    } else {
      result = c(result, list(current))
      processed_n = processed_n + nrow(current)
    }

    i = i + 1
  }

  # assign segment id
  for (i in seq_along(result)) {
    result[[i]][, .putative_cluster := i]
  }

  # remove all segments n < 4 (n = 4 == one possible intersection)

  n_by_seg = sapply(result, nrow)
  result = result[n_by_seg > 3]

  o = rbindlist(result)

  setorder(o, .id)
  o[,
    .putative_cluster := factor(.putative_cluster) |>
      fct_inorder() |>
      as.numeric()
  ]
  o = merge(
    ctdf[, .(.id)],
    o[, .(.id, .putative_cluster)],
    all.x = TRUE,
    sort = FALSE
  )

  set(ctdf, j = ".putative_cluster", value = o$.putative_cluster)
}
