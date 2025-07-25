

.has_clusters <- function(s ) {

  N = 5

  if (nrow(s) <= N) {
    return(FALSE)
  }

  MIN_PTS = ceiling(sqrt(nrow(s)))
  
  o = hdbscan(st_coordinates(s$location), minPts = MIN_PTS )

  res = length(o$cluster_scores) > 1

  return(res)
  

}


.split_by_maxlen <- function(ctdf, deltaT) {


  # make segments
  segs =
    ctdf |>
    as_ctdf_track() |>
    mutate(len = st_length(track) |> set_units("km") |> as.numeric())
    

  ints = st_crosses(segs) 

  setDT(segs)

  pruned_ints = lapply(seq_along(ints), function(i) {
    j = ints[[i]]

    dfs = difftime(segs$start[j], segs$stop[i], units = "days") |> abs()
    j[dfs <= deltaT]
  })
  
  segs[, n_ints  := lengths(pruned_ints) ]
  segs[, cross   := n_ints > 0]

  segs[, bout_id := rleid(cross)]
  segs[, len     := sum(len), bout_id]

  # slice
  good_segs = segs[(!cross)] 

  if (nrow(good_segs) > 0) {
    max_good_len = good_segs[, max(len)]

    split_range = segs[len == max_good_len, range(.id)]

    x1 = ctdf[.id < split_range[1]]
    x2 = ctdf[.id > split_range[2]]

    o = list(x1, x2)
  } else {
    o = NULL
  }

  o

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
#'   \code{.segment} column indicating bout membership.
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
  X[, .segment := NA]

  # Initialize
  result = list()
  queue = .split_by_maxlen(X, deltaT = deltaT)
  
  total_n = nrow(X)
  i = 1
  processed_n = 0


  while (i <= length(queue)) {

    current = queue[[i]]

    if (current |> .has_clusters()) {
      new_chunks = .split_by_maxlen(current, deltaT = deltaT)
      queue = c(queue, new_chunks)
    } else {
      result = c(result, list(current))
      processed_n = processed_n + nrow(current)
    }

    i = i + 1
  }
  

  sids = 1:length(result)
  for (i in sids) {
    result[[i]][, .segment := i]
  }
  
  # remove all segments n < 4 (n = 4 == one possible intersection)

  n_by_seg = sapply(result, nrow)
  result = result[n_by_seg > 3]

  o = rbindlist(result)

  setorder(o, .id)
  o[, .segment := factor(.segment) |> fct_inorder() |> as.numeric()]
  o = merge(ctdf[, .(.id)], o[, .(.id, .segment)], all.x = TRUE, sort = FALSE)

  set(ctdf, j = ".segment", value = o$.segment)

}