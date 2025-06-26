

.has_clusters <- function(s ) {


  if (nrow(s) <= 3) {
    return(FALSE)
  }

  MIN_PTS =  ceiling(sqrt(nrow(s)))

  o = hdbscan(st_coordinates(s$location), minPts = MIN_PTS )

  return(length(o$cluster_scores) > 1)
  

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


#' segment and filter a ctdf
#' slice_ctdf

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, s_srs = 4326, t_srs = "+proj=eqearth")
#' ctdf = slice_ctdf(ctdf)  
#' 
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
#' slice_ctdf(ctdf )   




slice_ctdf <- function(ctdf, deltaT = 30) {

  .check_ctdf(ctdf)

  X = copy(ctdf)
  X[, .segment := NA]

  # Initialize
  result = list()
  queue = .split_by_maxlen(X, deltaT = deltaT)
  total_n = nrow(X)
  i = 1
  processed_n = 0
  pb = txtProgressBar(min = 0, max = 0.9, style = 1, char = "░")

  while (i <= length(queue)) {
    setTxtProgressBar(pb, processed_n / total_n)

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
  close(pb)
  

  sids = 1:length(result)
  for (i in sids) {
    result[[i]][, .segment := i]
  }
  

  o = rbindlist(result)
  setorder(o, .id)
  o[, .segment := factor(.segment) |> fct_inorder() |> as.numeric()]
  o = merge(ctdf[, .(.id)], o[, .(.id, .segment)], all.x = TRUE, sort = FALSE)

  set(ctdf, j = ".segment", value = o$.segment)

}