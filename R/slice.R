
#' segment and filter a ctdf
#' slice_ctdf

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf, deltaT = 6)  
#' 
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf )   



slice_ctdf <- function(ctdf, deltaT = 24, slice_method = median, minN = 5) {

  if (!inherits(ctdf, "ctdf")) {
    stop("slice_ctdf() only works on objects of class 'ctdf'")
  }

  # make segments
  segs =
    ctdf |>  
    # smooth_ctdf() |>
    as_ctdf_track()
    
  crs = st_crs(segs)

  ints = st_crosses(segs) 

  setDT(segs)

  pruned_ints = lapply(seq_along(ints), function(i) {
    j = ints[[i]]

    dfs = difftime(segs$start[j], segs$stop[i], units = "hours") |> abs()

    j[dfs <= deltaT]
  })
  
  segs[, n_ints := lengths(pruned_ints) ]
  segs[, cross := n_ints > 0]

  segs[, bout_id := rleid(cross)]
  segs[, len := st_length(track) |> set_units("km") |> as.numeric()]
  segs[, len := sum(len), bout_id]
  segs[, n := .N, .segment]

  # slice
  segs[, .filter := FALSE]
  segs[, can_be_sliced := fifelse(len > slice_method(len), TRUE, FALSE)]
  segs[(can_be_sliced & !cross & n >= minN), .filter := TRUE]  

  segs[, .segment  := rleid(.filter)  ]

  o = rbind(segs[1, .(.filter, .segment)], segs[, .(.filter, .segment)])


  # set
  ctdf[, let(.segment = o$.segment, .filter = o$.filter)]

}


split_maxlen <- function(ctdf, deltaT = 24, minN = 5) {

  if (!inherits(ctdf, "ctdf")) {
    stop("slice_ctdf() only works on objects of class 'ctdf'")
  }

  # make segments
  segs =
    ctdf |>  
    as_ctdf_track()
    
  crs = st_crs(segs)

  ints = st_crosses(segs) 

  setDT(segs)

  pruned_ints = lapply(seq_along(ints), function(i) {
    j = ints[[i]]

    dfs = difftime(segs$start[j], segs$stop[i], units = "hours") |> abs()

    j[dfs <= deltaT]
  })
  
  segs[, n_ints := lengths(pruned_ints) ]
  segs[, cross := n_ints > 0]

  segs[, bout_id := rleid(cross)]
  segs[, len := st_length(track) |> set_units("km") |> as.numeric()]
  segs[, len := sum(len), bout_id]
  segs[, n := .N, .segment]

  # slice
  segs[, .filter := FALSE]

  max_good_len = segs[(!cross) & n >= minN, max(len)]

  split_range = segs[len == max_good_len, range(.id)]

  x1 = ctdf[.id < split_range[1]]
  x2 = ctdf[.id > split_range[2]]

  list(x1, x2)

}


slice_by_max_len <- function(ctdf, minN = 5) {


  has_clusters <- function(s) {
    if (nrow(s) < minN) {
      return(FALSE)
    } else {
      o = hdbscan(st_coordinates(s$location), minPts = ceiling(sqrt(nrow(s))))
      ans = length(o$cluster_scores) > 1
    }
    ans
  }

  result = list()
  queue  = split_maxlen(ctdf, minN)

  while (length(queue) > 0) {
    
    current = queue[[1]]
    queue   = queue[-1]

    if (current |> has_clusters() ) {
      queue = c(queue, split_maxlen(current, minN))
    } else {
      result = c(result, list(current))
    }
  
  }

  sids = 1:length(result)
  for (i in sids) {
    result[[i]][, .segment := i]
  }
  
  result = result[sapply(result, nrow) > minN]

  o = rbindlist(result)
  o[, .filter := FALSE]

  class(o) <- c("ctdf", class(o))
  o

}