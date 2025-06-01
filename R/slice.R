
#' segment and filter a ctdf
#' slice_ctdf

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf, deltaT = 6) |> m('.filter')
#' 
slice_ctdf <- function(ctdf, deltaT = 24, slice_method = mean ) {

  if (!inherits(ctdf, "ctdf")) {
    stop("slice_ctdf() only works on objects of class 'ctdf'")
  }

  # make segments
  segs = 
    smooth_ctdf(ctdf) |>
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

  # slice
  segs[, .filter := FALSE]
  segs[, can_be_sliced := fifelse(len > slice_method(len), TRUE, FALSE)]
  segs[(can_be_sliced & !cross), .filter := TRUE]

  segs[, .segment  := rleid(.filter)  ]

  o = rbind(segs[1, .(.filter, .segment)], segs[, .(.filter, .segment)])


  # set
  ctdf[, let(.segment = o$.segment, .filter = o$.filter)]

}