
#' segment ctdf
#' segment_ctdf

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' segment_ctdf(ctdf, deltaT = 12, min_n_segments = 3)
#' se = st_as_sf(ctdf)
#' mapview(se, zcol = ".segment")
#' 
segment_ctdf <- function(ctdf, deltaT = 12, min_n_segments = 5, min_bout_length) {

  #TODO min_bout_length
  #TODO allow for mid segments timestamps as argument
  #TODO allow for guessing min_n_segments from data
  #TODO segment by season/year. 
  # Alternatively return a new object (ctdf subset for movement only) to be used
  #  for both segment and filter!

  if (!inherits(ctdf, "ctdf")) {
    stop("filter_intersection() only works on objects of class 'ctdf'")
  }

  segs = ctdf |>
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
  segs[, N_bout_id := .N, bout_id]
  segs = segs[(!cross) & N_bout_id > min_n_segments]
  segs[, bout_id := factor(bout_id) |> as.integer()]

  # mid segments timestamps
  midseg = ctdf[ segs[, round(median(.id) ), by = bout_id]$V1 ]$timestamp

  ctdf[, .segment := findInterval(timestamp, midseg)]



}