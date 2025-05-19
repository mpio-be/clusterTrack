
#' segment and filter a ctdf
#' slice_ctdf

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' se = st_as_sf(ctdf)
#' mapviewOptions(fgb = FALSE)
#' mapview(se, zcol = ".filter")
#' mapview(se, zcol = ".segment")
#' 
slice_ctdf <- function(ctdf, deltaT = 12, slice_min_n_segments = 5, slice_min_bout_length, filter_min_n_segments = 5) {


  if (!inherits(ctdf, "ctdf")) {
    stop("slice_ctdf() only works on objects of class 'ctdf'")
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
  segs[, len := st_length(segment)]

  # segmentation
    doseg = segs[(!cross) & N_bout_id > slice_min_n_segments]
    doseg[, bout_id := factor(bout_id) |> as.integer()]
    # mid segments timestamps
    midseg = ctdf[ doseg[, round(median(.id) ), by = bout_id]$V1 ]$timestamp

  # filter
    dofill = segs[N_bout_id >=filter_min_n_segments,]$.id

  # set
    ctdf[, .segment := findInterval(timestamp, midseg)]
    ctdf[.id %in% dofill, .filter := TRUE]




}