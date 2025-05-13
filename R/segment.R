
#' segment ctdf
#' segment_ctdf

#' @export
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2 )
#' se = segment_ctdf(ctdf, deltaT = 12) 
#' mapview(se, zcol = "bout_id")
#' 
segment_ctdf <- function(ctdf, deltaT = 12, n_segments = 5, bout_length) {

  # TODO bout_length

  if (!inherits(ctdf, "ctdf")) {
    stop("filter_intersection() only works on objects of class 'ctdf'")
  }


  segs = ctdf |>
    as_ctdf_track()
  
  crs = st_crs(ctdf)

  ints = st_crosses(segs) 

  setDT(segs)


  pruned_ints = lapply(seq_along(ints), function(i) {
    j = ints[[i]]

    dfs = difftime(segs$start[j], segs$stop[i], units = "hours") |> abs()

    j[dfs <= deltaT]


  })

  segs[, n_ints := lengths(pruned_ints) ]
  segs[, no_cross := n_ints == 0]

  segs[, bout_id := rleid(no_cross)]
  segs[, N_bout_id := .N, bout_id]
  segs[(!no_cross) | N_bout_id <= n_segments, let(bout_id = NA, N_bout_id = NA)]
  segs[, bout_id := factor(bout_id) |> as.integer()]
  segs[, let(n_ints = NULL, no_cross = NULL,  N_bout_id = NULL)]

  st_as_sf(segs) |>
    st_set_crs(crs)

}