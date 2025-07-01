
.check_ctdf <- function(x) {
  
  if (!inherits(x, "ctdf")) {
    stop("Not a 'ctdf' object!",call. = FALSE)
    }

  if(is.unsorted(x$timestamp)) {
    stop("It seems this ctdf is not sorted anymore along timestamp!", call. = FALSE)
  }

  nams = c(".id", ".segment", "cluster", "location", "timestamp")
  nams_ok = nams %in% names(x)
  
  if(!all(nams_ok)) {
    stop("Some build in columns are missing", call. = FALSE)
  }



}

#' @export
as_ctdf <- function(x, ...) {
  UseMethod("as_ctdf")
}

#' @export
as_ctdf.default <- function(x, ...) {
  stop("No method for objects of class ", class(x))
}

#' @export
plot.ctdf <- function(x,y = NULL,  pch = 16) {
  
  tr = as_ctdf_track(x)
  xs = st_as_sf(x) 

  plot(st_geometry(tr), col = "#706b6b")


  plot(xs |>st_geometry(), pch = pch, add = TRUE)

  plot(x[1, .(location)] |> st_as_sf(), col = "#1900ff", size = 3, pch = 16, add = TRUE)
  plot(x[nrow(x), .(location)] |> st_as_sf(), col = "#ff0000", size = 3, pch = 16, add = TRUE)
}

#' Coerce an object to clusterTrack data format
#'
#' Converts an object with spatial coordinates and a timestamp column
#' to a standardized `sf/data.table`-based format used internally by the clusterTrack package.
#'
#' @param x       A `data.frame` object.
#' @param coords  Character vector of length 2 specifying the coordinate column names.
#'                Defaults to `c("longitude", "latitude")`.
#' @param time    Name of the time column. Will be renamed to `"timestamp"` internally.
#' @param s_srs   Source spatial reference. Default is 4326. 
#' @param t_srs  target spatial reference passed to `st_transform()`. Default is "+proj=eqearth".
#'
#' @param ...     Currently unused
#'

#' @return An object of class `ctdf`, which inherits from `sf`.
#'
#' @note
#' This is currently a thin wrapper around `st_as_sf()`, but standardizes timestamp naming, ordering,
#' and geometry column name (`"location"`). A colum `filter`, and with all values set to `FALSE` is added as well.
#' This column will be updated by upstream methods.
#'
#' @examples
#' data(toy_ctdf_k2)
#' x = as_ctdf(toy_ctdf_k2)
#' plot(x)
#'
#' @export

as_ctdf <- function(x, coords = c("longitude", "latitude"), time = "time", s_srs = 4326, t_srs = "+proj=eqearth", ...) {
  

  reserved = intersect(names(x), c(".segment", ".id", "cluster"))

  if (length(reserved) > 0) {
    warning(
      sprintf(
        "as_ctdf(): input contains reserved column name%s: %s which may be overwritten here or by upstream methods.",
        if (length(reserved) > 1) "s" else "",
        paste(reserved, collapse = ", ")
      )
    )
  }

  o =  as.data.table(x)
  setnames(o, c(coords, time), c("X", "Y", "timestamp"))

  dups = which(duplicated(o[, .(Y, X, timestamp)]))
  if (length(dups) > 0) {
    stop(
      sprintf(
        "as_ctdf(): found %d duplicated point%s (latitude, longitude, timestamp) at row%s: %s",
        length(dups),
        if (length(dups) > 1) "s" else "",
        if (length(dups) > 1) "s" else "",
        paste(dups, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  setorder(o, timestamp)

  o[, .id := .I]
  o[, .segment := NA_integer_]
  o[, cluster  := NA_integer_]

  o = st_as_sf(o, coords = c("X", "Y"), crs = s_srs)

  o = st_transform(o, crs = t_srs)

  st_geometry(o) = "location"

  setDT(o)
  setcolorder(o, c(".id", ".segment", "cluster", "location", "timestamp"))

  class(o) <- c("ctdf", class(o))
  o
}


#' Convert a `ctdf` track to movement step segments as LINESTRINGs
#'
#' Takes a `ctdf` object and returns an `sf` object with LINESTRING geometries representing
#' the movement steps between consecutive locations. Each segment connects two points,
#' starting at the previous location and ending at the current one - i.e., each segment
#' ends at the position of the current row.
#'
#' @param ctdf A `ctdf` object (with ordered rows and a `"location"` geometry column).
#'
#' @return An `sf` object with LINESTRING geometry for each step.
#'
#' @details The number of rows is nrow(ctdf) - i, where i = 1 and corresponds to the starting index in ctdf.
#'
#'
#' @examples
#' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2)
#' s = as_ctdf_track(ctdf)
#' plot(s['.id'])
#'
#' @export
as_ctdf_track <- function(ctdf) {
  
  .check_ctdf(ctdf)


  o = ctdf |>
    st_as_sf() |>
    mutate(
      location_prev  = lag(location),
      start          = lag(timestamp), 
      stop           = timestamp
    )
  this_crs = st_crs(o)

  o = o |>
    dplyr::filter(!st_is_empty(location_prev))

  o |>
    rowwise() |>
    mutate(
      track = rbind(st_coordinates(location_prev), st_coordinates(location)) |> st_linestring()|>list()
    ) |>
    ungroup() |>
    st_set_geometry("track") |>
    select(.id, .segment, start, stop, track)|>
    st_set_crs(this_crs)


}


#' Summarise a ctdf by cluster
#'
#' Returns one row per `cluster` with start/stop times, tenure (days),
#' convex‐hull centroid and row‐count.
#'
#' @param object A `ctdf` (inherits `data.table`).
#' @param ...  Currently ignored.
#' @return A `data.table` (and `data.frame`) of class c("summary_ctdf","data.table","data.frame").
#' @export
#' @examples 
#' #' data(toy_ctdf_k2)
#' ctdf = as_ctdf(toy_ctdf_k2)
#' cluster_track(ctdf)
#' summary(ctdf)
#' 
#' 
summary.ctdf = function(ctdf, ...) {
  
  .check_ctdf(ctdf)

  # TODO: pre-cluster summary. 

  out = 
  ctdf[cluster > 0, .(
    start    = min(timestamp),
    stop     = max(timestamp),
    geometry = st_union(location) |> st_convex_hull() |> st_centroid(),
    segment  = unique(.segment),
    N        = .N
  ), by = cluster]

  out[, tenure := difftime(stop, start, units = "days")]

  out[, next_geom := geometry[ shift(seq_len(.N), type = "lead") ] ]

  out[, dist_to_next := st_distance(geometry, next_geom, by_element = TRUE)]

  out[, next_geom := NULL]

  class(out) = c("summary_ctdf", class(out))
  out
}