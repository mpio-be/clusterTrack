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
#' @details  The output includes additional columns for step duration (`step_duration`), 
#' step distance (`step_distance` in meters), and speed (`speed` in km/h). 
#' The number of rows is nrow(ctdf) - i, where i = 1 and corresponds to the starting index in ctdf.
#'
#'
#' @examples
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = 'locationDate')
#' s = track_segments(ctdf)
#' plot(s[c('timestamp', 'speed')])
#'
#' @export
track_segments <- function(ctdf) {
  
  if (!inherits(ctdf, "ctdf")) {
    stop("track_segments() only works on objects of class 'ctdf'")
  }

  o = ctdf |>
    st_as_sf() |>  
    mutate(
      location_prev  = lag(location),
      timestamp_prev = lag(timestamp)
    )

  o = o |> filter(!st_is_empty(location_prev))

  o = o |>
    rowwise() |>
    mutate(
      segment = list(st_linestring(
        rbind(st_coordinates(location_prev), st_coordinates(location))
      )),
      step_duration = difftime(timestamp, timestamp_prev, units = "hours") |> as.numeric(),
      step_distance = geodist(
        st_coordinates(location),
        st_coordinates(location_prev),
        paired = TRUE,
        measure = "haversine"
      )
    ) |>
    ungroup()
  
  o = o |>
    select(-timestamp_prev, -location_prev) |>
    mutate(
      segment = st_sfc(segment, crs = st_crs(ctdf)),
      step_distance = set_units(step_distance, "m"),
      step_duration = set_units(step_duration, "hours"),
      speed = set_units(step_distance / step_duration, "km/h")
    )

  o 

}