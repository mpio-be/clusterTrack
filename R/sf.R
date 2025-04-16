#' Convert `ctdf` point track to step segments as LINESTRINGs
#'
#' Takes a `ctdf` object and returns
#' an `sf` object of LINESTRINGs representing the movement steps between
#' consecutive locations.
#'
#' @param ctdf A `ctdf` object (or any `sf` POINT object with time-ordered rows).
#'
#' @return An `sf` object with LINESTRING geometry for each step.
#'
#' @examples
#' data(pesa56511)
#' x = as_ctdf(pesa56511, time = 'locationDate')
#' segs = ctdf_to_segments(x)
#' plot(segs)
#'
#' @export
ctdf_to_segments <- function(x) {

  # TODO: slow = TRUE st_distance, else euclidian dist

  x |>
    mutate(
      geom_next = lead(geometry),
      timestamp_next = lead(timestamp)
    ) |>
    filter(!st_is_empty(geom_next)) |>
    rowwise() |>
    mutate(
      segment = list(st_linestring(
        rbind(st_coordinates(geometry), st_coordinates(geom_next))
      )),
      step_duration = as.numeric(difftime(timestamp_next, timestamp, units = "secs")),
      step_distance = st_distance(geometry, geom_next, by_element = TRUE)
    ) |>
    ungroup() |>
    mutate(segment = st_sfc(segment, crs = st_crs(x))) |>
    st_as_sf() |>
    st_set_geometry("segment")
}
