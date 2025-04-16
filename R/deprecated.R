
#' @title Deprecated: use as_ctdf instead
#' @description Please use [as_ctdf()] in new code.
#' @keywords internal

#' @export
as_tdbscan <- function(x, ...) {
  .Deprecated("as_ctdf", package = "clusterTrack")
  as_ctdf(x, ...)
}
