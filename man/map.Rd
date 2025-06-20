% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterTrack.Vis.R
\name{map}
\alias{map}
\title{Map a ctdf object}
\usage{
map(ctdf)
}
\arguments{
\item{ctdf}{A `ctdf` object.}
}
\description{
Visualize clustered track data on an interactive map.
}
\examples{
data(pesa56511)
ctdf  = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth") |>cluster_track()
map(ctdf)
}
