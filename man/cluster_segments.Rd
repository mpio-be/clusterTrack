% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{cluster_segments}
\alias{cluster_segments}
\title{Cluster segments}
\usage{
cluster_segments(ctdf, threshold = 0.75, method = "quantile")
}
\arguments{
\item{ctdf}{A `ctdf`.}
}
\value{
NULL.  The `ctdf` object is updated in place.
}
\description{
Cluster segments
}
\examples{
data(toy_ctdf_k2)
ctdf = as_ctdf(toy_ctdf_k2, s_srs = 4326, t_srs = "+proj=eqearth")
slice_ctdf(ctdf)
cluster_segments(ctdf )

data(lbdo66867)
ctdf = as_ctdf(lbdo66867, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
slice_ctdf(ctdf)
cluster_segments(ctdf)

data(pesa56511)
ctdf  = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth")
slice_ctdf(ctdf) 
cluster_segments(ctdf) 

}
