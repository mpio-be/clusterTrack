% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctdf.R
\name{as_ctdf_track}
\alias{as_ctdf_track}
\title{Convert a `ctdf` track to movement step segments as LINESTRINGs}
\usage{
as_ctdf_track(ctdf)
}
\arguments{
\item{ctdf}{A `ctdf` object (with ordered rows and a `"location"` geometry column).}
}
\value{
An `sf` object with LINESTRING geometry for each step.
}
\description{
Takes a `ctdf` object and returns an `sf` object with LINESTRING geometries representing
the movement steps between consecutive locations. Each segment connects two points,
starting at the previous location and ending at the current one - i.e., each segment
ends at the position of the current row.
}
\details{
The number of rows is nrow(ctdf) - i, where i = 1 and corresponds to the starting index in ctdf.
}
\examples{
data(toy_ctdf_k3)
ctdf = as_ctdf(toy_ctdf_k3)
s = as_ctdf_track(ctdf)
plot(s['.id'])

}
