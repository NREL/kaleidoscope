% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kaleidoscope.R
\name{kde2dw}
\alias{kde2dw}
\title{Two-Dimensional Weighted Kernel Density Estimation}
\usage{
kde2dw(x, y, w, h, n = 25, lims = c(range(x), range(y)))
}
\arguments{
\item{x}{x coordinate of data}

\item{y}{y coordinate of data}

\item{w}{data weights}

\item{h}{vector of bandwidths for x and y directions.  Defaults to
normal reference bandwidth (see ‘bandwidth.nrd’). A scalar
value will be taken to apply to both directions.}

\item{n}{Number of grid points in each direction.  Can be scalar or a
length-2 integer vector.}

\item{lims}{The limits of the rectangle covered by the grid as ‘c(xl, xu,
yl, yu)’.}
}
\description{
A weighted two-dimensional kernel density estimation with an
axis-aligned bivariate normal kernel, evaluated on a square grid. This is
a weighted-version of MASS::kde2d.
}

