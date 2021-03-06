% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GraphicFunctions.R
\name{distripolygonVioline}
\alias{distripolygonVioline}
\title{Violine polygon from a distribution}
\usage{
distripolygonVioline(
  distri,
  color,
  ysh = 0,
  yax = FALSE,
  minden = 0.6,
  maxdensity = NULL,
  transform = identity,
  adjust = 1,
  border = NULL
)
}
\arguments{
\item{distri}{A vector containing the distribution to plot}

\item{color}{Color to fill the shape}

\item{ysh}{Shift on the y-axis}

\item{yax}{Do we plot a y-axis?}

\item{minden}{Minimum mode of a density relative to the maximal density of all distributions}

\item{maxdensity}{NULL}

\item{transform}{Function}

\item{adjust}{Smoothing factor in the function density()}

\item{border}{The color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.}
}
\value{
None
}
\description{
Creates violine polygon from a distribution
}
\examples{
distri <- rnorm(2000, 0,1)
plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
distripolygonVioline(distri=distri, color="red")

}
