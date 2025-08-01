% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlayDensities.R
\name{iscamaddt}
\alias{iscamaddt}
\title{Overlay a t Density Function on Histogram}
\usage{
iscamaddt(
  x,
  df,
  main = "Histogram with t curve",
  xlab = deparse(substitute(x)),
  bins = NULL
)
}
\arguments{
\item{x}{A numeric vector representing the data to be plotted.}

\item{df}{A numeric value representing the degrees of freedom of \code{x}.}

\item{main}{Optional title for the plot}

\item{xlab}{Optional x-axis label for the plot}

\item{bins}{Optional number of bins for the histogram.}
}
\value{
A histogram of x overlayed with an t density function.
}
\description{
Overlay a t Density Function on Histogram
}
\examples{
set.seed(0)
x <- rt(100, 30)
iscamaddt(x, 30)
iscamaddt(x, 30, main = "Your Active Title", xlab = "t Data", bins = 20)
}
