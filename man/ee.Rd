% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ee.R
\name{ee}
\alias{ee}
\title{Euclidean Error}
\usage{
ee(y, x, method = "median", mean = FALSE)
}
\arguments{
\item{y}{matrix of observations (see details)}

\item{x}{3-dimensional array of ensemble forecasts/samples of a predictive distribution (depending on \code{y}; see details)}

\item{method}{character; "\code{median}" and "\code{mean}"; default: "\code{median}" (see details)}

\item{mean}{logical; if \code{TRUE} the mean of the EE values is calculated for output; if \code{FALSE} the single EE values are used as output; default: \code{FALSE}}
}
\value{
Vector of the score value(s).
}
\description{
This function calculates the Euclidean Error (EE) given observations of a multivariate variable and ensemble forecasts/samples of a predictive distribution.
}
\details{
The observations are given in the matrix \code{y} with n rows, where each column belongs to an univariate observation variable.
The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
entry must be a matrix with n rows, having the same structure as \code{y}, filled with the ensemble forecasts or samples of a predictive distribution.
Only finite values of \code{y} and \code{x} are used.

If method "\code{median}" is specified, the multivariate L1-medians (Vardi et Zhang, 2000) of the i-th third dimension entries of \code{x}
are calculated. If method "\code{mean}" is specified, the sample mean vectors of the
i-th third dimension entries of \code{x} are calculated. In both cases, the number of ensemble forecasts or
samples of a predictive distribution should be "large", e.g. 10.000.

A lower EE indicates a better forecast.
}
\examples{
#simulated data
n <- 30
m <- 10000
y <- cbind(rnorm(n), rgamma(n, shape = 1))
x <- array(NA, dim = c(m, 2, n))
x[, 1, ] <- rnorm(n*m)
x[, 2, ] <- rgamma(n*m, shape = 1)

#ee calculation
ee(y = y, x = x, method = "median", mean = FALSE)
ee(y = y, x = x, method = "median", mean = TRUE)

ee(y = y, x = x, method = "mean", mean = FALSE)
ee(y = y, x = x, method = "mean", mean = TRUE)

}
\references{
Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.

Vardi, Y. and Zhang, C. (2000). The multivariate L1-median and associated data depth. Proceedings of the National Academy of Science of the United States of America, 97, 1423-1426.
}
\author{
David Jobst
}
