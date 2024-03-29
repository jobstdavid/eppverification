% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvr.hist.R
\name{mvr.hist}
\alias{mvr.hist}
\title{Multivariate Verification Rank Histogram}
\usage{
mvr.hist(
  y,
  x,
  method = "mv",
  type = "relative",
  bins = NULL,
  title = NULL,
  reliability = FALSE,
  entropy = FALSE,
  na.rm = FALSE
)
}
\arguments{
\item{y}{matrix of observations (see details)}

\item{x}{3-dimensional array of samples of a predictive distribution (depending on \code{y}; see details)}

\item{method}{character; "\code{mv}", "\code{avg}", "\code{mst}", "\code{bd}"; default: "\code{mv}" (see details)}

\item{type}{character; "\code{relative}", "\code{absolute}" and "\code{density}"; default: "\code{relative}" (see details)}

\item{bins}{numeric; if \code{NULL} the number of bins is equal to \code{nrow(x[, , 1])+1}; otherwise \code{bins} must be chosen so that \code{(nrow(x[, , 1])+1)/bins} is an integer; default: \code{NULL} (see details)}

\item{title}{character; title of the plot; default: "\code{Multivariate Verification Rank Histogram}"}

\item{reliability}{logical; if \code{TRUE} the multivariate reliability index is calculated for the plot (see details); if \code{FALSE} the multivariate reliability index is not calculated; default: \code{FALSE}}

\item{entropy}{logical; if \code{TRUE} the entropy is calculated for the plot (see details); if \code{FALSE} the entropy  is not calculated; default: \code{FALSE}}

\item{na.rm}{logical; if \code{TRUE} NA are stripped before the rank computation proceeds; if \code{FALSE} NA are used in the rank computation; default: \code{FALSE}}
}
\value{
ggplot object with a plot of the Multivariate Verification Rank Histogram.
}
\description{
This function plots the Multivariate Verification Rank Histogram (MVRH) given observations of a multivariate variable and samples of a predictive distribution.
}
\details{
The observations are given in the matrix \code{y} with n rows, where each column belongs to an univariate observation variable.
The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
entry must be a matrix with n rows, having the same structure as \code{y}, filled with the samples of a multivariate predictive distribution.

The parameter \code{bins} specifies the number of columns for the MVRH. For "large"
\code{ncol(x[, , 1])} it is often reasonable to reduce the resolution of the MVRH by
using \code{bins} so that \code{(ncol(x[, , 1])+1)/bins} is an integer.

For the calculation of the ranks, different methods are available, where "\code{mv}" stands for "multivariate ranks",
"\code{avg}" stands for "average ranks", "\code{mst}" stands for "minimum-spanning-tree ranks" and
"\code{bd}" stands for "band-depth ranks". These methods are implemented as described in e.g. Thorarinsdottir et al. (2016).

If \code{type} is "\code{relative}" the relative frequencies of the bins are plotted.
If \code{type} is "\code{absolute}" the absolute frequencies of the bins are plotted.
If \code{type} is "\code{density}" the relative densities of the bins are plotted.

An uniform MVRH indicates a calibrated predictive distribution. Depending on the chosen method, we have the
following interpretation:
\itemize{
 \item{}{"\code{mv}" and "\code{avg}": A ∩-shape in the
MVRH indicates overdispersion and a ∪-shape indicates underdispersion
of the predictive distribution. A systematic bias of the predictive distribution
results in a triangular shaped MVRH histogram.}
 \item{}{"\code{mst}" and "\code{bd}": Too many low ranks indicate underdispersion or bias of
the predictive distribution. Too many high ranks indicate overdispersion or bias of
the predictive distribution.}
}

The deviation from uniformity of the MVRH can be quantified by the multivariate reliability index (RI).
The smaller the RI, the better is the calibration of the forecast. The optimal value of the RI is 0.

The entropy is a tool to assess the calibration of a forecast. The optimal
value of the entropy is 1, representing a calibrated forecast.
}
\examples{
# simulated data
n <- 30
m <- 50
y <- cbind(rnorm(n), rgamma(n, shape = 1))
x <- array(NA, dim = c(m, 2, n))
x[, 1, ] <- rnorm(n*m)
x[, 2, ] <- rgamma(n*m, shape = 1)

# mvr.hist plot
mvr.hist(y = y, x = x)
mvr.hist(y = y, x = x, bins = 17, title = "MVRH",
reliability = TRUE, entropy = FALSE)
mvr.hist(y = y, x = x, bins = 3, method = "avg", type = "absolute",
reliability = FALSE, entropy = TRUE)
mvr.hist(y = y, x = x, bins = 3, method = "bd", type = "density",
reliability = TRUE, entropy = TRUE)

}
\references{
Delle Monache, L., Hacker, J., Zhou, Y., Deng, X. and Stull, R., (2006). Probabilistic aspects of meteorological and ozone regional ensemble forecasts. Journal of Geophysical Research: Atmospheres, 111, D24307.

Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.

Smith, L. and Hansen, J. (2004). Extending the limits of ensemble forecast verification with the minimum spanning tree. Monthly Weather Review, 132, 1522-1528.

Taillardat, M., Mestre, O., Zamo, M. and Naveau, P., (2016). Calibrated Ensemble Forecasts Using Quantile Regression Forests and Ensemble Model Output Statistics. American Meteorological Society, 144, 2375-2393.

Thorarinsdottir, T., Scheurer, M. and Heinz, C. (2016). Assessing the calibration of high-dimensional ensemble forecasts using rank histograms. Journal of Computational and Graphical Statistics, 25, 105-122.

Tribus, M. (1969). Rational Descriptions, Descisions and Designs. Pergamon Press.

Wilks, D. (2004). The minimum spanning tree histogram as verification tool for multidimensional ensemble forecasts. Monthly Weather Review, 132, 1329-1340.
}
\author{
David Jobst
}
