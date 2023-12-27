#' Bias based on the Expectation of the Probability Integral Transform
#'
#' This function calculates bias based on the expectation of the Probability Integral Transform (E(PIT)).
#'
#' @param u vector of PIT values in [0,1] (see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#'
#' @details
#' The vector \code{u} contains the PIT values \code{u}=F(\code{x}) for a predictive
#' distribution F and argument \code{x}.
#' The expectation is calculated in terms of the sample mean of the PIT values.
#'
#' The expectation of the PIT values (E(PIT)) provides information on the bias of a calibrated predictive distribution.
#' An expectation of the PIT values equal to 1/2 corresponds to the expectation of the uniform distribution
#' on [0,1], which is desirable. Any deviation from 1/2 indicates that the predictive distribution is biased.
#'
#' @return
#' Expectation in terms of the sample mean of the PIT values.
#'
#' @examples
#' # simulated data
#' n <- 10000
#' u <- runif(n)
#'
#' # bias calculation
#' bias(u = u)
#'
#' @references
#' Gneiting, T. and Ranjan, R. (2013). Combining predictive distributions. Electronic Journal of Statistics, 7, 1747-1782.
#'
#' Taillardat, M. et al. (2016). Calibrated Ensemble Forecasts Using Quantile Regression Forests and Ensemble Model Output Statistics. Monthly Weather Review, 144(6), 2375-2393.
#'
#' @author David Jobst
#'
#' @rdname bias
#'
#' @export
bias <- function(u, na.action = na.omit) {
  if (!is.vector(u)) {
    stop("'u' should be a vector!")
  }

  # handle NA
  u <- as.vector(na.action(u))

  if (any(u > 1) || any(u < 0)) {
    stop("'u' values have to be in the interval [0,1]!")
  }

  mean(as.numeric(u))

}

