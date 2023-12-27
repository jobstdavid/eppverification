#' Continuous Ranked Probability Score
#'
#' This function calculates the Continuous Ranked Probability Score (CRPS) given observations of a univariate variable and samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of samples of a predictive distribution (depending on \code{y}; see details)
#' @param method character; "\code{ens}" and "\code{sml}". Default: "\code{ens}" (see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' For a vector \code{y} of length n, \code{x} should be given as matrix
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution.
#'
#' If method "\code{ens}" is specified, the CRPS values are calculated for
#' given ensemble forecasts in \code{x} (Grimit et al., 2006).
#' If method "\code{sml}" is specified, the CRPS values are calculated for a "small" number of
#' given ensemble forecasts in \code{x} (Ferro et al., 2008).
#'
#' A lower CRPS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' m1 <- 50
#' m2 <- 3
#' m3 <- 10000
#' y <- rnorm(n)
#' x1 <- matrix(rnorm(n*m1), ncol = m1)
#' x2 <- matrix(rnorm(n*m2), ncol = m2)
#' x3 <- matrix(rnorm(n*m3), ncol = m3)
#'
#' # crps calculation
#' crps(y = y, x = x1, method = "ens")
#' crps(y = y, x = x1, method = "ens", aggregate = mean)
#'
#' crps(y = y, x = x2, method = "sml")
#' crps(y = y, x = x2, method = "sml", aggregate = mean)
#'
#' @references
#' Matheson, J. Winkler, R. (1976). Scoring Rules for Continuous Probability Distributions. 22(10), 1087-1096.
#'
#' Ferro, C., Richardson, D. and Weigel, A. (2008). On the effect of ensemble size on the discrete and continuous ranked probability scores. Meteorological Applications, 15, 19-24.
#'
#' Grimit, E., Gneiting, T., Berrocal, V. and Johnson, N. (2006). The continuous ranked probability score for circular variables and its applications to mesoscale forecast ensemble verification. Quarterly Journal of the Royal Meteorological Society, 132, 2925-2942.
#'
#'
#' @author David Jobst
#'
#' @rdname crps
#'
#' @importFrom stats na.omit
#'
#' @export
crps <- function(y, x, method = "ens", na.action = na.omit, aggregate = FALSE, ...) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.matrix(x)) {
    stop("'x' should be a matrix!")
  }
  if (length(y) != nrow(x)) {
    stop("Length of 'y' is not equal to the number of rows of 'x'!")
  }

  if (method == "ens") {
      crps.value <- crps_cpp(y = y, x = x)
  } else if (method == "sml") {
      crps.value <- crps_sml_cpp(y = y, x = x)
  } else {
      stop("This method is not available!")
  }

  crps.value <- as.vector(na.action(crps.value))
  if (!isFALSE(aggregate)) {
    crps.value <- do.call(aggregate, list(crps.value, ...))
  }

  return(as.numeric(crps.value))

}

