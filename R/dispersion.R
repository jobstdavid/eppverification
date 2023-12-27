#' Dispersion based on the Variance of the Probability Integral Transform
#'
#' This function calculates dispersion based on the variance of the Probability Integral Transform (Var(PIT)).
#'
#' @param u vector of PIT values in [0,1] (see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#'
#' @details
#' The vector \code{u} contains the PIT values \code{u}=F(\code{x}) for a predictive
#' distribution F and argument \code{x}.
#' The variance is calculated in terms of the sample variance of the PIT values.
#'
#' The variance of the PIT values (Var(PIT)) provides information on the dispersion of a calibrated predictive distribution.
#' A variance of the PIT values equal to 1/12 ≈ 0.0833 corresponds to the variance of the uniform distribution
#' on [0,1], which is desirable. A variance greater than 1/12 indicates underdispersion and a
#' variance smaller than 1/12 indicates overdispersion of the predictive distribution.
#'
#' @return
#' Variance in terms of the sample variance of the PIT values.
#'
#' @examples
#' # simulated data
#' n <- 10000
#' u <- runif(n)
#'
#' # dispersion calculation via Var(PIT)
#' dispersion(u = u)
#'
#' @references
#' Gneiting, T. and Ranjan, R. (2013). Combining predictive distributions. Electronic Journal of Statistics, 7, 1747-1782.
#'
#' @author David Jobst
#'
#' @rdname dispersion
#'
#' @importFrom stats var
#' @export
dispersion <- function(u, na.action = na.omit) {
  if (!is.vector(u)) {
    stop("'u' should be a vector!")
  }

  # handle NA
  u <- as.vector(na.action(u))

  if (any(u > 1) || any(u < 0)) {
    stop("'u' values have to be in the interval [0,1]!")
  }

  var(as.numeric(u))

}

