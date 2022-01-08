#' Expectation of the Probability Integral Transform Values
#'
#' This function calculates the Expectation value of the Probability Integral Transform values (E(PIT)).
#'
#' @param u vector of PIT values in [0,1] (see details)
#'
#' @details
#' The vector \code{u} contains the PIT values \code{u}=F(\code{x}) for a predictive
#' distribution F and argument \code{x}.
#' Only finite values of \code{u} are used.
#' The expectation value is calculated by the sample mean of the PIT values.
#'
#' The expectation value of the PIT values (E(PIT)) provides information on the bias of a predictive distribution.
#' An expectation of the PIT values equal to 1/2 corresponds to the expectation of the uniform distribution
#' on [0,1], which is desirable. Any deviation from 1/2 indicates that the predictive distribution is biased.
#'
#' @return
#' Expectation value of the PIT values.
#'
#' @examples
#' #simulated data
#' n <- 10000
#' u <- runif(n)
#'
#' #E(PIT) calculcation
#' m.pit(u = u)
#'
#' @references
#' Gneiting, T. and Ranjan, R. (2013). Combining predictive distributions. Electronic Journal of Statistics, 7, 1747-1782.
#'
#' @author David Jobst
#'
#' @rdname m.pit
#'
#' @export
m.pit <- function(u) {
  if (!is.vector(u)) {
    stop("'u' should be a vector!")
  }
  #allow only finite values for u
  u <- u[is.finite(u)]
  if (any(u > 1) || any(u < 0))
    stop("'u' values have to be in the interval [0,1]!")
  mean(as.numeric(u))

}

