#' Variance of the Probability Integral Transform Values
#'
#' This function calculates the variance of the Probability Integral Transform values (Var(PIT)).
#'
#' @param u vector of PIT values in [0,1] (see details)
#'
#' @details
#' The vector \code{u} contains the PIT values \code{u}=F(\code{x}) for a predictive
#' distribution F and argument \code{x}.
#'
#' The variance of the PIT values (Var(PIT)) provides information on the dispersion of a predictive distribution.
#' A variance of the PIT values equal to 1/12 ≈ 0.0833 corresponds to the variance of the uniform distribution
#' on [0,1], which is desirable. A variance greater than 1/12 indicates underdispersion and a
#' variance smaller than 1/12 indicates overdispersion of the predictive distribution.
#'
#' @return
#' Variance of the PIT values.
#'
#' @examples
#' #simulated data
#' n <- 10000
#' u <- runif(n)
#'
#' #Var(PIT) calculcation
#' var.pit(u = u)
#'
#' @references
#' Gneiting, T. and Ranjan, R. (2013). Combining predictive distributions. Electronic Journal of Statistics, 7, 1747-1782.
#'
#' @author David Jobst
#'
#' @rdname var.pit
#'
#' @importFrom stats var
#' @export
var.pit <- function(u) {
  if (!is.vector(u)) {
    stop("'u' should be a vector!")
  }

  #allow only finite values for u
  u <- u[is.finite(u)]

  if (any(u > 1) || any(u < 0))
    stop("'u' values have to be in the interval [0,1]!")
  var(as.numeric(u))
}

