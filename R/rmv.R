#' Root Mean Variance
#'
#' This function calculates the Root Mean Variance (RMV) given ensemble forecasts/samples of a predictive distribution.
#'
#' @param x vector of variances or matrix of ensemble forecasts/samples of a predictive distribution (see details)
#'
#' @details
#' If \code{x} is provided as matrix, it should contain the
#' ensemble forecasts or the samples of a predictive distribution in each row. Consequently,
#' the variances are calculated row-wise by its sample version.
#' If \code{x} is provided as vector, it should contain the variances of the
#' ensemble forecasts or of a predictive distribution.
#' Only finite values of \code{x} are used.
#'
#' A lower RMV indicates a sharper forecast. The optimal value of the RMV is 0.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' m <- 50
#' x1 <- matrix(rnorm(n*m), ncol = m)
#' x2 <- apply(x1, 1, var)
#'
#' #rmv calculation
#' rmv(x = x1)
#' rmv(x = x2)
#'
#' @references
#' Gneiting, T. and Ranjan, R. (2013). Combining predictive distributions. Electronic Journal of Statistics, 7, 1747-1782.
#'
#' @author David Jobst
#'
#' @rdname rmv
#'
#' @export
rmv <- function(x) {
  if (is.matrix(x)) {
    #use sample variance
    variance <- apply(x, 1, variance.i)
    rmv.value <- sqrt(mean(variance))
  } else if (is.vector(x)) {
    #allow only finite values for x
    x <- x[is.finite(x)]
    if (any(x < 0)) {
      stop("'x' should contain values >= 0!")
    }
    rmv.value <- sqrt(mean(x))
  } else {
    stop("'x' should be a vector or matrix!")
  }
  return(as.numeric(rmv.value))
}
#'
#' internal function
#' @noRd
variance.i <- function(z) {
z <- z[is.finite(z)]
out <- var(z)
return(out)
}
