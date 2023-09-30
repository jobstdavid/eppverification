#' Root Mean Variance
#'
#' This function calculates the Root Mean Variance (RMV) given ensemble forecasts/samples of a predictive distribution.
#'
#' @param x vector of variances or matrix of ensemble forecasts/samples of a predictive distribution (see details)
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' If \code{x} is provided as matrix, it should contain the
#' ensemble forecasts or the samples of a predictive distribution in each row. Consequently,
#' the variances are calculated row-wise by its sample version.
#' If \code{x} is provided as vector, it should contain the variances of the
#' ensemble forecasts or of a predictive distribution.
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
#' @importFrom stats na.omit
#' @importFrom Rfast rowVars
#'
#' @export
rmv <- function(x, na.rm = FALSE) {
  if (is.matrix(x)) {

    # prepare data
    if(na.rm) {
      x <- na.omit(x)
    }

    #use sample variance
    variance <- rowVars(x, parallel = TRUE, na.rm = FALSE)
    rmv.value <- sqrt(mean(variance))
  } else if (is.vector(x)) {

    # prepare data
    if(na.rm) {
      x <- x[is.finite(x)]
    }

    if (any(x < 0)) {
      stop("'x' should contain values >= 0!")
    }
    rmv.value <- sqrt(mean(x))
  } else {
    stop("'x' should be a vector or matrix!")
  }
  return(as.numeric(rmv.value))
}

