#' Determinant Sharpness
#'
#' This function calculates the Determinant Sharpness (DS) given covariance matrices of a predictive distribution.
#'
#' @param x 3-dimensional array of samples or covariance matrices of a predictive distribution (see details)
#' @param covmat logical; if \code{TRUE} the covariance matrices of a predictive distribution are provided in \code{x} by the user;
#' if \code{FALSE} the sample covariance matrix is calculated based on the samples of a predictive distribution contained in \code{x}; default: \code{FALSE}
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' Each third dimension entry of the array \code{x} must contain a (symmetric) covariance matrix or samples of a multivariate predictive distribution.
#' This covariance matrices are either analytically available or must are estimated by e.g. the sample covariance matrix.
#'
#' A lower DS indicates a sharper forecast.
#'
#' @return
#' Vector of the score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' m <- 50
#' x <- array(NA, dim = c(2, 2, n))
#' for (i in 1:n) {
#' x[, , i] <- cov(cbind(rnorm(m), rgamma(m, shape = 1)))
#' }
#' z <- array(rnorm(10000*5*365), dim = c(10000, 5, 365))
#'
#' # ds calculation with provided covariance matrix
#' ds(x = x, covmat = TRUE)
#' ds(x = x, covmat = TRUE, aggregate = mean)
#'
#' # ds calculation without provided covariance matrix
#' ds(x = z, covmat = FALSE)
#' ds(x = z, covmat = FALSE, aggregate = mean)
#'
#' @references
#' Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.
#'
#' @author David Jobst
#'
#' @rdname ds
#'
#' @importFrom Rfast cova
#' @export
ds <- function(x, covmat = FALSE, na.action = na.omit, aggregate = FALSE, ...) {
  # x is a 3-dimensional array, where each matrix entry stands for a time point.
  # each matrix in that array has to be a covariance matrix.
  if (!is.array(x)) {
    stop("'x' should be a 3-dimensional array!")
  }
  dimensions <- apply(x, 3, dim)
  if(length(unique(dimensions[1, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of rows!")
  }
  if(length(unique(dimensions[2, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of columns!")
  }

  if (covmat) {

    # check for symmetry
    symmetry <- apply(x, 3, isSymmetric)
    if (!all(symmetry)) {
      stop("'x' should contain only symmetric matrices!")
    }

    # check for positive semi-definite
    evalues <- as.vector(sapply(1:dim(x)[3], function(i) eigen(x[, , i])$values))
    if (any(evalues < 0)) {
      stop("'x' should contain only positive semi-definite matrices!")
    }


    det.x <- sapply(1:dim(x)[3], function(i) det(x[, , i]))
    det.x[det.x < 0] <- NA # not absolutely necessary due to the semi-definite matrices
    ds.values <- det.x^(1/(2*dim(x)[2]))

  } else {

    det.x <- ds_cpp(x)
    det.x[det.x < 0] <- NA # not absolutely necessary due to the semi-definite matrices
    ds.values <- det.x^(1/(2*dim(x)[2]))

  }

  ds.values <- as.vector(na.action(ds.values))
  if (!isFALSE(aggregate)) {
    ds.values <- do.call(aggregate, list(ds.values, ...))
  }

  return(ds.values)

}


