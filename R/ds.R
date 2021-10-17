#' Determinant Sharpness
#'
#' This function calculates the Determinant Sharpness (DS) given covariance matrices of ensemble forecasts/a predictive distribution.
#'
#' @param x 3-dimensional array of covariance matrices of ensemble forecasts/a predictive distribution (see details)
#' @param mean logical; if \code{TRUE} the mean of the DS values is calculated for output; if \code{FALSE} the single DS values are used as output; default: \code{FALSE}
#'
#' @details
#' Each third dimension entry of the array \code{x} must contain a (symmetric) covariance matrix of ensemble forecasts/a predictive distribution.
#' This covariance matrices are either analytically available or must be estimated by e.g. the sample covariance matrix.
#' Only covariance matrices with finite values in \code{x} are used.
#'
#' A lower DS indicates a sharper forecast.
#'
#' @return
#' Vector of the score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' m <- 50
#' x <- array(NA, dim = c(2, 2, n))
#' for (i in 1:n) {
#' x[, , i] <- cov(cbind(rnorm(m), rgamma(m, shape = 1)))
#' }
#'
#' #ds calculation
#' ds(x = x, mean = FALSE)
#' ds(x = x, mean = TRUE)
#'
#' @references
#' Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.
#'
#' @author David Jobst
#'
#' @rdname ds
#'
#' @export
ds <- function(x, mean = FALSE) {
  #x is a 3-dimensional array, where each matrix entry stands for a time point.
  #Each matrix in that array has to be a covariance matrix.
  if (!is.array(x)) {
    stop("'x' should be a 3-dimensional array!")
  }
  symmetry <- apply(x, 3, isSymmetric)
  if (!all(symmetry)) {
    stop("'x' should contain symmetric matrices!")
  }
  dimensions <- apply(x, 3, dim)
  if(length(unique(dimensions[1, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of rows!")
  }
  if(length(unique(dimensions[2, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of columns!")
  }

  ds.value <- c()
  d <- ncol(x[, , 1])
  index <- which(apply(apply(x, 3, is.finite), 2, all))

  for (i in index) {
    det.x <- det(x[, , i])
    det.x[det.x < 0] <- NA
    ds.value <- c(ds.value, det.x^(1/(2*d)))
  }

  if (mean == TRUE) {
    ds.value <- mean(ds.value, na.rm = TRUE)
  }

  return(ds.value)
}
