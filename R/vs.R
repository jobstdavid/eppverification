#' Variogram Score
#'
#' This function calculates the Variogram Score (VS) given observations of a multivariate variable and samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of samples of a predictive distribution (depending on \code{y}; see details)
#' @param w quadratic matrix with non-negative weights; default: \code{NULL}, i.e. matrix with constant weights 1 (see details)
#' @param p positive value; default: \code{p = 0.5} (see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' The observations are given in the matrix \code{y} with n rows and d columns, where each column belongs to an univariate observation variable.
#' The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
#' entry of \code{x} must be a matrix with n rows, having the same structure as \code{y}, filled with samples of a multivariate predictive distribution.
#' The quadratic weight matrix \code{w} must have d columns and rows.
#'
#' A lower VS indicates a better forecast.
#'
#' @return
#' Vector of the score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' m <- 50
#' y <- cbind(rnorm(n), rgamma(n, shape = 1))
#' x <- array(NA, dim = c(m, 2, n))
#' x[, 1, ] <- rnorm(n*m)
#' x[, 2, ] <- rgamma(n*m, shape = 1)
#' w <- matrix(c(0.2, 0.3, 0.3, 0.7), ncol = 2, byrow = TRUE)
#' p <- 2
#'
#' # vs calculation
#' vs(y = y, x = x)
#' vs(y = y, x = x, aggregate = mean)
#'
#' vs(y = y, x = x, w = w, p = p)
#' vs(y = y, x = x, w = w, p = p, aggregate = mean)
#'
#' @references
#' Scheurer, M. and Hamill, T. (2015). Variogram-based proper scoring rules for probabilistic forecasts of multivariate quantities. Monthly Weather Review, 143, 1321-1334.
#'
#' @author David Jobst
#'
#' @rdname vs
#'
#' @export
vs <- function(y, x, p = 0.5, w = NULL, na.action = na.omit, aggregate = FALSE, ...) {
  # y is a matrix where the columns represent the obs. variables and the rows stand for the time points
  # x is a 3-dimensional array, where each matrix in that array stands for a time point.
  # In each matrix the columns represent the obs. variables and the rows represent the number of samples

  if (!is.matrix(y)) {
    stop("'y' should be a matrix!")
  }
  if (!is.array(x)) {
    stop("'x' should be a 3-dimensional array!")
  }
  dimensions <- apply(x, 3, dim)
  if (nrow(y) != ncol(dimensions)) {
    stop("The third dimension of 'x' and the number of rows of 'y' are not equal!")
  }
  if(length(unique(dimensions[1, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of rows!")
  }
  if(length(unique(dimensions[2, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of columns!")
  }
  if(dimensions[2, 1] != ncol(y)) {
    stop("The number of columns of the entries of 'x' is not equal with the number of columns of 'y'!")
  }

  if (is.null(w)) {
    w <- matrix(1, ncol(y), ncol(y))
  } else {
    if (!is.matrix(w)) {
      stop("'w' should be a quadratic matrix!")
    }
    if (!isSymmetric(w)) {
      stop("'w' should be a symmetric matrix!")
    }
    if (any(as.vector(w) < 0)) {
      stop("'w' contains negative values!")
    }
  }


  if (p <= 0) {
    stop("'p' must be positive!")
  }


  vs.values <- vs_cpp(y = y, x = x, w = w, p = p)

  vs.values <- as.vector(na.action(vs.values))
  if (!isFALSE(aggregate)) {
    vs.values <- do.call(aggregate, list(vs.values, ...))
  }

  return(as.numeric(vs.values))

}
