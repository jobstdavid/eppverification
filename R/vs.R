#' Variogram Score
#'
#' This function calculates the Variogram Score (VS) given observations of a multivariate variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of ensemble forecasts/samples of a predictive distribution (depending on \code{y}; see details)
#' @param w Quadratic matrix with nonnegative weights; default: matrices with constant weight 1 (see details)
#' @param p positive value; default: \code{p = 0.5} (see details)
#' @param mean logical; if \code{TRUE} the mean of the VS values is calculated for output; if \code{FALSE} the single VS values are used as output; default: \code{FALSE}
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' The observations are given in the matrix \code{y} with n rows and d columns, where each column belongs to an univariate observation variable.
#' The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
#' entry of \code{x} must be a matrix with n rows, having the same structure as \code{y}, filled with the ensemble forecasts or samples of a predictive distribution.
#' The quadratic weight matrix \code{w} must have d columns and rows.
#'
#' A lower VS indicates a better forecast.
#'
#' @return
#' Vector of the score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' m <- 50
#' y <- cbind(rnorm(n), rgamma(n, shape = 1))
#' x <- array(NA, dim = c(m, 2, n))
#' x[, 1, ] <- rnorm(n*m)
#' x[, 2, ] <- rgamma(n*m, shape = 1)
#' w <- matrix(c(0.2, 0.8, 0.3, 0.7), ncol = 2, byrow = TRUE)
#' p <- 1
#'
#' #vs calculation
#' vs(y = y, x = x, mean = FALSE)
#' vs(y = y, x = x, mean = TRUE)
#'
#' vs(y = y, x = x, w = w, p = p, mean = FALSE)
#' vs(y = y, x = x, w = w, p = p, mean = TRUE)
#'
#' @references
#' Scheurer, M. and Hamill, T. (2015). Variogram-based proper scoring rules for probabilistic forecasts of multivariate quantities. Monthly Weather Review, 143, 1321-1334.
#'
#' @author David Jobst
#'
#' @rdname vs
#'
#' @export
vs <- function(y, x, w = NULL, p = NULL, mean = FALSE, na.rm = FALSE) {
  #y is a matrix where the columns represent the obs. variables and the rows stand for the time points
  #x is a 3-dimensional array, where each matrix in that array stands for a time point. In each matrix the columns represent the obs. variables
  #and the rows represent the number of ensemble members/samples

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
    if (any(as.vector(w) < 0)) {
      stop("'w' contains negative values!")
    }
  }


  if (is.null(p)) {
    p <- 0.5
  } else {
    if (p <= 0) {
      stop("'p' must be positive!")
    }
  }

  vs.values <- vs_cpp(y = y, x = x, w = w, p = p)

  if (na.rm == TRUE) {
    vs.values <- as.vector(na.omit(vs.values))
  }

  if (mean == TRUE) {
    vs.values <- mean(vs.values)
  }

  return(as.numeric(vs.values))
}
