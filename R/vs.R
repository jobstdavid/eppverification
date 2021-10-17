#' Variogram Score
#'
#' This function calculates the Variogram Score (VS) given observations of a multivariate variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of ensemble forecasts/samples of a predictive distribution (depending on \code{y}; see details)
#' @param w 3-dimensional array of quadratic matrices with nonnegative weights; default: matrices with constant weight 1 (see details)
#' @param p vector with positive values; default: vector with constant value 0.5 (see details)
#' @param mean logical; if \code{TRUE} the mean of the VS values is calculated for output; if \code{FALSE} the single VS values are used as output; default: \code{FALSE}
#'
#' @details
#' The observations are given in the matrix \code{y} with n rows and d columns, where each column belongs to an univariate observation variable.
#' The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x} and \code{w} and to the i-th entry of \code{p}. The i-th third dimension
#' entry of \code{x} must be a matrix with n rows, having the same structure as \code{y}, filled with the ensemble forecasts or samples of a predictive distribution.
#' Only finite values of \code{y}, \code{x}, \code{w} and \code{p} are used.
#'
#' The 3-dimensional array \code{w} must contain quadratic matrices of dimension d with nonnegative weights.
#'
#' The vector \code{p} of length n must contain positive values
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
#' w <- array(NA, dim = c(2, 2, n))
#' for (i in 1:n) {
#' w[, , i] <- matrix(runif(4), ncol = 2)
#' }
#' p <- runif(n)
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
vs <- function(y, x, w = NULL, p = NULL, mean = FALSE) {
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
    w <- array(1, dim = c(ncol(y), ncol(y), nrow(y)))
  } else {
    if (!is.array(w)) {
      stop("'w' should be a 3-dimensional array!")
    }
    dimensions <- apply(w, 3, dim)
    if (nrow(y) != ncol(dimensions)) {
      stop("The third dimension of 'w' and the number of rows of 'y' are not equal!")
    }
    if(length(unique(dimensions[1, ])) != 1) {
      stop("The entries of 'w' don't have equal numbers of rows!")
    }
    if(length(unique(dimensions[2, ])) != 1) {
      stop("The entries of 'w' don't have equal numbers of columns!")
    }
    if(dimensions[2, 1] != ncol(y)) {
      stop("The number of columns of the entries of 'w' is not equal with the number of columns of 'y'!")
    }
    if(!all(is.finite(as.vector(w)))) {
      stop("A matrix of 'w' contains non-finite values!")
    }
    if (any(as.vector(w) < 0)) {
      stop("A matrix of 'w' contains negative values!")
    }
  }


  if (is.null(p)) {
    p <- rep(0.5, nrow(y))
  } else {
    if (!is.vector(p) || length(p) != nrow(y)) {
      stop("'p' must be a vector of length nrow(y)!")
    }
    if (any(p <= 0)) {
      stop("'p' must have positive entries!")
    }
  }

  d <- ncol(y)
  i1 <- rep(1:(d-1), (d-1):1)
  i2 <- unlist(sapply(2:d, function(k) seq(k, d) ))
  index <- which(apply(is.finite(y), 1, all))
  vs.value <- c()

  for (i in index) {
    data <- x[, , i]
    data <- matrix(data[apply(is.finite(data), 1, all), ], ncol = ncol(data))
    weight <- w[, , i]
    weight <- weight + t(weight)
    vxy <- (abs(y[i, i1] - y[i, i2])^p[i] - mean(abs(data[, i1] - data[, i2])^p[i]))^2
    vs.value <- c(vs.value, weight[lower.tri(weight, diag = FALSE)] %*% vxy)
  }

  if (mean == TRUE) {
    vs.value <- mean(vs.value)
  }

  return(as.numeric(vs.value))
}
