#' Euclidean Error
#'
#' This function calculates the Euclidean Error (EE) given observations of a multivariate variable and samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of samples of a multivariate predictive distribution (depending on \code{y}; see details)
#' @param method character; "\code{median}" and "\code{mean}"; default: "\code{median}" (see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' The observations are given in the matrix \code{y} with n rows, where each column belongs to an univariate observation variable.
#' The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
#' entry must be a matrix with n rows, having the same structure as \code{y}, filled with the samples of a multivariate predictive distribution.
#'
#' If method "\code{median}" is specified, the multivariate L1-medians (Vardi et Zhang, 2000) of the i-th third dimension entries of \code{x}
#' are calculated. If method "\code{mean}" is specified, the sample mean vectors of the
#' i-th third dimension entries of \code{x} are calculated. In both cases, the amount of
#' samples of the multivariate predictive distribution should be "large", e.g. 10.000.
#'
#' A lower EE indicates a better forecast.
#'
#' @return
#' Vector of the score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' m <- 10000
#' y <- cbind(rnorm(n), rgamma(n, shape = 1))
#' x <- array(NA, dim = c(m, 2, n))
#' x[, 1, ] <- rnorm(n*m)
#' x[, 2, ] <- rgamma(n*m, shape = 1)
#'
#' # ee calculation
#' ee(y = y, x = x, method = "median")
#' ee(y = y, x = x, method = "median", aggregate = mean)
#'
#' ee(y = y, x = x, method = "mean")
#' ee(y = y, x = x, method = "mean", aggregate = mean)
#'
#' @references
#' Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.
#'
#' Vardi, Y. and Zhang, C. (2000). The multivariate L1-median and associated data depth. Proceedings of the National Academy of Science of the United States of America, 97, 1423-1426.
#'
#' @author David Jobst
#'
#' @rdname ee
#'
#' @importFrom pcaPP l1median_VaZh
#' @importFrom Rfast rowsums colmeans
#' @export
ee <- function(y, x, method = "median", na.action = na.omit, aggregate = FALSE, ...) {
  #y is a matrix where the columns represent the obs. variables and the rows stand for the time points
  #x is a 3-dimensional array, where each matrix in that array stands for a time point. In each matrix the columns represent the obs. variables
  #and the rows represent the number of samples

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



  if (method == "median") {
    med <- t(sapply(1:dim(x)[3], function(i) l1median_VaZh(x[, , i], maxit = 10000)$par))
    # med <- l1median_cpp(X = x, maxit = 10000, tol = 1e-8, zero_tol = 1e-15)
    ee.values <- sqrt(rowsums((y-med)^2, parallel = TRUE))
  } else if (method == "mean") {
    mu <- t(sapply(1:dim(x)[3], function(i) colmeans(x[, ,i], parallel = TRUE)))
    ee.values <- sqrt(rowsums((y-mu)^2, parallel = TRUE))
  } else {
    stop("This method is not available!")
  }

  ee.values <- as.vector(na.action(ee.values))
  if (!isFALSE(aggregate)) {
    ee.values <- do.call(aggregate, list(ee.values, ...))
  }

  return(ee.values)
}
