#' Euclidean Error
#'
#' This function calculates the Euclidean Error (EE) given observations of a multivariate variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of ensemble forecasts/samples of a predictive distribution (depending on \code{y}; see details)
#' @param method character; "\code{median}" and "\code{mean}"; default: "\code{median}" (see details)
#' @param mean logical; if \code{TRUE} the mean of the EE values is calculated for output; if \code{FALSE} the single EE values are used as output; default: \code{FALSE}
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' The observations are given in the matrix \code{y} with n rows, where each column belongs to an univariate observation variable.
#' The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
#' entry must be a matrix with n rows, having the same structure as \code{y}, filled with the ensemble forecasts or samples of a predictive distribution.
#' Only finite values of \code{y} and \code{x} are used.
#'
#' If method "\code{median}" is specified, the multivariate L1-medians (Vardi et Zhang, 2000) of the i-th third dimension entries of \code{x}
#' are calculated. If method "\code{mean}" is specified, the sample mean vectors of the
#' i-th third dimension entries of \code{x} are calculated. In both cases, the number of ensemble forecasts or
#' samples of a predictive distribution should be "large", e.g. 10.000.
#'
#' A lower EE indicates a better forecast.
#'
#' @return
#' Vector of the score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' m <- 10000
#' y <- cbind(rnorm(n), rgamma(n, shape = 1))
#' x <- array(NA, dim = c(m, 2, n))
#' x[, 1, ] <- rnorm(n*m)
#' x[, 2, ] <- rgamma(n*m, shape = 1)
#'
#' #ee calculation
#' ee(y = y, x = x, method = "median", mean = FALSE)
#' ee(y = y, x = x, method = "median", mean = TRUE)
#'
#' ee(y = y, x = x, method = "mean", mean = FALSE)
#' ee(y = y, x = x, method = "mean", mean = TRUE)
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
ee <- function(y, x, method = "median", mean = FALSE, na.rm = FALSE) {
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

  if (na.rm == TRUE) {
    ee.values <- as.vector(na.omit(ee.values))
  }

  if (mean == TRUE) {
    ee.values <- mean(ee.values, na.rm = TRUE)
  }

  return(ee.values)
}
