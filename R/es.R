#' Energy Score
#'
#' This function calculates the Energy Score (ES) given observations of a multivariate variable and samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of samples of a multivariate predictive distribution (depending on \code{y}; see details)
#' @param method character; "\code{ens}" and "\code{mc}"; default: "\code{ens}" (see details)
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
#' If method "\code{ens}" is specified, the ES values are calculated
#' for given samples \code{x} of a multivariate predictive distribution (Gneiting et al., 2008).
#' If method "\code{mc}" is specified, the ES values are calculated by a Monte-Carlo approximation
#' using samples \code{x} of a predictive distribution (Gneiting et al., 2008).
#' In the latter case, the number of samples should be "large", e.g. 10.000.
#'
#' A lower ES indicates a better forecast.
#'
#' @return
#' Vector of the score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' m1 <- 50
#' m2 <- 10000
#' y <- cbind(rnorm(n), rgamma(n, shape = 1))
#' x1 <- array(NA, dim = c(m1, 2, n))
#' x2 <- array(NA, dim = c(m2, 2, n))
#' x1[, 1, ] <- rnorm(n*m1)
#' x1[, 2, ] <- rgamma(n*m1, shape = 1)
#' x2[, 1, ] <- rnorm(n*m2)
#' x2[, 2, ] <- rgamma(n*m2, shape = 1)
#'
#' # es calculation
#' es(y = y, x = x1, method = "ens")
#' es(y = y, x = x1, method = "ens", aggregate = mean)
#'
#' es(y = y, x = x2, method = "mc")
#' es(y = y, x = x2, method = "mc", aggregate = mean)
#'
#' @references
#' Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.
#'
#' @author David Jobst
#'
#' @rdname es
#'
#' @export
es <- function(y, x, method = "ens", na.action = na.omit, aggregate = FALSE, ...) {
  # y is a matrix where the columns represent the obs. variables and the rows stand for the dates
  # x is a 3-dimensional array, where each matrix in that array stands for a date. In each matrix the columns represent the obs. variables
  # and the rows represent the number of samples

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

  if (method == "ens") {
    es.values <- es_cpp(y = y, x = x)
  } else if (method == "mc") {
    es.values <- es_mc_cpp(y = y, x = x)
  } else {
    stop("This method is not available!")
  }

  es.values <- as.vector(na.action(es.values))
  if (!isFALSE(aggregate)) {
    es.values <- do.call(aggregate, list(es.values, ...))
  }

  return(as.numeric(es.values))

}


