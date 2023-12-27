#' Squared Error
#'
#' This function calculates the Squared Error (SE) given observations of an univariate variable and samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of samples of a predictive distribution or vector containing the means of a predictive distribution (depending on \code{y}; see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' For a vector \code{y} of length n, \code{x} can be given as matrix of samples of a predictive distribution
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution.
#' The row-wise means are determined by its sample version.
#'
#' If the means of a predictive distribution are directly
#' available, \code{x} can be given as vector of means, where
#' the i-th entry of \code{y} belongs to the i-th entry of \code{x}.
#'
#' In addition, with this function, the Mean Squared Error (MSE) by specifying \code{aggregate = mean} and
#' Root Mean Squares Error (RMSE) can be calculated (see examples).
#'
#' A lower SE indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' m <- 50
#' y <- rnorm(n)
#' x1 <- matrix(rnorm(n*m), ncol = m)
#' x2 <- apply(x1, 1, mean)
#'
#' # ae calculation
#' se(y = y, x = x1)
#' se(y = y, x = x1, aggregate = mean)
#'
#' se(y = y, x = x2)
#' (mse <- se(y = y, x = x2, aggregate = mean))
#' (rmse <- sqrt(mse))
#'
#' @references
#' Gneiting, T. (2011). Making and Evaluating Point Forecasts. Journal of the American Statistical Association, 106(494), 746-762.
#'
#' @author David Jobst
#'
#' @rdname se
#'
#' @importFrom stats na.omit
#' @importFrom Rfast rowmeans
#'
#' @export
se <- function(y, x, na.action = na.omit, aggregate = FALSE, ...) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }

  if (is.matrix(x)) {
    if (length(y) != nrow(x)) {
      stop("Length of 'y' is not equal to the number of rows of 'x'!")
    }

    mu <- rowmeans(x)

    se.value <- (y-mu)^2

  } else if (is.vector(x)) {
    if (length(y) != length(x)) {
      stop("Length of 'y' is not equal to the length of 'x'!")
    }

    se.value <- (y-x)^2
  } else {
    stop("'x' should be a vector or matrix!")
  }

  se.value <- as.vector(na.action(se.value))
  if (!isFALSE(aggregate)) {
    se.value <- do.call(aggregate, list(se.value, ...))
  }

  return(as.numeric(se.value))

}


