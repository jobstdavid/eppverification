#' Absolute Error
#'
#' This function calculates the Absolute Error (AE) given observations of a univariate variable and samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of samples of a predictive distribution or vector containing the medians of a predictive distribution (depending on \code{y}; see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' For a vector \code{y} of length n, \code{x} can be given as matrix of samples of a predictive distribution
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution.
#' The row-wise medians are determined by its sample version.
#'
#' If the medians of a predictive distribution are directly
#' available, \code{x} can be given as vector containing the medians, where
#' the i-th entry of \code{y} belongs to the i-th entry of \code{x}.
#'
#' In addition, with this function, the Mean Absolute Error (MAE) can be calculated by specifying \code{aggregate = mean}.
#'
#' A lower AE indicates a better forecast.
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
#' x2 <- apply(x1, 1, median)
#'
#' # ae calculation
#' ae(y = y, x = x1)
#' ae(y = y, x = x1, aggregate = mean)
#'
#' ae(y = y, x = x2)
#' (mae <- ae(y = y, x = x2, aggregate = mean))
#'
#' @references
#' Gneiting, T. (2011). Making and Evaluating Point Forecasts. Journal of the American Statistical Association, 106(494), 746-762.
#'
#' @author David Jobst
#'
#' @rdname ae
#'
#' @importFrom stats median na.omit
#' @importFrom Rfast rowMedians
#'
#' @export
ae <- function(y, x, na.action = na.omit, aggregate = FALSE, ...) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (is.matrix(x)) {
    if (length(y) != nrow(x)) {
      stop("Length of 'y' is not equal to the number of rows of 'x'!")
    }

    med <- rowMedians(x = x, parallel = TRUE, na.rm = FALSE)
    ae.value <- abs(y-med)
  } else if (is.vector(x)) {
    if (length(y) != length(x)) {
      stop("Length of 'y' is not equal to the length of 'x'!")
    }

    ae.value <- abs(y-x)
  } else {
    stop("'x' should be a vector or matrix!")
  }

  ae.value <- as.vector(na.action(ae.value))
  if (!isFALSE(aggregate)) {
    ae.value <- do.call(aggregate, list(ae.value, ...))
  }

  return(as.numeric(ae.value))

}



