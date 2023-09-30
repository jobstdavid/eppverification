#' Absolute Error
#'
#' This function calculates the Absolute Error (AE) given observations of a one-dimensional variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of ensemble forecasts/samples of a predictive distribution or vector of medians of ensemble forecasts/a predictive distribution (depending on \code{y}; see details)
#' @param mean logical; if \code{TRUE} the mean of the AE values is calculated for output; if \code{FALSE} the single AE values are used as output; default: \code{FALSE}
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{x} can be given as matrix of ensemble forecasts/samples of a predictive distribution
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution
#' or ensemble forecasts. The row-wise medians are determined by its sample version.
#'
#' If the medians of ensemble forecasts or of a predictive distribution are directly
#' available, \code{x} can be given as vector of medians, where
#' the i-th entry of \code{y} belongs to the i-th entry of \code{x}.
#'
#' In addition, with this function, the Mean Absolute Error (MAE) can be calculated.
#'
#' A lower AE indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' m <- 50
#' y <- rnorm(n)
#' x1 <- matrix(rnorm(n*m), ncol = m)
#' x2 <- apply(x1, 1, median)
#'
#' #ae calculation
#' ae(y = y, x = x1, mean = FALSE)
#' ae(y = y, x = x1, mean = TRUE)
#'
#' ae(y = y, x = x2, mean = FALSE)
#' (mae <- ae(y = y, x = x2, mean = TRUE))
#'
#' @references
#' Vannitsem, S., Wilks, D. and Messner, J. (2018). Statistical Postprocessing of Ensemble Forecasts. Elsevier. 164.
#'
#' @author David Jobst
#'
#' @rdname ae
#'
#' @importFrom stats median na.omit
#' @importFrom Rfast rowMedians
#'
#' @export
ae <- function(y, x, mean = FALSE, na.rm = FALSE) {
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

  if (na.rm == TRUE) {
    ae.value <- as.vector(na.omit(ae.value))
  }

  if (mean == TRUE) {
    ae.value <- mean(ae.value)
  }
  return(as.numeric(ae.value))

}



