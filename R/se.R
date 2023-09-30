#' Squared Error
#'
#' This function calculates the Squared Error (SE) given observations of a one-dimensional variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of ensemble forecasts/samples of a predictive distribution or vector of means of ensemble forecasts/a predictive distribution (depending on \code{y}; see details)
#' @param mean logical; if \code{TRUE} the mean of the SE values is calculated for output; if \code{FALSE} the single SE values are used as output; default: \code{FALSE}
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{x} can be given as matrix of ensemble forecasts/samples
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution
#' or ensemble forecasts. The row-wise means are determined by its sample version.
#'
#' If the means of ensemble forecasts or of a predictive distribution are directly
#' available, \code{x} can be given as vector of means, where
#' the i-th entry of \code{y} belongs to the i-th entry of \code{x}.
#'
#' In addition, with this function, the Mean Squared Error (MSE) and Root Mean Squares Error (RMSE) can be calculated (see examples).
#'
#' A lower SE indicates a better forecast.
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
#' x2 <- apply(x1, 1, mean)
#'
#' #ae calculation
#' se(y = y, x = x1, mean = FALSE)
#' se(y = y, x = x1, mean = TRUE)
#'
#' se(y = y, x = x2, mean = FALSE)
#' (mse <- se(y = y, x = x2, mean = TRUE))
#' (rmse <- sqrt(mse))
#'
#' @references
#' Vannitsem, S., Wilks, D. and Messner, J. (2018). Statistical Postprocessing of Ensemble Forecasts. Elsevier. 164.
#'
#' @author David Jobst
#'
#' @rdname se
#'
#' @importFrom stats na.omit
#' @importFrom Rfast rowmeans
#'
#' @export
se <- function(y, x, mean = FALSE, na.rm = FALSE) {
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

  if (na.rm == TRUE) {
    se.value <- as.vector(na.omit(se.value))
  }

  if (mean == TRUE) {
    se.value <- mean(se.value)
  }
  return(as.numeric(se.value))

}


