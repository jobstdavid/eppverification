#' Dawid-Sebastiani Score
#'
#' This function calculates the Dawid-Sebastiani Score (DSS) given observations of a one-dimensional variable and ensemble forecasts/samples of a predictive distribution/parameters.
#'
#' @param y vector of observations
#' @param x matrix of ensemble forecasts/samples of a predictive distribution or vector of variances of ensemble forecasts/a predictive distribution (depending on \code{y}; see details)
#' @param mu if \code{NULL}, \code{mu} is calculated by the row-wise mean of matrix \code{x}; otherwise \code{mu} must be provided as the means of the ensemble forecasts/predictive distribution; default: \code{NULL} (depending on \code{x}; see details)
#' @param mean logical; if \code{TRUE} the mean of the DSS values is calculated for output; if \code{FALSE} the single DSS values are used as output; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{x} can be given as matrix of ensemble forecasts/samples of a predictive distribution
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution
#' or ensemble forecasts. Consequently \code{mu} must be \code{NULL}. The row-wise
#' means and variances are determined by its sample versions.
#'
#' If the variances and means of ensemble forecasts or of a predictive distribution are directly
#' available, \code{x} can be given as vector of variances and \code{mu} can be given as vector of means, where
#' the i-th entry of \code{y} belongs to the i-th entry of \code{x} and \code{mu}.
#' Only finite values of \code{y}, \code{x} and \code{mu} are used.
#'
#' A lower DSS indicates a better forecast.
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
#' x2 <- rep(1, n)
#' mu <- rep(0, n)
#'
#' #dss calculation
#' dss(y = y, x = x1, mu = NULL, mean = FALSE)
#' dss(y = y, x = x1, mu = NULL, mean = TRUE)
#'
#' dss(y = y, x = x2, mu = mu, mean = FALSE)
#' dss(y = y, x = x2, mu = mu, mean = TRUE)
#'
#' @references
#' Dawid, A. and Sebastiani, P. (1999). Coherent disperion criteria for optimal experimental design. Annals of Statistics, 27, 65-81.
#'
#' @author David Jobst
#'
#' @rdname dss
#'
#' @export
dss <- function(y, x, mu = NULL, mean = FALSE) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (is.null(mu)) {
    if (!is.matrix(x)) {
      stop("'x' should be a matrix!")
    }
    if (length(y) != nrow(x)) {
      stop("Length of 'y' is not equal to the number of rows of 'x'!")
    }
    #allow only finite values for y
    index <- which(is.finite(y))
    y <- y[index]
    x <- matrix(x[index, ], nrow = length(index))

    out <- apply(x, 1, mu.var.i)
    dss.value <- sapply(1:length(out), function(j) log(out[[j]]$var) + (y[j]-out[[j]]$mu)^2/out[[j]]$var)
  } else {
    if (!is.vector(x)) {
      stop("'x' should be a vector!")
    }
    if (!is.vector(mu)) {
      stop("'mu' should be a vector!")
    }
    if (length(y) != length(x) || length(y) != length(mu)) {
      stop("Lengths of 'y', 'x' and 'mu' are not equal!")
    }

    #prepare data
    data <- cbind(y, x, mu)
    data <- matrix(data[apply(is.finite(data), 1, all), ], ncol = 3)
    y <- data[, 1]
    x <- data[, 2]
    mu <- data[, 3]

    if (any(x <= 0)) {
      stop("'x' should contain values > 0!")
    }

    dss.value <- log(x) + (y-mu)^2/x
  }

  if (mean == TRUE) {
    dss.value <- mean(dss.value)
  }
  return(as.numeric(dss.value))

}
#'
#' internal function
#' @noRd
mu.var.i <- function(z) {
  z <- z[is.finite(z)]
  mu <- mean(z)
  var <- var(z)
  out <- list(mu = mu, var = var)
  return(out)
}


