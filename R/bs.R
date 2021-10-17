#' Brier Score
#'
#' This function calculates the Brier Score (BS) given observations of a one-dimensional variable, thresholds and corresponding PIT values of a predictive distribution.
#'
#' @param y vector of observations
#' @param z vector of threshold values (depending on \code{y}; see details)
#' @param p vector of PIT values in [0,1] (depending on \code{z}; see details)
#' @param mean logical; if \code{TRUE} the mean of the BS values is calculated for output; if \code{FALSE} the single BS values are used as output; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, the i-th entry of \code{y} belongs to the i-th entry
#' of \code{z} and \code{p}. For given thresholds \code{z} the PIT values are obtained by
#' \code{p}=F(\code{z}) for a predictive distribution F. Only finite values of \code{y}, \code{z} and \code{p} are used.
#'
#' A lower BS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' y <- rnorm(n)
#' z <- sample(-10:35, size = n, replace = TRUE)
#' p <- pnorm(z)
#'
#' #bs calculation
#' bs(y = y, z = z, p = p, mean = FALSE)
#' bs(y = y, z = z, p = p, mean = TRUE)
#'
#' @references
#' Brier, G. (1950). Verification of forecasts expressed in terms of probability. Monthly Weather Review, 78, 1-3.
#'
#' @author David Jobst
#'
#' @rdname bs
#'
#' @export
bs <- function(y, z, p, mean = FALSE) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.vector(z)) {
    stop("'z' should be a vector!")
  }
  if (!is.vector(p)) {
    stop("'p' should be a vector!")
  }
  if (length(y) != length(p) || length(y) != length(z)) {
    stop("Lengths of 'y', 'z' and 'p' are not equal!")
  }

  #prepare data
  data <- cbind(y, z, p)
  data <- matrix(data[apply(is.finite(data), 1, all), ], ncol = 3)
  y <- data[, 1]
  z <- data[, 2]
  p <- data[, 3]

  if (any(p > 1) || any(p < 0))
    stop("'p' values have to be in the interval [0,1]!")

  bs.value <- (p - 1*(y <= z))^2

  if (mean == TRUE) {
    bs.value <- mean(bs.value)
  }
  return(as.numeric(bs.value))

}


