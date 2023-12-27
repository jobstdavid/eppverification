#' Brier Score
#'
#' This function calculates the Brier Score (BS) given observations of an univariate variable, thresholds and corresponding PIT values of a predictive distribution.
#'
#' @param y vector of observations
#' @param t vector of threshold values (depending on \code{y}; see details)
#' @param u vector of PIT values in [0,1] (depending on \code{t}; see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' For a vector \code{y} of length n, the i-th entry of \code{y} belongs to the i-th entry
#' of \code{u} and \code{t}. For given thresholds \code{t} the PIT values are obtained by
#' \code{u}=F(\code{t}) for a predictive distribution F.
#'
#' A lower BS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' y <- rnorm(n)
#' t <- sample(-10:35, size = n, replace = TRUE)
#' u <- pnorm(t)
#'
#' # bs calculation
#' bs(y = y, t = t, u = u)
#' bs(y = y, t = t, u = u, aggregate = mean)
#'
#' @references
#' Brier, G. (1950). Verification of forecasts expressed in terms of probability. Monthly Weather Review, 78, 1-3.
#'
#' @author David Jobst
#'
#' @rdname bs
#'
#' @importFrom stats na.omit
#'
#' @export
bs <- function(y, t, u, na.action = na.omit, aggregate = FALSE, ...) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.vector(t)) {
    stop("'t' should be a vector!")
  }
  if (!is.vector(u)) {
    stop("'u' should be a vector!")
  }
  if (length(y) != length(u)) {
    stop("Lengths of 'y' and 'u' are not equal!")
  }

  if (any(u > 1) || any(u < 0))
    stop("'u' values have to be in the interval [0,1]!")

  bs.value <- (u - 1*(y <= t))^2

  bs.value <- as.vector(na.action(bs.value))
  if (!isFALSE(aggregate)) {
    bs.value <- do.call(aggregate, list(bs.value, ...))
  }

  return(as.numeric(bs.value))

}


