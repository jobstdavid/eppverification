#' Logarithmic Score
#'
#' This function calculates the Logarithmic Score (LogS) given density values of observations of an uni- or multivariate predictive distribution.
#'
#' @param y vector of density values
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' For a predictive density function f the density values \code{y} of observations \code{x} are
#' obtained by \code{y} = f(\code{x}).
#'
#' A lower LogS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' x <- sample(-10:35, size = n, replace = TRUE)
#' y <- dnorm(x = x, mean = 0, sd = 1)
#'
#' # logs calculation
#' logs(y = y)
#' logs(y = y, aggregate = mean)
#'
#' @references
#' Good, I. (1952). Rational decisions. Journal of the Royal Statistical Society Ser. B, 14, 107-114.
#'
#' @author David Jobst
#'
#' @rdname logs
#'
#' @export
logs <- function(y, na.action = na.omit, aggregate = FALSE, ...) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }

  logs.value <- -log(y)

  logs.value <- as.vector(na.action(logs.value))
  if (!isFALSE(aggregate)) {
    logs.value <- do.call(aggregate, list(logs.value, ...))
  }

  return(as.numeric(logs.value))
}

