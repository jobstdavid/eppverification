#' Logarithmic Score
#'
#' This function calculates the Logarithmic Score (LogS) given density values of observations of a uni- or multivariate-dimensional predictive distribution.
#'
#' @param y vector of density values
#' @param mean logical; if \code{TRUE} the mean of the LogS values is calculated for output; if \code{FALSE} the single LogS values are used as output; default: \code{FALSE}
#'
#' @details
#' For a predictive density function f the density values \code{y} of observations \code{z} are
#' obtained by \code{y} = f(\code{z}). For a "large" number of ensemble forecasts, the density function(s)
#' of the ensemble forecasts may be approximated by using, e.g. kernel density
#' estimation or by fitting a parametric distribution.
#' Only finite values of \code{y} are used.
#'
#' A lower LogS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' x <- sample(-10:35, size = n, replace = TRUE)
#' y <- dnorm(x = x, mean = 0, sd = 1)
#'
#' #logs calculation
#' logs(y = y)
#' logs(y = y, mean = TRUE)
#'
#' @references
#' Good, I. (1952). Rational decisions. Journal of the Royal Statistical Society Ser. B, 14, 107-114.
#'
#' @author David Jobst
#'
#' @rdname logs
#'
#' @export
logs <- function(y, mean = FALSE) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  #allow only finite values for y
  y <- y[is.finite(y)]
  if (any(y <= 0)) {
    stop("'y' should contain values > 0!")
  }

  logs.value <- -log(y)

  if (mean == TRUE) {
    logs.value <- mean(logs.value)
  }
  return(as.numeric(logs.value))
}

