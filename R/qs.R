#' Quantile Score
#'
#' This function calculates the Quantile Score (QS) given observations of an univariate variable, probabilities and corresponding quantiles of a predictive distribution.
#'
#' @param y vector of observations
#' @param q vector of quantile values (depending on \code{p}; see details)
#' @param p vector of probabilities in (0,1)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' For a vector \code{y} of length n, the i-th entry of \code{y} belongs to the i-th entry
#' of \code{q} and \code{p}. For given probabilities \code{p} the quantile values are obtained by
#' \code{q}=F^{-1}(\code{p}) for a predictive distribution F.
#'
#' A lower QS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' # simulated data
#' n <- 30
#' y <- rnorm(n)
#' p <- runif(n)
#' q <- qnorm(p)
#'
#' # qs calculation
#' qs(y = y, q = q, p = p)
#' qs(y = y, q = q, p = p, aggregate = mean)
#'
#' @references
#' Gneiting, T. (2011). Making and Evaluating Point Forecasts. Journal of the American Statistical Association, 106(494), 746-762.
#'
#' @author David Jobst
#'
#' @rdname qs
#'
#' @export
qs <- function(y, q, p, na.action = na.omit, aggregate = FALSE, ...) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.vector(q)) {
    stop("'q' should be a vector!")
  }
  if (!is.vector(p)) {
    stop("'p' should be a vector!")
  }
  if (length(y) != length(q)) {
    stop("Lengths of 'y' and 'q' are not equal!")
  }

  if (any(p >= 1) || any(p <= 0))
    stop("'p' values have to be in the interval (0,1)!")

  qs.value <- (q-y) * (1*(y <= q)-p)

  qs.value <- as.vector(na.action(qs.value))
  if (!isFALSE(aggregate)) {
    qs.value <- do.call(aggregate, list(qs.value, ...))
  }

  return(as.numeric(qs.value))

}


