#' Quantile Score
#'
#' This function calculates the Quantile Score (QS) given observations of a one-dimensional variable, probabilities and corresponding quantiles of a predictive distribution.
#'
#' @param y vector of observations
#' @param q vector of quantile values (depending on \code{p}; see details)
#' @param p vector of probabilities in (0,1)
#' @param mean logical; if \code{TRUE} the mean of the QS values is calculated for output; if \code{FALSE} the single QS values are used as output; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, the i-th entry of \code{y} belongs to the i-th entry
#' of \code{q} and \code{p}. For given probabilities \code{p} the quantile values are obtained by
#' \code{q}=F^{-1}(\code{p}) for a predictive distribution F. Only finite values of \code{y}, \code{q} and \code{p} are used.
#'
#' A lower QS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' y <- rnorm(n)
#' p <- runif(n)
#' q <- qnorm(p)
#'
#' #bs calculation
#' qs(y = y, q = q, p = p, mean = FALSE)
#' qs(y = y, q = q, p = p, mean = TRUE)
#'
#' @references
#' Friedrich, P. and Hense, A. (2007). Statistical downscaling of extreme precipitation events using censored quantile regression. Monthly Weather Review, 135, 2365-2378.
#'
#' Gneiting, T. and Raftery, A. (2007). Strictly proper scoring rules, prediction, and estimation. Journal of the American Statistical Association, 102, 359-378.
#'
#' @author David Jobst
#'
#' @rdname qs
#'
#' @export
qs <- function(y, q, p, mean = FALSE) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.vector(q)) {
    stop("'q' should be a vector!")
  }
  if (!is.vector(p)) {
    stop("'p' should be a vector!")
  }
  if (length(y) != length(q) || length(y) != length(p)) {
    stop("Lengths of 'y', 'q' and 'p' are not equal!")
  }

  #prepare data
  data <- cbind(y, q, p)
  data <- matrix(data[apply(is.finite(data), 1, all), ], ncol = 3)
  y <- data[, 1]
  q <- data[, 2]
  p <- data[, 3]

  if (any(p >= 1) || any(p <= 0))
    stop("'p' values have to be in the interval (0,1)!")

  qs.value <- (q-y) * (1*(y <= q)-p)

  if (mean == TRUE) {
    qs.value <- mean(qs.value)
  }
  return(as.numeric(qs.value))

}


