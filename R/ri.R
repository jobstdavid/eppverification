#' Reliability Index
#'
#' This function calculates the Reliability Index (RI) given observations of an univariate variable and samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of samples of a predictive distribution (depending on \code{y}; see details)
#' @param bins numeric; if \code{NULL} the number of bins is equal to \code{ncol(x)+1}; otherwise \code{bins} must be chosen so that \code{(ncol(x)+1)/bins} is an integer; default: \code{NULL} (see details)
#' @param na.rm logical; if \code{TRUE} NA are stripped before the rank computation proceeds; if \code{FALSE} NA are used in the rank computation; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{x} should be given as matrix
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution.
#'
#' The parameter \code{bins} specifies the number of columns for the VRH. For "large"
#' \code{ncol(x)} it is often reasonable to reduce the resolution of the VRH by
#' using \code{bins} so that \code{(ncol(x)+1)/bins} is an integer.
#'
#' The deviation from uniformity of the VRH can be quantified by the reliability index (RI).
#' The smaller the RI, the better the calibration of the forecast. The
#' optimal value of the RI is 0.
#'
#' @return
#' Vector of the score value.
#'
#' @examples
#' # simulated data
#' n <- 30
#' m <- 50
#' y <- rnorm(n)
#' x <- matrix(rnorm(n*m), ncol = m)
#'
#' # ri calculation
#' ri(y = y, x = x)
#' ri(y = y, x = x, bins = 17)
#'
#' @references
#' Delle Monache, L., Hacker, J., Zhou, Y., Deng, X. and Stull, R., (2006). Probabilistic aspects of meteorological and ozone regional ensemble forecasts. Journal of Geophysical Research: Atmospheres, 111, D24307.
#'
#' @author David Jobst
#'
#' @rdname ri
#'
#' @importFrom stats na.omit
#'
#' @export
ri <- function(y, x, bins = NULL, na.rm = FALSE) {

  ranks <- rnk(y, x, na.rm = na.rm)
  k <- ncol(x)

  if (!is.null(bins)) {
    z <- (k+1)/bins
    if (!(z%%1==0)) {
      stop("'bins' must be an integer, so that (ncol(x)+1)/bins is an integer, too!")
    }
    else {
      for (i in 1:bins) {
        ranks[ranks %in% (((i-1)*z+1):(i*z))] <- i
      }
    }
  } else {
    bins <- k+1
  }

  # count ranks
  tab <- rbind(1:bins, 0)
  cnt <- table(ranks)
  tab[2, as.numeric(names(cnt))] <- as.numeric(cnt)
  cnt <- tab[2, ]

  ri <- sum(abs(cnt/length(ranks) - 1/bins))
  return(as.numeric(ri))
}

