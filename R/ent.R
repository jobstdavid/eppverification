#' Entropy
#'
#' This function calculates the Entropy given observations of a univariate variable and samples of a predictive distribution.
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
#' The entropy is a tool to assess the calibration of a forecast. The optimal
#' value of the entropy is 1, representing a calibrated forecast.
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
#' # entropy calculation
#' ent(y = y, x = x, bins = 3)
#'
#' @references
#' Tribus, M. (1969). Rational Descriptions, Decisions and Designs. Pergamon Press.
#'
#' @author David Jobst
#'
#' @rdname ent
#'
#' @importFrom stats na.omit
#'
#' @export
ent <- function(y, x, bins = NULL, na.rm = FALSE) {

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
  f <- cnt/length(ranks)
  ent <- -1/log(bins) * sum(f*log(f))

  return(as.numeric(ent))
}

