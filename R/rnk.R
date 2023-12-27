#' Ranks
#'
#' This function calculates the ranks given observations of an univariate variable and samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of samples of a predictive distribution (depending on \code{y}; see details)
#' @param na.rm logical; if \code{TRUE} NA are stripped before the rank computation proceeds; if \code{FALSE} NA are used in the rank computation; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{x} should be given as matrix
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution.
#'
#' @return
#' Vector of ranks.
#'
#' @examples
#' # simulated data
#' n <- 30
#' m <- 50
#' y <- rnorm(n)
#' x <- matrix(rnorm(n*m), ncol = m)
#'
#' # rank calculation
#' rnk(y = y, x = x)
#'
#' @references
#' Anderson, J. (1996). A method for producing and evaluating probabilistic forecasts from ensemble model integrations. Journal of Climate, 9, 1518-1530.
#'
#' Candille, G. and Talagrand, O. (2005). Evaluation of probabilistic prediction systems for a scalar variable. Quarterly Journal of the Royal Meteorological Society, 131(609), 2131-2150.
#'
#' Hamill, T. and Colucci, S. (1997). Verification of Eta-RSM short-range ensemble forecasts. Monthly Weather Review, 125, 1312-1327.
#'
#' Hamill, T. (2001). Interpretation of rank histograms for verifying ensemble forecasts. Monthly Weather Review, 129, 550-560.
#'
#' Talagrand, O., Vautard, R. and Strauss, B. (1997). Evaluation of probabilistic prediction systems. Workshop on Predictability (ECMWF), 1-25.
#'
#' @author David Jobst
#'
#' @rdname rnk
#'
#' @export
rnk <- function(y, x, na.rm = FALSE) {
  # y entries correspond to each time point
  # x column entries correspond to the samples and the rows correspond to each time point
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.matrix(x)) {
    stop("'x' should be a matrix!")
  }
  if (length(y) != nrow(x)) {
    stop("Length of 'y' is not equal to the number of rows of 'x'!")
  }

  # prepare data
  data <- cbind(y, x)
  if (na.rm) {
    rank <- apply(data, 1, function(z) rank(z, na.last = NA, ties = "random")[1])
  } else {
    rank <- apply(data, 1, function(z) rank(z, na.last = "keep", ties = "random")[1])
  }

  return(rank)
}

