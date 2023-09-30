#' Continuous Ranked Probability Score
#'
#' This function calculates the Continuous Ranked Probability Score (CRPS) given observations of a one-dimensional variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of ensemble forecasts/samples of a predictive distribution (depending on \code{y}; see details)
#' @param method character; "\code{ens}", "\code{sml}" and "\code{mc}"; default: "\code{ens}" (see details)
#' @param mean logical; if \code{TRUE} the mean of the CRPS values is calculated for output; if \code{FALSE} the single CRPS values are used as output; default: \code{FALSE}
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{x} should be given as matrix
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution or ensemble forecasts.
#'
#' If method "\code{ens}" is specified, the CRPS values are calculated for
#' given ensemble forecasts in \code{x} (Grimit et al., 2006).
#' If method "\code{sml}" is specified, the CRPS values are calculated for a "small" number of
#' given ensemble forecasts in \code{x} (Ferro et al., 2008). If method "\code{mc}"
#' is specified, the CRPS values are calculated by a Monte-Carlo approximation using samples
#' of a predictive distribution in \code{x} (Gneiting et al., 2008).
#'
#' A lower CRPS indicates a better forecast.
#'
#' @return
#' Vector of score value(s).
#'
#' @examples
#' #simulated data
#' n <- 30
#' m1 <- 50
#' m2 <- 3
#' m3 <- 10000
#' y <- rnorm(n)
#' x1 <- matrix(rnorm(n*m1), ncol = m1)
#' x2 <- matrix(rnorm(n*m2), ncol = m2)
#' x3 <- matrix(rnorm(n*m3), ncol = m3)
#'
#' #crps calculation
#' crps(y = y, x = x1, method = "ens", mean = FALSE)
#' crps(y = y, x = x1, method = "ens", mean = TRUE)
#'
#' crps(y = y, x = x2, method = "sml", mean = FALSE)
#' crps(y = y, x = x2, method = "sml", mean = TRUE)
#'
#' crps(y = y, x = x3, method = "mc", mean = FALSE)
#' crps(y = y, x = x3, method = "mc", mean = TRUE)
#'
#' @references
#' Ferro, C., Richardson, D. and Weigel, A. (2008). On the effect of ensemble size on the discrete and continuous ranked probability scores. Meteorological Applications, 15, 19-24.
#'
#' Grimit, E., Gneiting, T., Berrocal, V. and Johnson, N. (2006). The continuous ranked probability score for circular variables and its applications to mesoscale forecast ensemble verification. Quarterly Journal of the Royal Meteorological Society, 132, 2925-2942.
#'
#' Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.
#'
#' @author David Jobst
#'
#' @rdname crps
#'
#' @importFrom stats na.omit
#'
#' @export
crps <- function(y, x, method = "ens", mean = FALSE, na.rm = FALSE) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.matrix(x)) {
    stop("'x' should be a matrix!")
  }
  if (length(y) != nrow(x)) {
    stop("Length of 'y' is not equal to the number of rows of 'x'!")
  }

  if (method == "ens") {
      crps.value <- crps_cpp(y = y, x = x)
  } else if (method == "sml") {
    crps.value <- crps_sml_cpp(y = y, x = x)
  } else if (method == "mc") {
    crps.value <- crps_mc_cpp(y = y, x = x)
  } else {
      stop("This method is not available!")
  }

  if (na.rm == TRUE) {
    crps.value <- as.vector(na.omit(crps.value))
  }

  if (mean == TRUE) {
    crps.value <- mean(crps.value)
  }

  return(as.numeric(crps.value))

}

