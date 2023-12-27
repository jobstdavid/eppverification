#' Multivariate Entropy
#'
#' This function calculates the Multivariate Entropy given observations of a multivariate variable and samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of samples of a predictive distribution (depending on \code{y}; see details)
#' @param bins numeric; if \code{NULL} the number of bins is equal to \code{nrow(x[, , 1])+1}; otherwise \code{bins} must be chosen so that \code{(nrow(x[, , 1])+1)/bins} is an integer; default: \code{NULL} (see details)
#' @param method character; "\code{mv}", "\code{avg}", "\code{mst}", "\code{bd}"; default: "\code{mv}" (see details)
#' @param na.rm logical; if \code{TRUE} NA are stripped before the rank computation proceeds; if \code{FALSE} NA are used in the rank computation; default: \code{FALSE}
#'
#' @details
#' The observations are given in the matrix \code{y} with n rows, where each column belongs to an univariate observation variable.
#' The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
#' entry must be a matrix with n rows, having the same structure as \code{y}, filled with the samples of a multivariate predictive distribution.
#'
#' The parameter \code{bins} specifies the number of columns for the MVRH. For "large"
#' \code{ncol(x[, , 1])} it is often reasonable to reduce the resolution of the MVRH by
#' using \code{bins} so that \code{(ncol(x[, , 1])+1)/bins} is an integer.
#'
#' For the calculation of the ranks, different methods are available, where "\code{mv}" stands for "multivariate ranks",
#' "\code{avg}" stands for "average ranks", "\code{mst}" stands for "minimum-spanning-tree ranks" and
#' "\code{bd}" stands for "band-depth ranks". These methods are implemented as described in e.g. Thorarinsdottir et al. (2016).
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
#' y <- cbind(rnorm(n), rgamma(n, shape = 1))
#' x <- array(NA, dim = c(m, 2, n))
#' x[, 1, ] <- rnorm(n*m)
#' x[, 2, ] <- rgamma(n*m, shape = 1)
#'
#' # multivariate entropy calculation
#' ment(y = y, x = x, bins = 3, method = "bd")
#'
#' @references
#' Delle Monache, L., Hacker, J., Zhou, Y., Deng, X. and Stull, R., (2006). Probabilistic aspects of meteorological and ozone regional ensemble forecasts. Journal of Geophysical Research: Atmospheres, 111, D24307.
#'
#' Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.
#'
#' Smith, L. and Hansen, J. (2004). Extending the limits of ensemble forecast verification with the minimum spanning tree. Monthly Weather Review, 132, 1522-1528.
#'
#' Taillardat, M., Mestre, O., Zamo, M. and Naveau, P., (2016). Calibrated Ensemble Forecasts Using Quantile Regression Forests and Ensemble Model Output Statistics. American Meteorological Society, 144, 2375-2393.
#'
#' Thorarinsdottir, T., Scheurer, M. and Heinz, C. (2016). Assessing the calibration of high-dimensional ensemble forecasts using rank histograms. Journal of Computational and Graphical Statistics, 25, 105-122.
#'
#' Tribus, M. (1969). Rational Descriptions, Descisions and Designs. Pergamon Press.
#'
#' Wilks, D. (2004). The minimum spanning tree histogram as verification tool for multidimensional ensemble forecasts. Monthly Weather Review, 132, 1329-1340.
#'
#' @author David Jobst
#'
#' @rdname ment
#'
#' @export
ment <- function(y, x, method = "mv", bins = NULL, na.rm = FALSE) {

  ranks <- mrnk(y, x, method, na.rm)
  k <- nrow(x[, , 1])

  if (!is.null(bins)) {
    z <- (k+1)/bins
    if (!(z%%1==0)) {
      stop("'bins' must be an integer, so that (nrow(x[, , 1]) + 1)/bins is an integer, too!")
    }
    else {
      for (i in 1:bins) {
        ranks[ranks %in% (((i-1)*z+1):(i*z))] <- i
      }
    }
  } else {
    bins <- k+1
  }

  #count ranks
  tab <- rbind(1:bins, 0)
  cnt <- table(ranks)
  tab[2, as.numeric(names(cnt))] <- as.numeric(cnt)
  cnt <- tab[2, ]
  f <- cnt/length(ranks)
  ent <- -1/log(bins) * sum(f*log(f))

  return(as.numeric(ent))
}




