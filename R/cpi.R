#' Central Prediction Interval
#'
#' This function can calculate the central prediction interval score (IS), overprediction, underprediction, width and coverage of a central prediction interval given observations of a one-dimensional variable and quantiles of ensemble forecasts/a predictive distribution for a certain interval range.
#'
#' @param y vector of observations
#' @param lower vector with the lower quantiles (depending on \code{interval.range}; see details)
#' @param upper vector with the upper quantiles (depending on \code{interval.range}; see details)
#' @param interval.range numeric; integer(s) between 0 and 100 (see details)
#' @param separate character; vector with "\code{is}", "\code{overprediction}", "\code{underprediction}", "\code{width}" or "\code{coverage}"; default: "\code{is}" (see details)
#' @param mean logical; if \code{TRUE} the mean of the values specified in \code{separate} is calculated for output; if \code{FALSE} the single values specified in \code{separate} are used as output; default: \code{FALSE}
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{lower}, \code{upper} and \code{interval.range} should be given as vector
#' of length n, where the i-th entry of \code{y} belongs to the i-th entry of the other vectors. For \code{interval.range}
#' only one value is also sufficient. This value will be taken for all quantiles as \code{interval.range}.
#'
#' The \code{interval.range} indicates the size of the central prediction interval, i.e. \code{interval.range} corresponds to
#' \code{alpha := (100 - interval.range)/100}. Consequently \code{lower} must provide the alpha/2-quantiles and \code{upper} the (1-alpha/2)-quantiles of ensemble forecasts/a predictive distribution.
#' Internally, \code{interval.range} will be transformed to \code{alpha}.
#'
#' For ensemble forecasts of size m, it is common to choose \code{interval.range := (m-1)/(m+1)*100}. Consequently \code{lower} must contain
#' in each entry the minimum value of the m forecasts and \code{upper} must contain in each entry the maximum value of the m forecasts.
#'
#' The parameter \code{separate} allows to choose the output values. "\code{is}" is the central prediction interval score, "\code{overprediction}" is the overprediction,
#' "\code{underprediction}" is the underprediction, "\code{width}" is the central prediction interval width and "\code{coverage}" is the central prediction interval coverage.
#' If an observation lies in a central prediction interval, the output will be 1, otherwise 0 for the parameter "\code{coverage}".
#'
#' A lower IS indicates a better forecast.
#'
#' A lower central prediction interval width score indicates a sharper forecast. The optimal value is 0.
#'
#' A score of the central prediction interval coverage close to \code{interval.range} indicates a more calibrated forecast.
#' A central prediction interval coverage score of \code{interval.range} is optimal and indicates a calibrated forecast.
#'
#' @return
#' Vector or list of score value(s) specified in \code{separate}.
#'
#' @examples
#' #simulated data
#' n <- 30
#' y <- rnorm(n, mean = 1:n)
#' interval.range <- 90
#' alpha <- (100-interval.range)/100
#' lower <- qnorm(alpha/2, rnorm(n, mean = 1:n))
#' upper <- qnorm((1-alpha/2), rnorm(n, mean = 1:n))
#'
#' #cpi calculation
#' cpi(y = y, lower = lower, upper = upper, interval.range = interval.range,
#' separate = "is", mean = FALSE)
#' cpi(y = y, lower = lower, upper = upper, interval.range = interval.range,
#' separate = "is", mean = TRUE)
#'
#' cpi(y = y, lower = lower, upper = upper, interval.range = interval.range,
#' separate = c("is", "overprediction", "underprediction", "width", "coverage"),
#' mean = FALSE)
#' cpi(y = y, lower = lower, upper = upper, interval.range = interval.range,
#' separate = c("is", "overprediction", "underprediction", "width", "coverage"),
#' mean = TRUE)
#'
#' @references
#' Gneiting, T. and Raftery, A. (2007). Strictly Proper Scoring Rules, Prediction, and Estimation. Journal of the American Statistical Association, 102, 359-378.
#'
#' @author David Jobst
#'
#' @rdname cpi
#'
#' @importFrom stats na.omit
#'
#' @export
cpi <- function(y, lower, upper, interval.range, separate = "is", mean = FALSE, na.rm = FALSE) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.vector(lower)) {
    stop("'lower' should be a vector!")
  }
  if (!is.vector(upper)) {
    stop("'upper' should be a vector!")
  }
  if (any(interval.range <= 0) || any(interval.range >= 100)) {
    stop("'interval.range' value(s) should be in the interval (0, 100)!")
  }
  if (!(length(interval.range) %in% c(1, length(y)))) {
    stop("Length of 'interval.range' needs to be 1 or the same as of 'y'!")
  }
  if (length(y) != length(lower) || length(y) != length(upper)) {
    stop("Lengths of 'y', 'lower' and 'upper' are not equal!")
  }

  #prepare data
  if (na.rm) {
    data <- cbind(interval.range, y, lower, upper)
    data <- na.omit(data)
    interval.range <- data[, 1]
    y <- data[, 2]
    lower <- data[, 3]
    upper <- data[, 4]
  }

  alpha <- (100 - interval.range)/100
  width <- (upper - lower)
  overprediction <- 2/alpha * (lower - y) * (y < lower)
  underprediction <- 2/alpha * (y - upper) * (y > upper)
  is <- width + underprediction + overprediction
  coverage <- 1*(lower < y & y < upper)
  cpi.values <- list(is = is, overprediction = overprediction, underprediction = underprediction, width = width, coverage = coverage)

  cpi.values <- cpi.values[separate]

  if (mean == TRUE) {
    cpi.values <- lapply(cpi.values, mean)
  }
  if (length(cpi.values) == 1) {
    cpi.values <- as.numeric(unlist(cpi.values))
  }
  return(cpi.values)

}

