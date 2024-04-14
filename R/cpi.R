#' Central Prediction Interval
#'
#' This function can calculate the central prediction interval score (IS), overprediction, underprediction, width and coverage of a central prediction interval given observations of univariate variable and quantiles of a predictive distribution for a certain nominal coverage.
#'
#' @param y vector of observations
#' @param lower vector with the lower quantiles (depending on \code{nominal.coverage}; see details)
#' @param upper vector with the upper quantiles (depending on \code{nominal.coverage}; see details)
#' @param nominal.coverage numeric; nominal coverage in \% (see details)
#' @param separate character; vector with "\code{is}", "\code{overprediction}", "\code{underprediction}", "\code{width}" or "\code{coverage}"; default: "\code{is}" (see details)
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#' @param aggregate logical or function for aggregating the single scores, e.g. \code{sum}, \code{mean}, \code{weighted.mean}, ....
#' Default: \code{FALSE}, i.e. no aggregation function.
#' @param ... further arguments passed to the \code{aggregate} function.
#'
#' @details
#' For a vector \code{y} of length n, \code{lower}, \code{upper} and \code{nominal.coverage} should be given as vector
#' of length n, where the i-th entry of \code{y} belongs to the i-th entry of the other vectors. For \code{nominal.coverage}
#' only one value is also sufficient.
#'
#' The \code{nominal.coverage} corresponds to \code{alpha := (100 - nominal.coverage)/100}. Consequently \code{lower} must provide the alpha/2-quantiles and \code{upper} the (1-alpha/2)-quantiles of a predictive distribution.
#' Internally, \code{nominal.coverage} will be transformed to \code{alpha}.
#'
#' The parameter \code{separate} allows to choose the output values. "\code{is}" is the central prediction interval score, "\code{overprediction}" is the overprediction,
#' "\code{underprediction}" is the underprediction, "\code{width}" is the central prediction interval width and "\code{coverage}" is the central prediction interval coverage.
#' If an observation lies in a central prediction interval, the output will be 1, otherwise 0 for the parameter "\code{coverage}".
#'
#' A lower IS indicates a better forecast.
#'
#' A lower central prediction interval width indicates a sharper forecast.
#'
#' A score of the central prediction interval coverage close to \code{nominal.coverage} indicates a more calibrated forecast.
#' A central prediction interval coverage score of \code{nominal.coverage} is optimal and indicates a calibrated forecast.
#'
#' @return
#' Vector or list of score value(s) specified in \code{separate}.
#'
#' @examples
#' # simulated data
#' n <- 30
#' y <- rnorm(n, mean = 1:n)
#' nominal.coverage <- 90
#' alpha <- (100-nominal.coverage)/100
#' lower <- qnorm(alpha/2, rnorm(n, mean = 1:n))
#' upper <- qnorm((1-alpha/2), rnorm(n, mean = 1:n))
#'
#' # cpi calculation
#' cpi(y = y, lower = lower, upper = upper, nominal.coverage = nominal.coverage,
#' separate = "is")
#' cpi(y = y, lower = lower, upper = upper, nominal.coverage = nominal.coverage,
#' separate = "is", aggregate = mean)
#'
#' cpi(y = y, lower = lower, upper = upper, nominal.coverage = nominal.coverage,
#' separate = c("is", "overprediction", "underprediction", "width", "coverage"))
#' cpi(y = y, lower = lower, upper = upper, nominal.coverage = nominal.coverage,
#' separate = c("is", "overprediction", "underprediction", "width", "coverage"),
#' aggregate = mean)
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
cpi <- function(y, lower, upper, nominal.coverage, separate = "is", na.action = na.omit, aggregate = FALSE, ...) {
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.vector(lower)) {
    stop("'lower' should be a vector!")
  }
  if (!is.vector(upper)) {
    stop("'upper' should be a vector!")
  }
  if (any(nominal.coverage <= 0) || any(nominal.coverage >= 100)) {
    stop("'nominal.coverage' value(s) should be in the interval (0, 100)!")
  }
  if (!(length(nominal.coverage) %in% c(1, length(y)))) {
    stop("Length of 'nominal.coverage' needs to be 1 or the same as of 'y'!")
  }
  if (length(y) != length(lower) || length(y) != length(upper)) {
    stop("Lengths of 'y', 'lower' and 'upper' are not equal!")
  }

  #prepare data
  data <- cbind(nominal.coverage, y, lower, upper)
  data <- na.action(data)
  nominal.coverage <- data[, 1]
  y <- data[, 2]
  lower <- data[, 3]
  upper <- data[, 4]


  alpha <- (100 - nominal.coverage)/100
  width <- (upper - lower)
  overprediction <- 2/alpha * (lower - y) * (y < lower)
  underprediction <- 2/alpha * (y - upper) * (y > upper)
  is <- width + underprediction + overprediction
  coverage <- 1*(lower <= y & y <= upper)
  cpi.values <- list(is = is, overprediction = overprediction, underprediction = underprediction, width = width, coverage = coverage)

  cpi.values <- cpi.values[separate]

  if (!isFALSE(aggregate)) {
    cpi.values <- lapply(1:length(separate), function(k) {
        do.call(aggregate, list(cpi.values[[k]], ...))
    })
    names(cpi.values) <- separate
  }

  if (length(cpi.values) == 1) {
    cpi.values <- as.numeric(unlist(cpi.values))
  }

  return(cpi.values)

}



