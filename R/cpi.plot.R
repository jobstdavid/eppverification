#' Central Prediction Intervals Plot
#'
#' This function plots the central prediction intervals given evaluation points, observations of a one-dimensional variable and quantiles of ensemble forecasts/a predictive distribution for a certain interval range.
#'
#' @param x evaluation points of class "\code{Date}", "\code{numeric}" or "\code{integer}"
#' @param y vector of observations
#' @param lower vector with the lower quantiles (depending on \code{interval.range}; see details)
#' @param upper vector with the upper quantiles (depending on \code{interval.range}; see details)
#' @param interval.range numeric; an integer between 0 and 100 (see details)
#' @param x.lab character; label of x-axis; default: ""
#' @param y.lab character; label of y-axis; default: ""
#' @param title character; title of the plot; default: "\code{interval.range\% Central Prediction Interval}"
#' @param info logical; if TRUE the central prediction interval width and coverage are calculated for the plot (see details); if FALSE the central prediction interval width and coverage are not calculated; default: \code{FALSE}
#'
#' @details
#' For \code{x} of length n, \code{y}, \code{lower} and \code{upper} should be given as vector
#' of length n, where the i-th entry of \code{x} belongs to the i-th entry of the other vectors.
#' Only finite values of \code{x}, \code{y}, \code{lower}, \code{upper} and \code{interval.range} are used.
#'
#' For \code{interval.range} only one value is needed. This value will be taken for all quantiles as \code{interval.range}.
#' The \code{interval.range} indicates the size of the central prediction interval, i.e. \code{interval.range} corresponds to
#' \code{alpha := (100 - interval.range)/100}. Consequently \code{lower} must provide the alpha/2-quantiles and \code{upper} the (1-alpha/2)-quantiles of ensemble forecasts/a predictive distribution.
#' Internally, \code{interval.range} will be transformed to \code{alpha}.
#'
#' For ensemble forecasts of size m, it is common to choose \code{interval.range := (m-1)/(m+1)*100}. Consequently \code{lower} must contain
#' in each entry the minimum value of the m forecasts and \code{upper} must contain in each entry the maximum value of the m forecasts.
#'
#' A lower central prediction interval width score indicates a sharper forecast. The optimal value is 0.
#'
#' A score of the central prediction interval coverage close to \code{interval.range} indicates a more calibrated forecast.
#' A central prediction interval coverage score of \code{interval.range} is optimal and indicates a calibrated forecast.
#'
#' @return
#' ggplot object with a plot of the central prediction intervals.
#'
#' @examples
#' #simulated data
#' n <- 30
#' x1 <- 1:n
#' x2 <- seq(Sys.Date(), by = "day", length.out = n)
#' y <- rnorm(n, mean = 1:n)
#' interval.range <- 90
#' alpha <- (100-interval.range)/100
#' lower <- qnorm(alpha/2, rnorm(n, mean = 1:n))
#' upper <- qnorm((1-alpha/2), rnorm(n, mean = 1:n))
#' x.lab <- "Date"
#' y.lab <- "Value"
#' title <- "Central Prediction Interval"
#'
#'
#' #cpi.plot plot
#' cpi.plot(x = x1, y = y, lower = lower, upper = upper,
#' interval.range = interval.range)
#' cpi.plot(x = x2, y = y, lower = lower, upper = upper,
#' interval.range = interval.range, x.lab = x.lab, y.lab = y.lab,
#' title = title, info = TRUE)
#'
#' @references
#' Gneiting, T. and Raftery, A. (2007). Strictly Proper Scoring Rules, Prediction,and Estimation. Journal of the American Statistical Association, 102, 359-378.
#'
#' @author David Jobst
#'
#' @rdname cpi.plot
#'
#' @importFrom ggplot2 ggplot geom_pointrange ggtitle aes labs xlab ylab theme element_text theme_bw
#' @export
cpi.plot <- function(x, y, lower, upper, interval.range, x.lab = "", y.lab = "", title = paste0(interval.range, "% Central Prediction Interval"), info = FALSE) {
  if (!(class(x) %in% c("Date", "numeric", "integer"))) {
    stop("'x' should have class 'Date', 'numeric' or 'integer'!")
  }
  if (!is.vector(y)) {
    stop("'y' should be a vector!")
  }
  if (!is.vector(lower)) {
    stop("'lower' should be a vector!")
  }
  if (!is.vector(upper)) {
    stop("'upper' should be a vector!")
  }
  if (length(interval.range) != 1) {
    stop("'interval.range' should be only one value!")
  }
  if (interval.range <= 0 || interval.range >= 100) {
    stop("'interval.range' should be a value in the interval (0, 100)!")
  }
  l <- list(x, y, lower, upper)
  l <- unlist(lapply(l, length))
  if (length(unique(l)) != 1) {
    stop("Lengths of 'x', 'y', 'lower' and 'upper' are not equal!")
  }

  #prepare data
  data <- cbind(x, y, lower, upper)
  data <- na.omit(data)
  c.x <- class(x)
  x <- data[, 1]
  class(x) <- c.x
  y <- data[, 2]
  ymin <- data[, 3]
  ymax <- data[, 4]

  data <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)
  p <- ggplot(data, aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
    geom_pointrange(color = "darkgrey", fill = "black", shape = 21, size = 1, fatten = 2, stroke = 0) +
    theme_bw() +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  if (info == TRUE) {
    out <- cpi(y, lower, upper, interval.range, separate = c("width", "coverage"), mean = TRUE)
    p <- p + labs(subtitle = paste0("Coverage = ", sep = "", round(out$coverage, 4)*100, "%, Width = ", round(out$width, 4)))
  }
  return(p)
}

