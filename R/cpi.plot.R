#' Central Prediction Intervals Plot
#'
#' This function plots the central prediction intervals given evaluation points, observations of an univariate variable and quantiles of a predictive distribution for a certain interval range.
#'
#' @param x evaluation points of class "\code{Date}", "\code{numeric}" or "\code{integer}"
#' @param y vector of observations
#' @param lower vector with the lower quantiles (depending on \code{nominal.coverage}; see details)
#' @param upper vector with the upper quantiles (depending on \code{nominal.coverage}; see details)
#' @param nominal.coverage numeric; nominal coverage in \% (see details)
#' @param x.lab character; label of x-axis; default: ""
#' @param y.lab character; label of y-axis; default: ""
#' @param title character; title of the plot; default: "\code{nominal.coverage\% Central Prediction Interval}"
#' @param info logical; if \code{TRUE} the mean central prediction interval width and coverage are calculated for the plot (see details); if \code{FALSE} the mean central prediction interval width and coverage are not calculated; default: \code{FALSE}
#'
#' @details
#' For \code{x} of length n, \code{y}, \code{lower} and \code{upper} should be given as vector
#' of length n, where the i-th entry of \code{x} belongs to the i-th entry of the other vectors.
#' NA's are omitted.
#'
#' A lower central prediction interval width indicates a sharper forecast.
#'
#' A score of the central prediction interval coverage close to \code{nominal.coverage} indicates a more calibrated forecast.
#' A central prediction interval coverage score of \code{nominal.coverage} is optimal and indicates a calibrated forecast.
#'
#' @return
#' ggplot object with a plot of the central prediction intervals.
#'
#' @examples
#' # simulated data
#' n <- 30
#' x1 <- 1:n
#' x2 <- seq(Sys.Date(), by = "day", length.out = n)
#' y <- rnorm(n, mean = 1:n)
#' nominal.coverage <- 90
#' alpha <- (100-nominal.coverage)/100
#' lower <- qnorm(alpha/2, rnorm(n, mean = 1:n))
#' upper <- qnorm((1-alpha/2), rnorm(n, mean = 1:n))
#' x.lab <- "Date"
#' y.lab <- "Value"
#' title <- "Central Prediction Interval"
#'
#'
#' # cpi.plot plot
#' cpi.plot(x = x1, y = y, lower = lower, upper = upper,
#' nominal.coverage = nominal.coverage)
#' cpi.plot(x = x2, y = y, lower = lower, upper = upper,
#' nominal.coverage = nominal.coverage, x.lab = x.lab, y.lab = y.lab,
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
cpi.plot <- function(x, y, lower, upper, nominal.coverage, x.lab = "", y.lab = "", title = paste0(nominal.coverage, "% Central Prediction Interval"), info = FALSE) {
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
  if (length(nominal.coverage) != 1) {
    stop("'nominal.coverage' should be only one value!")
  }
  if (nominal.coverage <= 0 || nominal.coverage >= 100) {
    stop("'nominal.coverage' should be a value in the interval (0, 100)!")
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
    out <- cpi(y, lower, upper, nominal.coverage, separate = c("width", "coverage"), aggregate = mean)
    p <- p + labs(subtitle = paste0("Coverage = ", sep = "", round(out$coverage, 4)*100, "%, Width = ", round(out$width, 4)))
  }
  return(p)
}

