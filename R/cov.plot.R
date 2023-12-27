#' Central Prediction Interval Coverage Model Comparison
#'
#' This function plots the central prediction interval coverage for different interval ranges of selected models as line plot.
#'
#' @param x matrix of central prediction interval coverages of different models in \%
#' @param models character; column names of \code{x} (depending on \code{x}; see details)
#' @param nominal.coverage numeric; nominal coverages in \% (see details)
#' @param colors character; color for each object in \code{models}; default: ggplot2 setting (depending on \code{x}; see details)
#' @param title character; title of the plot; default: "\code{Interval Coverage}"
#' @param legend character; position of the legend; default: "\code{right}" (see details)
#'
#' @details
#' For matrix \code{x}, each column contains the central prediction interval coverages of a different model in \%. Consequently
#' the i-th row entries of \code{x} correspond to the i-th entry in \code{nominal.coverage}. In addition each entry in \code{models} and \code{colors} refers to
#' the respective column of \code{x}. NA's are omitted.
#'
#' For the parameter \code{legend} the following options are available: "\code{right}", "\code{left}", "\code{bottom}", "\code{top}", "\code{hide}".
#'
#' The closer the points/a line plot to the dashed line is, the more calibrated the model is.
#'
#' @return
#' ggplot object with line plots of the central prediction interval coverages of different models.
#'
#' @examples
#' # simulated data
#' n <- 30
#' x <- matrix(runif(n)*100, ncol = 3)
#' x <- apply(x, 2, sort)
#' models <- c("A", "B", "C")
#' nominal.coverage <- seq(5, 95, length.out = 10)
#' colors <- c("darkred", "steelblue", "orange")
#' title <- "Interval Coverage Comparison"
#' legend <- "bottom"
#'
#' # cov.plot plot
#' cov.plot(x = x, models = models, nominal.coverage = nominal.coverage)
#' cov.plot(x = x, models = models, nominal.coverage = nominal.coverage,
#' colors = colors, title = title, legend = legend)
#'
#' @author David Jobst
#'
#' @rdname cov.plot
#'
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot geom_line geom_point geom_abline scale_x_continuous scale_y_continuous scale_color_manual ggtitle aes labs xlab ylab theme element_text theme_bw
#' @export
cov.plot <- function(x, models, nominal.coverage, colors = NULL, title = "Interval Coverage", legend = "right") {
  if (!is.matrix(x)) {
    stop("'x' should be a matrix!")
  }
  if (!is.vector(models)) {
    stop("'models' should be a vector!")
  }
  if (!is.vector(nominal.coverage)) {
    stop("'nominal.coverage' should be a vector!")
  }
  if (ncol(x) != length(models)) {
    stop("Length of 'models' is not appropriate!")
  }
  if (nrow(x) != length(nominal.coverage)) {
    stop("Length of 'nominal.coverage' is not appropriate!")
  }
  if (any(nominal.coverage <= 0) || any(nominal.coverage >= 100))
    stop("'nominal.coverage' values should be in the interval (0, 100)!")

  data <- cbind(x, nominal.coverage)
  data <- na.omit(data)
  x <- data[, 1:(ncol(data)-1)]
  nominal.coverage <- data[, ncol(data)]

  Legend <- rep(models, each = length(nominal.coverage))
  range <- rep(nominal.coverage, times = length(models))
  value <- as.vector(x)
  data <- data.frame(Legend, range, value)

  p <- ggplot(data) +
    geom_line(aes(x = range, y = value, group = Legend, color = Legend)) +
    geom_point(aes(x = range, y = value, group = Legend, color = Legend)) +
    geom_abline(intercept = 0, linetype = "dashed") +
    theme_bw() +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    xlab("Nominal Interval Coverage (%)") +
    ylab("Observed Interval Coverage (%)") +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = legend)

  if (!is.null(colors)) {
    if (!(length(colors) %in% c(1, length(models)))) {
      stop("Length of 'colors' should be 1 or the length of 'models'!")
    } else if (length(colors) == 1) {
      colors <- rep(colors, length(models))
    }
    col <- setNames(colors, models)
    suppressMessages(p <- p + scale_color_manual(values = col))
  }

  return(p)

}

