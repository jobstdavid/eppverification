#' Line Plot
#'
#' This function plots forecast data as line plots.
#'
#' @param x evaluation points of class "\code{Date}", "\code{numeric}" or "\code{integer}"
#' @param y matrix of forecast data (see details)
#' @param names character; column names of \code{y} (depending on \code{y}; see details)
#' @param point logical; if \code{TRUE} points are additionally plotted for each object in \code{names}; if \code{FALSE} no points are plotted for each object in \code{names}; default: \code{FALSE}
#' @param linetypes character; line type for each object in \code{names}; default: "\code{solid}" (depending on \code{y}; see details)
#' @param colors character; color for each object in \code{names}; default: "\code{black}" (depending on \code{y}; see details)
#' @param x.lab character; label of x-axis; default: ""
#' @param y.lab character; label of y-axis; default: ""
#' @param title character; title of the plot; default: ""
#' @param legend character; position of the legend; default: "\code{right}" (see details)
#'
#' @details
#' For \code{x} of length n, \code{y} should be given as matrix with n rows, where each column consists of one forecast.
#' The the i-th entry of \code{x} belongs to the i-th row of \code{y}. In addition each entry in \code{names}, \code{linetypes} and \code{colors} refers to
#' the respective column of \code{y}. If only one value is provided for \code{names}, \code{linetypes} and \code{colors} this value is
#' used for all column entries in \code{y}.
#' Only finite values of \code{x} and \code{y} are used.
#'
#' For \code{linetypes} the specifications of \code{linetype} in ggplot2 are used.
#'
#' For \code{colors} the specifications of \code{color} in ggplot2 are used.
#'
#' For the parameter \code{legend} the following options are available: "\code{right}", "\code{left}", "\code{bottom}", "\code{top}", "\code{hide}".
#'
#' @return
#' ggplot object with line plots of the forecast data.
#'
#' @examples
#' #simulated data
#' n <- 30
#' k <- 10
#' x1 <- 1:n
#' x2 <- seq(Sys.Date(), by = "day", length.out = n)
#' obs <- rnorm(n, mean = 1:n)
#' y <- sapply(1:k, function(i) obs-(i-1)*runif(n))
#' names1 <- c("observation", paste("forecast", sep = " ", 1:(k-1)))
#' names2 <- c("observation", rep("forecasts", 7), rep("modified forecasts", 2))
#' linetypes1 <- c("solid", rep("dashed", k-1))
#' linetypes2 <- c("solid", rep("dashed", 7), rep("dotted", 2))
#' colors1 <- c("red", rep("black", k-1))
#' colors2 <- c("red", rep("black", 7), rep("blue", 2))
#' x.lab <- "Date"
#' y.lab <- "Value"
#' title <- "Observation vs. Forecast"
#' legend1 <- "hide"
#' legend2 <- "bottom"
#'
#'
#' #line.plot plot
#' line.plot(x = x1, y = y, names = names1)
#' line.plot(x = x1, y = y, names = names1, point = TRUE,
#' linetypes = linetypes1, legend = legend1)
#' line.plot(x = x2, y = y, names = names1, colors = colors1,
#' x.lab = x.lab, y.lab = y.lab, title = title, legend = legend2)
#' line.plot(x = x2, y = y, names = names1, colors = colors1[1],
#' linetypes = linetypes1[2])
#' line.plot(x = x2, y = y, names = names2, colors = colors2,
#' linetypes = linetypes2)
#'
#' @author David Jobst
#'
#' @rdname line.plot
#'
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot geom_line geom_point ggtitle aes labs xlab ylab theme element_text scale_linetype_manual scale_color_manual
#' @export
line.plot <- function(x, y, names, point = FALSE, linetypes = "solid", colors = "black", x.lab = "", y.lab = "", title = "", legend = "right") {
  if (!(class(x) %in% c("Date", "numeric", "integer"))) {
    stop("'x' should have class 'Date', 'numeric' or 'integer'!")
  }
  if (!is.matrix(y)) {
    stop("'y' should be a matrix!")
  }
  if (length(x) != nrow(y)) {
    stop("Length of 'x' and number of rows of 'y' are not equal!")
  }
  if (length(names) != ncol(y)) {
    stop("Length of 'names' and number of columns of 'y' are not equal!")
  }
  if (length(unique(names)) < ncol(y)) {
    names.save <- names
    for (k in unique(names)) {
      names[which(names == k)] <- paste(k, sep = "", 1:length(which(names == k)))
    }
  } else {
    names.save <- names
  }
  #prepare data
  data <- cbind(x, y)
  data <- matrix(data[apply(is.finite(data), 1, all), ], ncol = ncol(data))
  c.x <- class(x)
  x <- data[, 1]
  class(x) <- c.x
  y <- data[, -1]

  z <- rep(x, times = length(names))
  Legend <- rep(names.save, each = length(x))
  Legend2 <- rep(names, each = length(x))
  value <- as.vector(y)
  data <- data.frame(z, Legend, value, Legend)

  if (!(length(linetypes) %in% c(1, length(names)))) {
    stop("Length of 'linetypes' should be 1 or the length of 'names'!")
  } else if (length(linetypes) == 1) {
    linetypes <- rep(linetypes, length(names))
  }
  if (!(length(colors) %in% c(1, length(names)))) {
    stop("Length of 'colors' should be 1 or the length of 'names'!")
  } else if (length(colors) == 1) {
    colors <- rep(colors, length(names))
  }
  linetypes <- setNames(linetypes, names.save)
  colors <- setNames(colors, names.save)


  p <- ggplot(data, aes(x = z, y = value, by = Legend2)) +
    geom_line(aes(color = Legend,  linetype = Legend)) +
    scale_linetype_manual(values = linetypes) +
    scale_color_manual(values = colors) +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = legend)

  if (point == TRUE) {
    p <- p + geom_point(aes(x = z, y = value, color = Legend))
  }

  return(p)
}


