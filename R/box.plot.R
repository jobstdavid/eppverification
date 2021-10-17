#' Box Plot
#'
#' This function creates box plots for a given dataset.
#'
#' @param x matrix of given dataset
#' @param names character; column names of \code{x} (depending on \code{x}; see details)
#' @param colors character; color for each object in \code{names}; default: ggplot2 setting (depending on \code{x}; see details)
#' @param x.lab character; label of x-axis; default: ""
#' @param y.lab character; label of y-axis; default: ""
#' @param title character; title of the plot; default: "\code{Box Plot}"
#' @param legend character; position of the legend; default: "\code{right}" (see details)
#'
#' @details
#' For matrix \code{x}, each column belongs to one object in \code{names} and \code{colors}.
#' If only one value is provided for \code{names} and \code{colors} this value is
#' used for all column entries in \code{x}.
#' Only finite values of \code{x} are used.
#'
#' For \code{colors} the specifications of \code{color} in ggplot2 are used.
#'
#' For the parameter \code{legend} the following options are available: "\code{right}", "\code{left}", "\code{bottom}", "\code{top}", "\code{hide}".
#'
#' @return
#' ggplot object with box plots for a given dataset.
#'
#' @examples
#' #simulated data
#' n <- 30
#' x <- cbind(rnorm(n), rgamma(n, shape = 1))
#' names <- c("A", "B")
#' colors <- c("darkred", "steelblue")
#' x.lab <- "Models"
#' y.lab <- "Values"
#' title <- "Box Plots"
#' legend <- "top"
#'
#' #box.plot plot
#' box.plot(x = x, names = names)
#' box.plot(x = x, names = names, colors = colors,
#' x.lab = x.lab, y.lab = y.lab, title = title, legend = legend)
#' box.plot(x = x, names = names, colors = colors[1])
#'
#' @author David Jobst
#'
#' @rdname box.plot
#'
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot geom_boxplot ggtitle aes labs xlab ylab theme element_text scale_fill_manual
#' @export
box.plot <- function(x, names, colors = NULL, x.lab = "", y.lab = "", title = "Box Plot", legend = "right") {

  if (!is.matrix(x)) {
    stop("'x' should be a matrix!")
  }
  if (!is.vector(names)) {
    stop("'names' should be a vector!")
  }
  if (length(names) != ncol(x)) {
    stop("Length of 'names' and number of columns of 'x' are not equal!")
  }

  #prepare data
  x <- matrix(x[apply(is.finite(x), 1, all), ], ncol = ncol(x))
  Legend <- rep(names, each = nrow(x))
  value <- as.vector(x)
  data <- data.frame(Legend, value)

  p <- ggplot(data) +
    geom_boxplot(aes(x = Legend, y = value, fill = Legend)) +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = legend)

  if (!is.null(colors)) {
    if (!(length(colors) %in% c(1, length(names)))) {
      stop("Length of 'colors' should be 1 or the length of 'names'!")
    } else if (length(colors) == 1) {
      colors <- rep(colors, length(names))
    }
    col <- setNames(colors, names)
    suppressMessages(p <- p + scale_fill_manual(values = col))
  }

  return(p)

}

