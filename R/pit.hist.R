#' Probability Integral Transform Histogram
#'
#' This function plots the Probability Integral Transform (PIT) Histogram for a predictive distribution.
#'
#' @param u vector of PIT values in [0,1] (see details)
#' @param bins numeric; number of bins; default: \code{bins = round(sqrt(length(u)))} (see details)
#' @param type character; "\code{relative}", "\code{absolute}" and "\code{density}"; default: "\code{density}" (see details)
#' @param title character; title of the plot; default: "\code{PIT Histogram}"
#' @param var logical; if \code{TRUE} the variance of the PIT values is calculated for the plot (see details); if \code{FALSE} the variance of the PIT values is not calculated; default: \code{FALSE}
#' @param m logical; if \code{TRUE} the expectation of the PIT values is calculated for the plot (see details); if \code{FALSE} the expectation of the PIT values is not calculated; default: \code{FALSE}
#'
#' @details
#' The vector \code{u} contains the PIT values \code{u}=F(\code{x}) for a predictive
#' distribution F and argument \code{x}.
#' Only finite values of \code{u} are used.
#'
#' The parameter \code{bins} specifies the number of columns for the PIT histogram.
#'
#' If \code{type} is "\code{relative}" the relative frequencies of the bins are plotted.
#' If \code{type} is "\code{absolute}" the absolute frequencies of the bins are plotted.
#' If \code{type} is "\code{density}" the relative densities of the bins are plotted.
#'
#' An uniform PIT histogram indicates a calibrated predictive distribution. A ∩-shape in the
#' PIT histogram indicates overdispersion and a ∪-shape indicates underdispersion
#' of the predictive distribution. A systematic bias of the predictive distribution
#' results in a triangular shaped PIT histogram.
#'
#' The variance of the PIT values (Var(PIT)) provides information on the dispersion of a predictive distribution.
#' A variance of the PIT values equal to 1/12 ≈ 0.0833 corresponds to the variance of the uniform distribution
#' on [0,1], which is desirable. A variance greater than 1/12 indicates underdispersion and a
#' variance smaller than 1/12 indicates overdispersion of the predictive distribution.
#'
#' The expectation value of the PIT values (E(PIT)) provides information on the bias of a predictive distribution.
#' An expectation of the PIT values equal to 1/2 corresponds to the expectation of the uniform distribution
#' on [0,1], which is desirable. Any deviation from 1/2 indicates that the predictive distribution is biased.
#'
#' @return
#' ggplot object with a plot of the PIT histogram.
#'
#' @examples
#' #simulated data
#' n <- 10000
#' u <- runif(n)
#'
#' #pit plot
#' pit.hist(u = u)
#' pit.hist(u = u, bins = 5, title = "PITH", var = TRUE, m = FALSE)
#' pit.hist(u = u, bins = 5, type = "relative", var = FALSE, m = TRUE)
#' pit.hist(u = u, bins = 5, type = "absolute", var = TRUE, m = TRUE)
#'
#'
#' @references
#' Czado, C., Gneiting, T. and Held, L. (2009). Predictive Model Assessment for Count Data. Biometrics, 65(4), 1254–1261.
#'
#' Gneiting, T. and Ranjan, R. (2013). Combining predictive distributions. Electronic Journal of Statistics, 7, 1747-1782.
#'
#' @author David Jobst
#'
#' @rdname pit.hist
#'
#' @importFrom ggplot2 ggplot geom_histogram geom_hline ggtitle aes labs xlab ylab theme element_text stat
#' @export
pit.hist <- function(u, bins = NULL, type = "density", title = "PIT Histogram", var = FALSE, m = FALSE) {
  if (!is.vector(u)) {
    stop("'u' should be a vector!")
  }
  #allow only finite values for u
  u <- u[is.finite(u)]
  if (any(u > 1) || any(u < 0))
    stop("'u' values have to be in the interval [0,1]!")

  if (is.null(bins)) {
    n <- length(u)
    bins <- round(sqrt(n))
  }

  #count bins
  classes <- cut(u, breaks = 0:bins/bins, include.lowest = TRUE, right = TRUE)
  cnt <- as.numeric(table(classes))
  x <- u

  if (type == "relative") {
  h <- ggplot(data = data.frame(x = x), aes(x = x)) +
    geom_histogram(aes(y = (stat(cnt) / sum(cnt))), breaks = seq(0, 1, length.out = bins + 1), colour = "grey") +
    geom_hline(yintercept = 1/bins, linetype = "dashed", color = "black") +
    xlab("PIT") +
    ylab("Relative Frequency") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  } else if (type == "absolute") {
    h <- ggplot(data = data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = stat(cnt)), breaks = seq(0, 1, length.out = bins + 1), colour = "grey") +
      geom_hline(yintercept = length(x)/bins, linetype = "dashed", color = "black") +
      xlab("PIT") +
      ylab("Absolute Frequency") +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  } else if (type == "density") {
    h <- ggplot(data = data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = (stat(cnt) / sum(cnt))  * length(cnt)), breaks = seq(0, 1, length.out = bins + 1), colour = "grey") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      xlab("PIT") +
      ylab("Density") +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  } else {
    stop("This type is not available!")
  }

  if (var == TRUE) {
    var.pit <- round(var(x), 3)
    h <- h + labs(subtitle = paste0("Var(PIT) = ", sep = "", var.pit))
  }

  if (var == TRUE & m == FALSE) {
    var.pit <- round(var(x), 4)
    h <- h + labs(subtitle = paste0("Var(PIT) = ", sep = "", var.pit))
  } else if (var == FALSE & m == TRUE) {
    m.pit <- round(mean(x), 4)
    h <- h + labs(subtitle = paste0("E(PIT) = ", sep = "", m.pit))
  } else if (var == TRUE & m == TRUE) {
    var.pit <- round(var(x), 4)
    m.pit <- round(mean(x), 4)
    h <- h + labs(subtitle = paste0("Var(PIT) =", sep = " ", var.pit, ", E(PIT) = ", m.pit))

  }


  return(h)
}

