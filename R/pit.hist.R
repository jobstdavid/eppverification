#' Probability Integral Transform Histogram
#'
#' This function plots the Probability Integral Transform (PIT) Histogram for a predictive distribution.
#'
#' @param u vector of PIT values in [0,1] (see details)
#' @param bins numeric; number of bins; default: \code{bins = round(sqrt(length(u)))} (see details)
#' @param type character; "\code{relative}", "\code{absolute}" and "\code{density}"; default: "\code{density}" (see details)
#' @param title character; title of the plot; default: "\code{PIT Histogram}"
#' @param dispersion logical; if \code{TRUE} the variance of the PIT values is calculated for the plot (see details); if \code{FALSE} the variance of the PIT values is not calculated; default: \code{FALSE}
#' @param bias logical; if \code{TRUE} the expectation of the PIT values is calculated for the plot (see details); if \code{FALSE} the expectation of the PIT values is not calculated; default: \code{FALSE}
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#'
#' @details
#' The vector \code{u} contains the PIT values \code{u}=F(\code{x}) for a predictive
#' distribution F evaluated at \code{x}.
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
#' The variance of the PIT values (Var(PIT)) provides information on the dispersion of a predictive distribution subject to calibration.
#' A variance of the PIT values equal to 1/12 ≈ 0.0833 corresponds to the variance of the uniform distribution
#' on [0,1], which is desirable. A variance greater than 1/12 indicates underdispersion and a
#' variance smaller than 1/12 indicates overdispersion of the predictive distribution.
#'
#' The expectation value of the PIT values (E(PIT)) provides information on the bias of a predictive distribution subject to calibration.
#' An expectation of the PIT values equal to 1/2 corresponds to the expectation of the uniform distribution
#' on [0,1], which is desirable. Any deviation from 1/2 indicates that the predictive distribution is biased.
#'
#' @return
#' ggplot object with a plot of the PIT histogram.
#'
#' @examples
#' # simulated data
#' n <- 10000
#' u <- runif(n)
#'
#' # pit plot
#' pit.hist(u = u)
#' pit.hist(u = u, bins = 5, title = "PITH", dispersion = TRUE, bias = FALSE)
#' pit.hist(u = u, bins = 5, type = "relative", dispersion = FALSE, bias = TRUE)
#' pit.hist(u = u, bins = 5, type = "absolute", dispersion = TRUE, bias = TRUE)
#'
#'
#' @references
#' Dawid, A. (1984). Present Position and Potential Developments: Some Personal Views: Statistical Theory: The Prequential Approach. 147(2), 278-292.
#'
#' Gneiting, T., Balabdaoui, F. and Raftery, A. (2007). Probabilistic forecasts, calibration and sharpness. Journal of the Royal Statistical Society, Series B, Statistical Methodology. 69, 243–268.
#'
#' Czado, C., Gneiting, T. and Held, L. (2009). Predictive Model Assessment for Count Data. Biometrics, 65(4), 1254–1261.
#'
#' Gneiting, T. and Ranjan, R. (2013). Combining predictive distributions. Electronic Journal of Statistics, 7, 1747-1782.
#'
#' @author David Jobst
#'
#' @rdname pit.hist
#'
#' @importFrom ggplot2 ggplot geom_histogram geom_hline ggtitle aes labs xlab ylab theme element_text after_stat theme_bw
#' @export
pit.hist <- function(u, bins = NULL, type = "density", title = "PIT Histogram", dispersion = FALSE, bias = FALSE, na.action = na.omit) {
  if (!is.vector(u)) {
    stop("'u' should be a vector!")
  }

  # handle NA and allow only finite values for u
  u <- as.vector(na.action(u))
  if (any(u > 1) || any(u < 0))
    stop("'u' values have to be in the interval [0,1]!")

  if (is.null(bins)) {
    n <- length(u)
    bins <- round(sqrt(n))
  }

  #count bins
  classes <- cut(u, breaks = 0:bins/bins, include.lowest = TRUE, right = TRUE)
  cnt <- as.numeric(table(classes))
  x <- c(0, 1)

  if (type == "relative") {
  h <- ggplot(data = data.frame(x = x), aes(x = x)) +
    geom_histogram(aes(y = (after_stat(cnt) / sum(cnt))), breaks = seq(0, 1, length.out = bins + 1), colour = "black", fill = "gray") +
    geom_hline(yintercept = 1/bins, linetype = "dashed", color = "black") +
    theme_bw() +
    xlab("PIT") +
    ylab("Relative Frequency") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  } else if (type == "absolute") {
    h <- ggplot(data = data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = after_stat(cnt)), breaks = seq(0, 1, length.out = bins + 1), colour = "black", fill = "gray") +
      geom_hline(yintercept = length(x)/bins, linetype = "dashed", color = "black") +
      theme_bw() +
      xlab("PIT") +
      ylab("Absolute Frequency") +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  } else if (type == "density") {
    h <- ggplot(data = data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = (after_stat(cnt) / sum(cnt))  * length(cnt)), breaks = seq(0, 1, length.out = bins + 1), colour = "black", fill = "gray") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      theme_bw() +
      xlab("PIT") +
      ylab("Density") +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  } else {
    stop("This 'type' is not available!")
  }

  if (dispersion == TRUE) {
    v.pit <- round(var(u), 3)
    h <- h + labs(subtitle = paste0("Var(PIT) = ", sep = "", v.pit))
  }

  if (dispersion == TRUE & bias == FALSE) {
    v.pit <- round(var(u), 4)
    h <- h + labs(subtitle = paste0("Var(PIT) = ", sep = "", v.pit))
  } else if (dispersion == FALSE & bias == TRUE) {
    m.pit <- round(mean(u), 4)
    h <- h + labs(subtitle = paste0("E(PIT) = ", sep = "", m.pit))
  } else if (dispersion == TRUE & bias == TRUE) {
    v.pit <- round(var(u), 4)
    m.pit <- round(mean(u), 4)
    h <- h + labs(subtitle = paste0("Var(PIT) =", sep = " ", v.pit, ", E(PIT) = ", m.pit))

  }


  return(h)
}

