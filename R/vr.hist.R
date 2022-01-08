#' Verification Rank Histogram
#'
#' This function plots the Verification Rank Histogram (VRH) given observations of a one-dimensional variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y vector of observations
#' @param x matrix of ensemble forecasts/samples of a predictive distribution (depending on \code{y}; see details)
#' @param bins numeric; if \code{NULL} the number of bins is equal to \code{ncol(x)+1}; otherwise \code{bins} must be chosen so that \code{(ncol(x)+1)/bins} is an integer; default: \code{NULL} (see details)
#' @param type character; "\code{relative}", "\code{absolute}" and "\code{density}"; default: "\code{relative}" (see details)
#' @param title character; title of the plot; default: "\code{Verification Rank Histogram}"
#' @param ri logical; if \code{TRUE} the reliability index is calculated for the plot (see details); if \code{FALSE} the reliability index is not calculated; default: \code{FALSE}
#'
#' @details
#' For a vector \code{y} of length n, \code{x} should be given as matrix
#' with n rows, where the i-th entry of \code{y} belongs to the i-th row
#' of \code{x}. The columns of \code{x} represent the samples of a predictive distribution
#' or ensemble forecasts.
#' Only finite values of \code{y} and \code{x} are used.
#'
#' The parameter \code{bins} specifies the number of columns for the VRH. For "large"
#' \code{ncol(x)} it is often reasonable to reduce the resolution of the VRH by
#' using \code{bins} so that \code{(ncol(x)+1)/bins} is an integer.
#'
#' If \code{type} is "\code{relative}" the relative frequencies of the bins are plotted.
#' If \code{type} is "\code{absolute}" the absolute frequencies of the bins are plotted.
#' If \code{type} is "\code{density}" the relative densities of the bins are plotted.
#'
#' An uniform VRH indicates a calibrated predictive distribution or ensemble forecasts. A ∩-shape in the
#' VRH indicates overdispersion and a ∪-shape indicates underdispersion
#' of the predictive distribution or ensemble forecasts. A systematic bias of the predictive distribution or ensemble forecasts
#' results in a triangular shaped VRH histogram.
#'
#' The deviation from uniformity of the VRH can be quantified by the reliability index (RI).
#' The smaller the RI, the better is the calibration of the forecast. The
#' optimal value of the RI is 0.
#'
#' @return
#' ggplot object with a plot of the Verification Rank Histogram.
#'
#' @examples
#' #simulated data
#' n <- 30
#' m <- 50
#' y <- rnorm(n)
#' x <- matrix(rnorm(n*m), ncol = m)
#'
#' #vr.hist plot
#' vr.hist(y = y, x = x)
#' vr.hist(y = y, x = x, bins = 17, title = "VRH", ri = TRUE)
#' vr.hist(y = y, x = x, bins = 17, type = "absolute", ri = TRUE)
#' vr.hist(y = y, x = x, bins = 17, type = "density", ri = TRUE)
#'
#' @references
#' Anderson, J. (1996). A method for producing and evaluating probabilistic forecasts from ensemble model integrations. Journal of Climate, 9, 1518-1530.
#'
#' Candille, G. and Talagrand, O. (2005). Evaluation of probabilistic prediction systems for a scalar variable. Quarterly Journal of the Royal Meteorological Society, 131(609), 2131-2150.
#'
#' Delle Monache, L., Hacker, J., Zhou, Y., Deng, X. and Stull, R., (2006). Probabilistic aspects of meteorological and ozone regional ensemble forecasts. Journal of Geophysical Research: Atmospheres, 111, D24307.
#'
#' Hamill, T. and Colucci, S. (1997). Verification of Eta-RSM short-range ensemble forecasts. Monthly Weather Review, 125, 1312-1327.
#'
#' Hamill, T. (2001). Interpretation of rank histograms for verifying ensemble forecasts. Monthly Weather Review, 129, 550-560.
#'
#' Talagrand, O., Vautard, R. and Strauss, B. (1997). Evaluation of probabilistic prediction systems. Workshop on Predictability (ECMWF), 1-25.
#'
#' @author David Jobst
#'
#' @rdname vr.hist
#'
#' @importFrom ggplot2 ggplot geom_histogram geom_hline ggtitle aes labs xlab ylab scale_x_continuous theme element_text stat
#' @export
vr.hist <- function (y, x, bins = NULL, type = "relative", title = "Verification Rank Histogram", ri = FALSE) {

  ranks <- rnk(y, x)
  k <- ncol(x)

  if (!is.null(bins)) {
    z <- (k+1)/bins
    if (!(z%%1==0)) {
      stop("'bins' must be an integer, so that (ncol(x)+1)/bins is an integer, too!")
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

  x <- ranks
  if (type == "relative") {
    h <- ggplot(data = data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = stat(cnt) / sum(cnt)), bins = bins, colour = "grey") +
      xlab("Rank") +
      ylab("Relative Frequency") +
      ggtitle(title) +
      scale_x_continuous(breaks = seq(1, bins, 1)) +
      geom_hline(yintercept = 1/bins, linetype = "dashed", color = "black") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  }
  else if (type == "absolute") {
    h <- ggplot(data = data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = stat(cnt)), bins = bins, colour = "grey") +
      xlab("Rank") +
      ylab("Absolute Frequency") +
      ggtitle(title) +
      scale_x_continuous(breaks = seq(1, bins, 1)) +
      geom_hline(yintercept = length(ranks)/bins, linetype = "dashed", color = "black") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }
  else if (type == "density") {
    h <- ggplot(data = data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = (stat(cnt) / sum(cnt)) * length(cnt)), bins = bins, colour = "grey") +
      xlab("Rank") +
      ylab("Density") +
      ggtitle(title) +
      scale_x_continuous(breaks = seq(1, bins, 1)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }
  else {
    stop("This type is not available!")
  }

  if (ri == TRUE) {
    ri <- sum(abs(cnt/length(ranks) - 1/bins))
    rel.index <- round(ri, 3)
    h <- h + labs(subtitle = paste0("RI = ", sep = "", rel.index))
  }
  return(h)
}

