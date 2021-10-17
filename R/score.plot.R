#' Score Plot
#'
#' This function plots scores of different models rated by selected measures as heatmap.
#'
#' @param x matrix of scores (see detail)
#' @param models character; column names of \code{x} (depending on \code{x}; see details)
#' @param measures character; row names of \code{x} (depending on \code{x}; see details)
#' @param title character; title of the plot; default: "\code{Model Comparison}"
#'
#' @details
#' For matrix \code{x}, each row contains the scores of one measure and
#' each columns contains the scores belonging to one model.
#' Only finite values of \code{x} are used.
#'
#' The brighter the color in the plot, the lower the score is.
#'
#' @return
#' ggplot object with score plot of different models rated by selected measures.
#'
#' @examples
#' #simulated data
#' x <- matrix(c(0.5, 0.3, 0.8, 0.21, 1.5, 0.7, 2, 1), byrow = TRUE, ncol = 4)
#' models <- c("A", "B", "C", "D")
#' measures <- c("CRPS", "LogS")
#' title <- ""
#'
#' #score.plot plot
#' score.plot(x = x, models = models, measures = measures)
#' score.plot(x = x, models = models, measures = measures, title = title)
#'
#' @author David Jobst
#'
#' @rdname score.plot
#'
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradient geom_text ggtitle scale_x_discrete scale_y_discrete aes labs xlab ylab theme element_text
#' @export
score.plot <- function(x, models, measures, title = "Model Comparison") {

  if (!is.matrix(x)) {
    stop("'x' should be a matrix!")
  }
  if (any(!is.finite(x))) {
    stop("'x' should contain finite values!")
  }
  if (!is.vector(models)) {
    stop("'models' should be a vector!")
  }
  if (!is.vector(measures)) {
    stop("'measures' should be a vector!")
  }
  if (ncol(x) != length(models)) {
    stop("Length of 'models' is not appropriate!")
  }
  if (nrow(x) != length(measures)) {
    stop("Length of 'measures' is not appropriate!")
  }

  mo <- rep(models, times = length(measures))
  me <- rep(measures, each = length(models))
  value <- as.vector(t(x))
  data <- data.frame(models = mo, measures = me, value)
  resc <- as.vector(sapply(measures, function(k) rescale(data[data$measures == k, "value"])))
  data <- data.frame(data, resc)

  p <- ggplot(data, aes(models, measures)) +
    geom_tile(aes(fill = resc), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(label = value), color = "black") +
    labs(x = "Model", y = "Measure") +
    ggtitle(title) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

  return(p)

}
#'
#' internal function
#' @noRd
rescale <- function(x) {
  (x-min(x))/(max(x) - min(x))
}

