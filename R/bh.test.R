#' Benjamini-Hochberg-Procedure
#'
#' This function performs the Benjamini-Hochberg-Procedure for different p-values in the multiple testing setting.
#' This function computes the significance of the individual tests.
#'
#' @param p vector of p-values on which the Benjamini-Hochberg-Procedure should be applied
#' @param alpha numeric; false discovery rate at level alpha; alpha is number between 0 and 1
#' @param na.action function to handle the NA's. Default: \code{na.omit}.
#'
#' @return
#' A logical vector where the value \code{TRUE} indicates that the p-value at the
#' corresponding  position is significant after the Benjamini-Hochberg-Procedure
#' with false discovery rate \code{alpha}. If the value is \code{FALSE} the p-value at the
#' corresponding  position is not significant after the
#' Benjamini-Hochberg-Procedure with false discovery rate \code{alpha}.
#'
#' @examples
#' # simulated data
#' p <- runif(100, 0, 0.06)
#'
#' # Benjamini-Hochberg-Procedure
#' bh.test(p = p)
#' bh.test(p = p, alpha = 0.1)
#'
#'
#' @references
#' Benjamini, Y. and Hochberg, Y. (1995). Controlling the False Discovery Rate: A Practical and Powerful Approach to Multiple Testing. Journal of the Royal Statistical Society, 57, 289-300.
#'
#' @author David Jobst
#' @rdname bh.test
#'
#' @importFrom utils tail
#' @export
bh.test <- function(p, alpha = 0.05, na.action = na.omit) {

  if (!is.vector(p)) {
    stop("'p' should be a vector!")
  }
  if (any(p > 1) || any(p < 0)) {
    stop("'p' values have to be in the interval [0,1]!")
  }
  if ((alpha >= 1) || (alpha <= 0)) {
    stop("'alpha' has to be in the interval (0,1)!")
  }

  # handle NA
  p <- as.vector(na.action(p))

  n  <- length(p)
  r <- rank(x = p, ties.method = "random")
  p_adj <- alpha * r/n
  bh <- p <= p_adj
  df <- data.frame(r = r, p = p, p_adj = p_adj, bh = bh)
  df <- df[order(df$r), ]
  crit <- tail(which(df$bh), 1)
  if (length(crit) != 0) {
    p_crit <- df$p[crit]
    out <- p <= p_crit
  } else {
    out <- rep(FALSE, n)
  }

  return(out)

}
