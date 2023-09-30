#' Benjamini-Hochberg-Procedure
#'
#' This function performs the Benjamini-Hochberg-Procedure for different p-values in the multiple testing setting.
#' This function computes the significance of the individual tests.
#'
#' @param p vector of p-values on which the Benjamini-Hochberg-Procedure should be applied
#' @param alpha numeric; false discovery rate at level alpha; alpha is number between 0 and 1
#' @param na.rm logical; if \code{TRUE} NA are removed after the computation; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @return
#' A logical vector where the value TRUE indicates that the p-value at the
#' corresponding  position is significant after the Benjamini-Hochberg-Procedure
#' with false discovery rate alpha. If the value is FALSE the p-value at the
#' corresponding  position is not significant after the
#' Benjamini-Hochberg-Procedure with false discovery rate alpha.
#'
#' @examples
#' #simulated data
#' p <- runif(100)
#'
#' #Benjamini-Hochberg-Procedure
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
#' @export
bh.test <- function(p, alpha = 0.05, na.rm = FALSE){

  if (!is.vector(p)) {
    stop("'p' should be a vector!")
  }
  if (any(p > 1) || any(p < 0)) {
    stop("'p' values have to be in the interval [0,1]!")
  }
  if ((alpha >= 1) || (alpha <= 0)) {
    stop("'alpha' has to be in the interval (0,1)!")
  }

  if(na.rm) {
    p <- p[is.finite(p)]
  }

  n <- length(p)

  #Ranks of p-values
  p_rank <- rank(x = p, na.last = NA, ties.method = "random")

  #Limits for sorted p-values
  limits <- alpha*(1:n)/n

  #p-values smaller than their limit TRUE, otherwise FALSE
  out <- (p <= limits[p_rank])

  return(out)

}
