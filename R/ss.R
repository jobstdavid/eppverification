#' Skill Score
#'
#' This function calculates the Skill Score (SS) of the mean score time series.
#'
#' @param s1 vector of scores from method 1
#' @param s2 vector of scores from method 2
#' @param na.rm logical; if \code{TRUE} NA are stripped before the computation proceeds; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' @details
#' The Skill Score (SS) uses the mean scores \code{s1} of the forecasting method of interest (method 1) and \code{s2} of the benchmark method (method 2).
#' Consequently, the Skill Score can be calculated via
#'
#' \deqn{\text{SS}=1-\frac{s_1}{s_2},}
#'
#' indicating the relative improvement of method 1 over method 2.
#'
#' Skill scores are positively oriented with a maximum value of 1.
#' Positive values indicate an improvement of method 1 over method 2, negative values stand for a worse performance of method 1 over method 2.
#'
#' @return
#' Skill score.
#'
#' @examples
#' #simulated data
#' s1 <- rnorm(100)
#' s2 <- rnorm(100)
#'
#' ss(s1 = s1, s2 = s2)
#'
#' @references
#' Gneiting, T. and Raftery, A. (2007). Strictly proper scoring rules, prediction, and estimation. Journal of the American Statistical Association, 102, 359-378.
#'
#' @author David Jobst
#'
#' @rdname ss
#'
#' @export
ss <- function(s1, s2, na.rm = FALSE) {

  if (length(s1) != length(s2)) {
    stop("Imput vectors must have same length!")
  }

  if (na.rm) {
    s <- na.omit(cbind(s1, s2))
    s1 <- s[, 1]
    s2 <- s[, 2]
  }

  ss.value <- 1 - s1/s2

  return(as.numeric(ss.value))

}


