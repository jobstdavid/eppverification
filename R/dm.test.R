#' Diebold-Mariano-Test
#'
#' This function performs the Diebold-Mariano-Test for equal predictive performance of two forecasts with respect to a scoring rule.
#' This test is a simplified version of \code{\link[forecast]{dm.test}}.
#'
#' @param s1 vector of scores from method 1
#' @param s2 vector of scores from method 2
#' @param alternative character; the alternative
#' hypothesis and must be one of "\code{two.sided}", "\code{greater}" or
#' "\code{less}"; default: "\code{two.sided}"
#' @param h integer; forecast horizon used in calculating s1 and s2; default: 1
#' @param na.rm logical; if \code{TRUE} NA are stripped before the computation proceeds; if \code{FALSE} NA are used in the computation; default: \code{FALSE}
#'
#' The null hypothesis is that the difference \code{s1 - s2} has zero mean.
#' The alternative "\code{less}" is that \code{s1 - s2} has negative mean.
#' The alternative "\code{greater}" is that \code{s1 - s2} has positive mean.
#' The alternative "\code{two.sided}" is that \code{s1 - s2} has mean unequal
#' zero.
#'
#' The difference \code{s1 - s2} may contain missing values,
#' in which case complete cases are used and a warning is given.
#'
#' @return
#' An object of class "\code{htest}".
#'
#' @examples
#' #simulated data
#' s1 <- arima.sim(list(ar = 0.7), sd = 0.5, 100)
#' s2 <- arima.sim(list(ar = 0.7), sd = 0.5, 100) - 0.2
#'
#' #Diebold-Mariano-Test
#' dm.test(s1, s2)
#'
#'
#' @references
#' Diebold, F. and Mariano, R. (1995). Comparing predictive accuracy. Journal of Business & Economic Statistics, 13, 253-263.
#'
#' Gneiting, T. and Katzfuss, M. (2014). Probabilistic forecasting. Annual Review of Statistics and Its Application, 1, 125-151.
#'
#' @author David Jobst
#' @note The function \code{dm.test} is inspired by the function \code{dm.test} from the R-package \code{forecast} by Hyndman et al.
#' @rdname dm.test
#'
#' @importFrom stats complete.cases acf pt
#' @export
dm.test <- function(s1, s2, alternative = c("two.sided", "less", "greater"), h = 1, na.rm = FALSE) {
  if (length(s1) != length(s2)) {
    stop("Imput vectors must have same length!")
  }

  alternative <- match.arg(alternative)
  dname <- paste(deparse(substitute(s1)), deparse(substitute(s2)))
  d <- s1 - s2
  if (any(is.na(d))) {
    warning("Missig values: autocovariance estimate may not be valid!")
  }

  # remove NA
  if (na.rm) {
    d <- d[complete.cases(d)]
  }

  n <- length(d)
  acf_est <- acf(d, type = "covariance", lag.max = h - 1, plot = FALSE)
  d_acf <- acf_est$acf[, , 1]
  d_var <- sum(c(d_acf[1], 2 * d_acf[-1]))/n
  if (d_var < 0) {
    S <- NA
    pval <- 0
  } else {
      S <- mean(d)/sqrt(d_var)
      k <- ((n + 1 - 2 * h + (h/n) * (h - 1))/n)^(1/2)
      S <- S * k
      if (alternative == "two.sided") {
        pval <- 2 * pt(-abs(S), df = n - 1)
      } else if (alternative == "less") {
        pval <- pt(S, df = n - 1)
      } else if (alternative == "greater") {
        pval <- pt(S, df = n - 1, lower.tail = FALSE)
      }
  }
  para <- h
  names(para) <- c("Forecast Horizon")
  out <- list(statistic = c(DM = S), parameter = para, p.value = pval,
               alternative = alternative, method = "Diebold-Mariano Test", data.name = dname)
  class(out) <- "htest"
  return(out)
}


