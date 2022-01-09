#' Multivariate Ranks
#'
#' This function calculates the ranks given observations of a multivariate variable and ensemble forecasts/samples of a predictive distribution.
#'
#' @param y matrix of observations (see details)
#' @param x 3-dimensional array of ensemble forecasts/samples of a predictive distribution (depending on \code{y}; see details)
#' @param method character; "\code{mv}", "\code{avg}", "\code{mst}", "\code{bd}"; default: "\code{mv}" (see details)
#'
#' @details
#' The observations are given in the matrix \code{y} with n rows, where each column belongs to an univariate observation variable.
#' The i-th row of matrix \code{y} belongs to the i-th third dimension entry of the array \code{x}. The i-th third dimension
#' entry must be a matrix with n rows, having the same structure as \code{y}, filled with the ensemble forecasts or samples of a predictive distribution.
#' Only finite values of \code{y} and \code{x} are used.
#'
#' For the calculation of the ranks, different methods are available, where "\code{mv}" stands for "multivariate ranks",
#' "\code{avg}" stands for "average ranks", "\code{mst}" stands for "minimum-spanning-tree ranks" and
#' "\code{bd}" stands for "band-depth ranks". These methods are implemented as described in e.g. Thorarinsdottir et al. (2016).
#'
#' @return
#' Vector of ranks.
#'
#' @examples
#' #simulated data
#' n <- 30
#' m <- 50
#' y <- cbind(rnorm(n), rgamma(n, shape = 1))
#' x <- array(NA, dim = c(m, 2, n))
#' x[, 1, ] <- rnorm(n*m)
#' x[, 2, ] <- rgamma(n*m, shape = 1)
#'
#' #mrnk calculation
#' mrnk(y = y, x = x, method = "mv")
#' mrnk(y = y, x = x, method = "avg")
#' mrnk(y = y, x = x, method = "mst")
#' mrnk(y = y, x = x, method = "bd")
#'
#' @references
#' Gneiting, T., Stanberry, L., Grimit, E., Held, L. and Johnson, N. (2008). Assessing probabilistic forecasts of multivariate quantities, with an application to ensemble predictions of surface winds. Test, 17, 211-264.
#'
#' Smith, L. and Hansen, J. (2004). Extending the limits of ensemble forecast verification with the minimum spanning tree. Monthly Weather Review, 132, 1522-1528.
#'
#' Thorarinsdottir, T., Scheurer, M. and Heinz, C. (2016). Assessing the calibration of high-dimensional ensemble forecasts using rank histograms. Journal of Computational and Graphical Statistics, 25, 105-122.
#'
#' Wilks, D. (2004). The minimum spanning tree histogram as verification tool for multidimensional ensemble forecasts. Monthly Weather Review, 132, 1329-1340.
#'
#' @author David Jobst
#'
#' @rdname mrnk
#'
#' @importFrom fields rdist
#' @importFrom vegan spantree
#' @export
mrnk <- function(y, x, method = "mv") {
  #y is a matrix where the columns represent the obs. variables and the rows stand for the time points
  #x is a 3-dimensional array, where each matrix in that array stands for a time point. In each matrix the columns represent the obs. variables
  #and the rows represent the number of ensemble members/samples

  if (!is.matrix(y)) {
    stop("'y' should be a matrix!")
  }
  if (!is.array(x)) {
    stop("'x' should be a 3-dimensional array!")
  }
  dimensions <- apply(x, 3, dim)
  if (nrow(y) != ncol(dimensions)) {
    stop("The third dimension of 'x' and the number of rows of 'y' are not equal!")
  }
  if(length(unique(dimensions[1, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of rows!")
  }
  if(length(unique(dimensions[2, ])) != 1) {
    stop("The entries of 'x' don't have equal numbers of columns!")
  }
  if(dimensions[2, 1] != ncol(y)) {
    stop("The number of columns of the entries of 'x' is not equal with the number of columns of 'y'!")
  }

  index <- which(apply(is.finite(y), 1, all))

  rank <- c()

  ## Multivariate ranks
  if (method == "mv") {
    for(i in index) {
      x.data <- x[, , i]
      x.data <- matrix(x.data[apply(is.finite(x.data), 1, all), ], ncol = ncol(x.data))
      data <- cbind(y[i, ], t(x.data))

      d <- dim(data)
      x.prerank <- numeric(d[2])
      for(i in 1:d[2]) {
        x.prerank[i] <- sum(apply(data <= data[, i], 2, all))
      }
      x.rank <- rank(x.prerank, ties = "random")[1]

      rank <- c(rank, x.rank)
    }
    ## Average ranks
  } else if (method == "avg") {
    for(i in index) {
      x.data <- x[, , i]
      x.data <- matrix(x.data[apply(is.finite(x.data), 1, all), ], ncol = ncol(x.data))
      data <- cbind(y[i, ], t(x.data))

      x.ranks <- apply(data, 1, rank)
      x.preranks <- apply(x.ranks, 1, mean)
      x.rank <- rank(x.preranks, ties = "random")[1]

      rank <- c(rank, x.rank)
    }
    ## Minimum spanning tree ranks
  } else if (method == "mst") {
    for(i in index) {
      x.data <- x[, , i]
      x.data <- matrix(x.data[apply(is.finite(x.data), 1, all), ], ncol = ncol(x.data))
      data <- cbind(y[i, ], t(x.data))

      l.mst <- NULL
      for(k in 1:(dim(data)[2])) {
        euc.dist <- rdist(t(data[, -k]))
        l.mst <- c(l.mst, sum(spantree(euc.dist)$dist))
      }
      x.rank <- rank(l.mst, ties = "random")[1]

      rank <- c(rank, x.rank)
    }
    ## Band depth ranks
  } else if (method == "bd") {
    for(i in index) {
      x.data <- x[, , i]
      x.data <- matrix(x.data[apply(is.finite(x.data), 1, all), ], ncol = ncol(x.data))
      data <- cbind(y[i, ], t(x.data))

      d <- dim(data)
      x.prerank <- array(NA, dim = d)
      for(i in 1:d[1]) {
        tmp.ranks <- rank(data[i, ])
        x.prerank[i, ] <- (d[2] - tmp.ranks) * (tmp.ranks - 1)
      }
      x.rank <- apply(x.prerank, 2, mean) + d[2] - 1
      x.rank <- rank(x.rank, ties = "random")[1]

      rank <- c(rank, x.rank)
    }
  } else {
    stop("This method is not available!")
  }

  return(rank)
}
