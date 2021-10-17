context("Correct scores for univariate verification?")

test_that("crps", {
  n <- 30
  y <- rep(0, n)
  x <- matrix(1, ncol = 50, nrow = n)

  expect_equal(crps(y, x, method = "ens", mean = FALSE), rep(1, n))
  expect_equal(crps(y, x, method = "ens", mean = TRUE), 1)
  expect_equal(crps(y, x, method = "sml", mean = FALSE), rep(1, n))
  expect_equal(crps(y, x, method = "sml", mean = TRUE), 1)
  expect_equal(crps(y, x, method = "mc", mean = FALSE), rep(1, n))
  expect_equal(crps(y, x, method = "mc", mean = TRUE), 1)
})

test_that("logs", {
  n <- 30
  y <- 1:n

  expect_equal(logs(y, mean = FALSE), -log(y))
  expect_equal(logs(y, mean = TRUE), mean(-log(y)))

})

test_that("dss", {
  n <- 30
  y <- 1:n
  lower <- rnorm(n)
  upper <- rnorm(n)
  mu <- rep(0.5, n)
  x1 <- rep(1, n)
  x2 <- matrix(1:1500, ncol = 50, nrow = n)

  expect_equal(dss(y, x1, mu = mu, mean = FALSE), (y-mu)^2)
  expect_equal(dss(y, x1, mu = mu, mean = TRUE), mean((y-mu)^2))
  expect_equal(dss(y, x2, mu = NULL, mean = FALSE), log(apply(x2, 1, var))+(y-apply(x2, 1, mean))^2/apply(x2, 1, var))
  expect_equal(dss(y, x2, mu = NULL, mean = TRUE), mean(log(apply(x2, 1, var))+(y-apply(x2, 1, mean))^2/apply(x2, 1, var)))
})

test_that("cpi", {
  n <- 30
  y <- rnorm(n, mean = 1:n)
  interval.range <- 90
  alpha <- (100-interval.range)/100
  lower <- qnorm(alpha/2, rnorm(n, mean = 1:n))
  upper <- qnorm((1-alpha/2), rnorm(n, mean = 1:n))

  expect_equal(cpi(y, lower, upper, interval.range, separate = "is", mean = FALSE),
               (upper-lower)+2/alpha * (lower - y) * (y < lower)+2/alpha * (y - upper) * (y > upper))
  expect_equal(cpi(y, lower, upper, interval.range, separate = "is", mean = TRUE),
               mean((upper-lower)+2/alpha * (lower - y) * (y < lower)+2/alpha * (y - upper) * (y > upper)))
  expect_equal(cpi(y, lower, upper, interval.range, separate = "overprediction", mean = FALSE),
               2/alpha * (lower - y) * (y < lower))
  expect_equal(cpi(y, lower, upper, interval.range, separate = "overprediction", mean = TRUE),
              mean(2/alpha * (lower - y) * (y < lower)))
  expect_equal(cpi(y, lower, upper, interval.range, separate = "underprediction", mean = FALSE),
               2/alpha * (y - upper) * (y > upper))
  expect_equal(cpi(y, lower, upper, interval.range, separate = "underprediction", mean = TRUE),
               mean(2/alpha * (y - upper) * (y > upper)))
  expect_equal(cpi(y, lower, upper, interval.range, separate = "coverage", mean = FALSE),
               1 * (lower < y & y < upper))
  expect_equal(cpi(y, lower, upper, interval.range, separate = "coverage", mean = TRUE),
               mean(1 * (lower < y & y < upper)))
})

test_that("ae", {
  n <- 30
  m <- 50
  y <- rnorm(n)
  x1 <- matrix(rnorm(n*m), ncol = m)
  x2 <- apply(x1, 1, median)

  expect_equal(ae(y = y, x = x1, mean = FALSE), abs(y-x2))
  expect_equal(ae(y = y, x = x1, mean = TRUE), mean(abs(y-x2)))
  expect_equal(ae(y = y, x = x2, mean = FALSE), abs(y-x2))
  expect_equal(ae(y = y, x = x2, mean = TRUE), mean(abs(y-x2)))
})

test_that("se", {
  n <- 30
  m <- 50
  y <- rnorm(n)
  x1 <- matrix(rnorm(n*m), ncol = m)
  x2 <- apply(x1, 1, mean)

  expect_equal(se(y = y, x = x1, mean = FALSE), (y-x2)^2)
  expect_equal(se(y = y, x = x1, mean = TRUE), mean((y-x2)^2))
  expect_equal(se(y = y, x = x2, mean = FALSE), (y-x2)^2)
  expect_equal(se(y = y, x = x2, mean = TRUE), mean((y-x2)^2))
})

test_that("bs", {
  n <- 30
  y <- rnorm(n)
  z <- sample(-10:35, size = n, replace = TRUE)
  p <- pnorm(z)

  expect_equal(bs(y = y, z = z, p = p, mean = FALSE), (p - 1*(y <= z))^2)
  expect_equal(bs(y = y, z = z, p = p, mean = TRUE), mean((p - 1*(y <= z))^2))
})

test_that("qs", {
  n <- 30
  y <- rnorm(n)
  p <- runif(n)
  q <- qnorm(p)

  expect_equal(qs(y = y, q = q, p = p, mean = FALSE), (q-y) * (1*(y <= q)-p))
  expect_equal(qs(y = y, q = q, p = p, mean = TRUE), mean((q-y) * (1*(y <= q)-p)))
})


test_that("rmv", {
  n <- 30
  m <- 50
  x1 <- matrix(rnorm(n*m), ncol = m)
  x2 <- apply(x1, 1, var)

  expect_equal(rmv(x1), sqrt(mean(x2)))
  expect_equal(rmv(x2), sqrt(mean(x2)))
})

test_that("Var(PIT)", {
  n <- 1000
  u <- runif(n)

  expect_equal(var.pit(u = u), var(u))
})

test_that("rnk, ri", {
  n <- 30
  m <- 50
  y <- 1501:1530
  x <- matrix(1:1500, ncol = 50, nrow = n)

  expect_equal(rnk(y = y, x = x), rep(m+1, n))
  expect_equal(ri(y = y, x = x), 50*1/51 + abs(1-1/51))
})
