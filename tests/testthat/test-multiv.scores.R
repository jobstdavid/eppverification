context("Correct scores for multivariate verification?")

test_that("es", {
  n <- 30
  y <- matrix(0, ncol = 4, nrow = n)
  x <- array(1, dim = c(50, 4, n))

  expect_equal(es(y, x, method = "ens", mean = FALSE), rep(2, n))
  expect_equal(es(y, x, method = "ens", mean = TRUE), 2)
  expect_equal(es(y, x, method = "mc", mean = FALSE), rep(2, n))
  expect_equal(es(y, x, method = "mc", mean = TRUE), 2)
})

test_that("ee", {
  n <- 30
  y <- matrix(0, ncol = 4, nrow = n)
  x <- array(1, dim = c(50, 4, n))

  expect_equal(ee(y = y, x = x, method = "median", mean = FALSE), rep(2, n))
  expect_equal(ee(y = y, x = x, method = "median", mean = TRUE), 2)
  expect_equal(ee(y = y, x = x, method = "mean", mean = FALSE), rep(2, n))
  expect_equal(ee(y = y, x = x, method = "median", mean = TRUE), 2)

})

test_that("vs", {
  n <- 30
  y <- matrix(0, ncol = 4, nrow = n)
  x <- array(1, dim = c(50, 4, n))

  expect_equal(vs(y = y, x = x, mean = FALSE), rep(0, n))
  expect_equal(vs(y = y, x = x, mean = TRUE), 0)
})

test_that("ds", {
  n <- 30
  m <- 50
  x <- array(NA, dim = c(2, 2, n))
  for (i in 1:n) {
    x[, , i] <- cov(cbind(rnorm(m), rgamma(m, shape = 1)))
  }

  expect_equal(ds(x = x, covmat = TRUE, mean = FALSE), sapply(1:n, function(i) det(x[, , i])^(1/4)))
  expect_equal(ds(x = x, covmat = TRUE, mean = TRUE), mean(sapply(1:n, function(i) det(x[, , i])^(1/4))))

})


test_that("mrnk, mri", {
  n <- 30
  y <- matrix(0, ncol = 4, nrow = n)
  x <- array(1, dim = c(50, 4, n))

  expect_equal(mrnk(y = y, x = x, method = "mv"), rep(1, n))
  expect_equal(mrnk(y = y, x = x, method = "avg"), rep(1, n))
  expect_equal(mrnk(y = y, x = x, method = "mst"), rep(1, n))
  expect_equal(mrnk(y = y, x = x, method = "bd"), rep(1, n))

  expect_equal(mri(y = y, x = x, method = "mv"), 50*1/51 + abs(1-1/51))
  expect_equal(mri(y = y, x = x, method = "avg"), 50*1/51 + abs(1-1/51))
  expect_equal(mri(y = y, x = x, method = "mst"), 50*1/51 + abs(1-1/51))
  expect_equal(mri(y = y, x = x, method = "bd"), 50*1/51 + abs(1-1/51))

})
