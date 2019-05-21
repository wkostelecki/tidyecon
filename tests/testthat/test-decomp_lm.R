context("decomp_lm")

x = matrix(c(1, 1, 0, 0.5), ncol = 2)
options = list(coefficients = c(2, 3))
y = x %*% options[["coefficients"]]

test_that("minimal case works", {

  decomp = decomp_lm(x, y)

  expect_equal(decomp[["id"]], c(1, 2, 1, 2))
  expect_equal(decomp[["vnum"]], c("v1", "v1", "v2", "v2"))
  expect_equal(decomp[["variable"]], c("v1", "v1", "v2", "v2"))
  expect_equal(decomp[["value"]], c(2, 2, 0, 1.5))

})

test_that("named columns feed through", {

  colnames(x) = c("x", "y")
  decomp = decomp_lm(x, y)

  expect_equal(decomp[["id"]], c(1, 2, 1, 2))
  expect_equal(decomp[["vnum"]], c("v1", "v1", "v2", "v2"))
  expect_equal(decomp[["variable"]], c("x", "x", "y", "y"))
  expect_equal(decomp[["value"]], c(2, 2, 0, 1.5))

})


test_that("named columns feed through", {

  colnames(x) = c("x", "y")
  row.names(x) = c("a", "b")
  decomp = decomp_lm(x, y, list(variables = c("m", "n")))

  expect_equal(decomp[["id"]], c("a", "b", "a", "b"))
  expect_equal(decomp[["vnum"]], c("v1", "v1", "v2", "v2"))
  expect_equal(decomp[["variable"]], c("m", "m", "n", "n"))
  expect_equal(decomp[["value"]], c(2, 2, 0, 1.5))

})
