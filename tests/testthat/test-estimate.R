context("estimate")


test_that("base case works", {

  model_spec = data.frame(expr = c("1", "cyl", "hp", "disp"))
  options = list(y = "mpg")
  model = estimate(mtcars, model_spec, options)

  lm = lm(mpg ~ cyl + hp + disp, mtcars)

  expect_equal(unname(model[["coefficients"]]),
               unname(lm[["coefficients"]]))

})
