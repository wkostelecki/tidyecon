
context("lm_fit")

test_that("coefficients match lm() for standard use", {

  lm_fit = lm_fit(as.matrix(cbind(`(Intercept)` = 1,
                                  mtcars[c("cyl", "disp", "hp")])),
                  mtcars[["mpg"]])

  lm = lm(mpg ~ cyl + disp + hp, mtcars)

  expect_equal(lm_fit[["coefficients"]],
               lm[["coefficients"]])

})




