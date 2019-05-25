
context("lm_fit")

test_that("coefficients/residuals match lm() for standard use", {

  lm_fit = lm_fit(as.matrix(cbind(`(Intercept)` = 1,
                                  mtcars[c("cyl", "hp", "disp")])),
                  mtcars[["mpg"]])

  lm = lm(mpg ~ cyl + hp + disp, mtcars)

  expect_equal(lm_fit[["coefficients"]],
               lm[["coefficients"]])

  expect_equal(lm_fit[["model"]][["residuals"]],
               unname(lm[["residuals"]]))

})

test_that("coefficients/residuals match lm() for co-linear case", {

  mtcars[["cyl2"]] = mtcars[["cyl"]]

  lm_fit = lm_fit(as.matrix(cbind(`(Intercept)` = 1,
                                  mtcars[c("cyl", "cyl2", "hp", "disp")])),
                  mtcars[["mpg"]])

  lm = lm(mpg ~ cyl + cyl2 + hp + disp, mtcars)

  expect_equal(lm_fit[["coefficients"]],
               lm[["coefficients"]])

  expect_equal(lm_fit[["model"]][["residuals"]],
               unname(lm[["residuals"]]))

})


test_that("coefficients/residuals match lm() for incomplete observations", {

  airquality = airquality[complete.cases(airquality), ]

  lm_fit = lm_fit(as.matrix(cbind(`(Intercept)` = 1,
                                  airquality[c("Solar.R", "Wind", "Temp", "Month")])),
                  airquality[["Ozone"]])

  remove(airquality)

  lm = lm(Ozone ~ Solar.R + Wind + Temp + Month, airquality)

  expect_equal(lm_fit[["coefficients"]],
               lm[["coefficients"]])

  expect_equal(lm_fit[["model"]][["residuals"]],
               unname(lm[["residuals"]]))

})

test_that("coefficients/residuals match weighted lm()", {

  lm_fit = lm_fit(as.matrix(cbind(`(Intercept)` = 1,
                                  mtcars[c("cyl", "hp", "disp")])),
                  mtcars[["mpg"]],
                  w = seq_len(nrow(mtcars)))

  lm = lm(mpg ~ cyl + hp + disp, mtcars, weights = seq_len(nrow(mtcars)))

  expect_equal(lm_fit[["coefficients"]],
               lm[["coefficients"]])

  expect_equal(lm_fit[["model"]][["residuals"]],
               unname(lm[["residuals"]]))

})
