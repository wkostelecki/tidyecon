context("utils")

test_that("vnums works", {

  expect_equal(vnums(5), paste0("v", seq_len(5)))

  target = c("v01", "v02", "v03", "v04", "v05",
             "v06", "v07", "v08", "v09", "v10")

  expect_equal(vnums(10), target)

  expect_equal(vnums(10, factor = TRUE),
               factor(target, target))

})
