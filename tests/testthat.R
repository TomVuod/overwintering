library(testthat)
library(overwintering)

test_check("overwintering")
test_that("Caclulate_ID works correctly",{
  x <- cumsum(rep(1, 20))
  x <- x/sum(x)
  expect_equal(calculate_CI(x), c(r = 20, l = 2))
})

