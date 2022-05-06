test_that("Caclulate_ID works correctly",{
  x <- c(rep(0.01,90),0.1)
  expect_equal(calculate_CI(x, 0.05), c(r = 91, l = 6))
})



