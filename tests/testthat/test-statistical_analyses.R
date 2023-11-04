test_that("caclulate_CI works correctly",{
  x <- data.frame(prob=c(rep(0.9/100,100),0.1),
                  upper_part_proportion = seq(0,1, length.out = 101))
  expect_equal(overwintering:::calculate_CI(x, 0.05), c(lower_limit = 0.05,
                                                        maximum_likelihood = 1,
                                                        upper_limit = 1))
})
