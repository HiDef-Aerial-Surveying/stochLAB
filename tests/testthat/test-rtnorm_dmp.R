test_that("rtnorm_dmp returns warning with NA",{
  expect_warning(
    rtnorm_dmp(n = 1, mean = 1, sd = 0, lower = 1),
    "mean is equal to lower bound - NAs produced"
  )
})
