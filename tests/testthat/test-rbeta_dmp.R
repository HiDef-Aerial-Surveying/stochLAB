test_that("Check that sampler works",{
  set.seed(99)
  expect_equal(
    rbeta_dmp(n=100,p=0.9,sd=0.01)[1],
    expected = 0.89754822
  )
})
