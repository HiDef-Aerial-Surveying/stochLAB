test_that("Sampled quantiles are returned",{
  set.seed(1234)
  sampd <- sample_qtls(10,c(0.1,0.2,0.3),qtls=c(0.05,0.1,0.95))
  expect_equal(
    sampd[1],
    expected = 0.052807659
  )
  expect_equal(
    length(sampd),
    expected = 10
  )
  expect_equal(
    sampd[10],
    expected = 0.103586294
  )

})
