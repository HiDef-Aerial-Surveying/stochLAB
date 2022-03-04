test_that("sampler returning correct sample length", {
  expect_equal(
    length(sampler_hd(dat=0.1,
               mode='rtnorm',
               n=100,
               mean=9,
               sd=0.1)),
    expected = 100
  )
})
