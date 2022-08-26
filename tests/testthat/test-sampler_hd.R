test_that("sampler returning correct sample length", {
  expect_equal(
    length(sampler_hd(dat=0.1,
               mode='rtnorm',
               n=100,
               mean=9,
               sd=0.1)),
    expected = 100
  )

  set.seed(999)
  expect_equal(
    mean(sampler_hd(dat=0.1,
                    mode='rtnorm',
                    n=100,
                    mean=9,
                    sd=0.1)),
    8.989281,
    tolerance = 0.00001
  )

  set.seed(999)
  expect_equal(
    mean(sampler_hd(dat=0.1,
                    mode='rnorm',
                    n=100,
                    mean=9,
                    sd=1)),
    8.892806,
    tolerance = 0.00001
  )


  set.seed(999)
  expect_equal(
    mean(sampler_hd(dat=0.1,
                    mode='rbeta',
                    n=100,
                    mean=0.1,
                    sd=0.001)),
    0.09995136,
    tolerance = 0.00001
  )



})
