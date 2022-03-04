test_that("DayLength calculation gets the correct day length",{
  expect_equal(
    DayLength(54.6)[1,],
    expected = data.frame(Month="Jan",
                          Total=744,
                          Day=245.39297,
                          Night=498.60703)
  )
})
