test_that("months are returned as 3 letter codes",{
  expect_equal(
    format_months("January"),
    expected="Jan"
  )
})
