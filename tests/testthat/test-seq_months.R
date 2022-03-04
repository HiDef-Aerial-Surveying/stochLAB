test_that("month sequence gets wrapped when season extends over December", {
  expect_equal(
    seq_months("Oct", "Mar"),
    expected = c("Oct","Nov","Dec","Jan","Feb","Mar")
  )
})
