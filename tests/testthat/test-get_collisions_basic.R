test_that("get_collisions_basic equates Band spreadsheet output (page 2 - option 1)", {
  expect_equal(
    get_collisions_basic(n_transits = band_spreadsheet_dt$n_transits_opt1,
                         prob_single_collision = band_spreadsheet_dt$prob_single_collision,
                         prop_operational = band_spreadsheet_dt$prop_oper,
                         avoidance_rate = band_spreadsheet_dt$avoid_rate,
                         lac_factor = band_spreadsheet_dt$lac_factor),
    expected = band_spreadsheet_dt$collisions_opt1)

})
