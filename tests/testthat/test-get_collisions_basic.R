test_that("get_collisions_basic equates Band spreadsheet output (page 2 - option 1)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    get_collisions_basic(
      n_transits = band_spreadsheet_dt$n_transits_opt1,
      avg_prob_coll = band_spreadsheet_dt$avg_prob_coll,
      mth_prop_oper = band_spreadsheet_dt$prop_oper,
      avoidance_rate = band_spreadsheet_dt$avoid_rate,
      lac_factor = band_spreadsheet_dt$lac_factor),
    expected = band_spreadsheet_dt$collisions_opt1
    )

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    get_collisions_basic(
      n_transits = band_spreadsheet_dt_2$n_transits_opt1,
      avg_prob_coll = band_spreadsheet_dt_2$avg_prob_coll,
      mth_prop_oper = band_spreadsheet_dt_2$prop_oper,
      avoidance_rate = band_spreadsheet_dt_2$avoid_rate,
      lac_factor = band_spreadsheet_dt_2$lac_factor),
    expected = band_spreadsheet_dt_2$collisions_opt1
  )
})
