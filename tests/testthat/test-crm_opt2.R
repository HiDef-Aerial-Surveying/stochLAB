test_that("crm_opt2 equates Band spreadsheet output (page 2)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    crm_opt2(d_y = band_spreadsheet_dt$d_y,
             flux_factor = band_spreadsheet_dt$flux_fct,
             avg_prob_coll = band_spreadsheet_dt$avg_prob_coll,
             mth_prop_oper = band_spreadsheet_dt$prop_oper,
             avoidance_rate = band_spreadsheet_dt$avoid_rate,
             lac_factor = band_spreadsheet_dt$lac_factor),
    expected = band_spreadsheet_dt$collisions_opt2)

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    crm_opt2(d_y = band_spreadsheet_dt_2$d_y,
             flux_factor = band_spreadsheet_dt_2$flux_fct,
             avg_prob_coll = band_spreadsheet_dt_2$avg_prob_coll,
             mth_prop_oper = band_spreadsheet_dt_2$prop_oper,
             avoidance_rate = band_spreadsheet_dt_2$avoid_rate,
             lac_factor = band_spreadsheet_dt_2$lac_factor),
    expected = band_spreadsheet_dt_2$collisions_opt2)

})
