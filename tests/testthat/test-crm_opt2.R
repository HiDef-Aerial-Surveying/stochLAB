test_that("crm_opt2 equates Band spreadsheet output (page 2)", {

  expect_equal(
    crm_opt2(gen_d_y = band_spreadsheet_dt$d_y,
             flux_factor = band_spreadsheet_dt$flux_fct,
             prob_single_collision = band_spreadsheet_dt$prob_single_collision,
             prop_operational = band_spreadsheet_dt$prop_oper,
             avoidance_rate = band_spreadsheet_dt$avoid_rate,
             lac_factor = band_spreadsheet_dt$lac_factor),
    expected = band_spreadsheet_dt$collisions_opt2)
})
