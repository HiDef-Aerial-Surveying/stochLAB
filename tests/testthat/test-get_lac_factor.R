test_that("get_lac_factor equates Band spreadsheet output (page 7, 96% avoidance)", {

  expect_equal(
    get_lac_factor(n_turbines = band_spreadsheet_dt$n_turbines,
                   rotor_radius = band_spreadsheet_dt$rotor_radius,
                   avoidance_rate = band_spreadsheet_dt$avoid_rate,
                   prob_single_collision = band_spreadsheet_dt$prob_single_collision,
                   mean_prop_operational = band_spreadsheet_dt$mean_prop_oper,
                   wf_width = band_spreadsheet_dt$wf_width),
    expected = band_spreadsheet_dt$lac_factor)

})
