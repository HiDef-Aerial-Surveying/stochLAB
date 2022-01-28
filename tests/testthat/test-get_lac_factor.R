test_that("get_lac_factor equates Band spreadsheet output (page 7, 96% avoidance)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    get_lac_factor(
      n_turbines = band_spreadsheet_dt$n_turbines,
      rotor_radius = band_spreadsheet_dt$rotor_radius,
      avoidance_rate = band_spreadsheet_dt$avoid_rate,
      avg_prob_coll = band_spreadsheet_dt$avg_prob_coll,
      avg_prop_operational = band_spreadsheet_dt$mean_prop_oper,
      wf_width = band_spreadsheet_dt$wf_width),
    expected = band_spreadsheet_dt$lac_factor)

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    get_lac_factor(
      n_turbines = band_spreadsheet_dt_2$n_turbines,
      rotor_radius = band_spreadsheet_dt_2$rotor_radius,
      avoidance_rate = band_spreadsheet_dt_2$avoid_rate,
      avg_prob_coll = band_spreadsheet_dt_2$avg_prob_coll,
      avg_prop_operational = band_spreadsheet_dt_2$mean_prop_oper,
      wf_width = band_spreadsheet_dt_2$wf_width),
    expected = band_spreadsheet_dt_2$lac_factor)

})
