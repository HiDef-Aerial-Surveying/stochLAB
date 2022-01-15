test_that("get_flux_factor equates Band spreadsheet output ('Stage A', page 2)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    get_flux_factor(
      n_turbines = band_spreadsheet_dt$n_turbines,
      rotor_radius = band_spreadsheet_dt$rotor_radius,
      flight_speed = band_spreadsheet_dt$flight_speed,
      bird_dens = band_spreadsheet_dt$bird_density,
      daynight_hrs = band_spreadsheet_dt$day_night_hrs,
      noct_activity = band_spreadsheet_dt$noct_activity
    ),
    expected = band_spreadsheet_dt$flux_fct
  )

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    get_flux_factor(
      n_turbines = band_spreadsheet_dt_2$n_turbines,
      rotor_radius = band_spreadsheet_dt_2$rotor_radius,
      flight_speed = band_spreadsheet_dt_2$flight_speed,
      bird_dens = band_spreadsheet_dt_2$bird_density,
      daynight_hrs = band_spreadsheet_dt_2$day_night_hrs,
      noct_activity = band_spreadsheet_dt_2$noct_activity
    ),
    expected = band_spreadsheet_dt_2$flux_fct
  )

})
