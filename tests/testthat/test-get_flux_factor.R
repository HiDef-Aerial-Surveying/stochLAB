test_that("get_flux_factor equates Band spreadsheet output ('Stage A', page 2)", {

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

})
