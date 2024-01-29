test_that("get_fhd_rotor equates Band spreadsheet output (column d(y), page 4)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    get_fhd_rotor(
      hub_height = band_spreadsheet_dt$hub_ht,
      fhd = band_spreadsheet_dt$fhd$prop,
      rotor_radius = band_spreadsheet_dt$rotor_radius,
      tidal_offset = band_spreadsheet_dt$tid_offset)$dy,
    expected = band_spreadsheet_dt$d_y$dy,
    tolerance = 1e-7
  )

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    get_fhd_rotor(
      hub_height = band_spreadsheet_dt_2$hub_ht,
      fhd = band_spreadsheet_dt_2$fhd$prop,
      rotor_radius = band_spreadsheet_dt_2$rotor_radius,
      tidal_offset = band_spreadsheet_dt_2$tid_offset)$dy,
    expected = band_spreadsheet_dt_2$d_y$dy,
    tolerance = 1e-7
  )

})

