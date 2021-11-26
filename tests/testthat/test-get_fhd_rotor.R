test_that("get_fhd_rotor equates Band spreadsheet output (column d(y), page 4)", {

  expect_equal(
    get_fhd_rotor(
      hub_height = band_spreadsheet_dt$hub_ht,
      fhd = band_spreadsheet_dt$fhd,
      rotor_radius = band_spreadsheet_dt$rotor_radius,
      tide_off = band_spreadsheet_dt$tid_offset),
    expected = band_spreadsheet_dt$d_y
  )

})

