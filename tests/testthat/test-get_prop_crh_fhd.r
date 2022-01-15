test_that("get_prop_crh_fhd equates Band spreadsheet output (Q'2R, page 4)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    get_prop_crh_fhd(band_spreadsheet_dt$d_y),
    expected = band_spreadsheet_dt$prop_crh_fhd
  )

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    get_prop_crh_fhd(band_spreadsheet_dt_2$d_y),
    expected = band_spreadsheet_dt_2$prop_crh_fhd
  )

})
