test_that("get_prop_crh_fhd equates Band spreadsheet output (Q'2R, page 4)", {

  expect_equal(
    get_prop_crh_fhd(band_spreadsheet_dt$d_y),
    band_spreadsheet_dt$prop_crh_fhd
  )

})
