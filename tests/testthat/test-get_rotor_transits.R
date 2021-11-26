test_that("get_rotor_transits equates Band spreadsheet (page 2, under Option 1 section)", {

  expect_equal(
    get_rotor_transits(flux_factor = band_spreadsheet_dt$flux_fct,
                       prop_crh = band_spreadsheet_dt$prop_crh_surv),
    expected = band_spreadsheet_dt$n_transits_opt1)

})
