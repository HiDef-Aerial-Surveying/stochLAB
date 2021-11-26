test_that("crm_opt1 equates Band spreadsheet output (page 2, avoidance 96%)", {

  expect_equal(
    crm_opt1(flux_factor = band_spreadsheet_dt$flux_fct,
             prop_crh_surv = band_spreadsheet_dt$prop_crh_surv,
             prob_single_collision = band_spreadsheet_dt$prob_single_collision,
             prop_operational = band_spreadsheet_dt$prop_oper,
             avoidance_rate = band_spreadsheet_dt$avoid_rate,
             lac_factor = band_spreadsheet_dt$lac_factor),
    expected = band_spreadsheet_dt$collisions_opt1)

})
