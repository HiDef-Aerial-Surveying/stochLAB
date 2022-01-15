test_that("crm_opt1 equates Band spreadsheet output (page 2)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    crm_opt1(flux_factor = band_spreadsheet_dt$flux_fct,
             prop_crh_surv = band_spreadsheet_dt$prop_crh_surv,
             avg_prob_coll = band_spreadsheet_dt$avg_prob_coll,
             mth_prop_oper = band_spreadsheet_dt$prop_oper,
             avoidance_rate = band_spreadsheet_dt$avoid_rate,
             lac_factor = band_spreadsheet_dt$lac_factor),
    expected = band_spreadsheet_dt$collisions_opt1)

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    crm_opt1(flux_factor = band_spreadsheet_dt_2$flux_fct,
             prop_crh_surv = band_spreadsheet_dt_2$prop_crh_surv,
             avg_prob_coll = band_spreadsheet_dt_2$avg_prob_coll,
             mth_prop_oper = band_spreadsheet_dt_2$prop_oper,
             avoidance_rate = band_spreadsheet_dt_2$avoid_rate,
             lac_factor = band_spreadsheet_dt_2$lac_factor),
    expected = band_spreadsheet_dt_2$collisions_opt1)

})
