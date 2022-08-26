test_that("band_crm() equates to Band spreadheet outputs in sheet 2", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  out_case1 <- band_crm(
    model_options = c('1', '2', '3'),
    flight_speed = band_spreadsheet_dt$flight_speed,
    body_lt = band_spreadsheet_dt$body_length,
    wing_span = band_spreadsheet_dt$wing_span,
    flight_type = band_spreadsheet_dt$flight_type,
    avoid_rt_basic = band_spreadsheet_dt$avoid_rate,
    avoid_rt_ext = band_spreadsheet_dt$avoid_rate_ext,
    noct_activity = band_spreadsheet_dt$noct_activity,
    prop_crh_surv = band_spreadsheet_dt$prop_crh_surv,
    dens_month = band_spreadsheet_dt$bird_dens_df,
    prop_upwind = band_spreadsheet_dt$prop_upwd,
    gen_fhd = band_spreadsheet_dt$fhd,
    rotor_speed = band_spreadsheet_dt$rotor_speed,
    rotor_radius = band_spreadsheet_dt$rotor_radius,
    blade_width = band_spreadsheet_dt$blade_width,
    blade_pitch = band_spreadsheet_dt$blade_pitch,
    n_blades = band_spreadsheet_dt$n_blades,
    hub_height = band_spreadsheet_dt$hub_ht,
    chord_prof = chord_prof_5MW,
    n_turbines = band_spreadsheet_dt$n_turbines,
    wf_width = band_spreadsheet_dt$wf_width,
    wf_latitude = band_spreadsheet_dt$wf_latitude,
    turb_oper_month = band_spreadsheet_dt$prop_oper_df,
    tidal_offset = band_spreadsheet_dt$tid_offset,
    lrg_arr_corr = band_spreadsheet_dt$lrg_arr_corr,
    xinc = 0.05,
    yinc = 0.05)

  expect_equal(out_case1$opt1, band_spreadsheet_dt$collisions_opt1)
  expect_equal(out_case1$opt2, band_spreadsheet_dt$collisions_opt2)
  expect_equal(out_case1$opt3, band_spreadsheet_dt$collisions_opt3,
               tolerance = 1e-7)
  expect_equal(dim(out_case1),c(12,4))
  expect_equal(names(out_case1),c("month","opt1","opt2","opt3"))


  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  out_case2 <- band_crm(
    model_options = c('1', '2', '3'),
    flight_speed = band_spreadsheet_dt_2$flight_speed,
    body_lt = band_spreadsheet_dt_2$body_length,
    wing_span = band_spreadsheet_dt_2$wing_span,
    flight_type = band_spreadsheet_dt_2$flight_type,
    avoid_rt_basic = band_spreadsheet_dt_2$avoid_rate,
    avoid_rt_ext = band_spreadsheet_dt_2$avoid_rate_ext,
    noct_activity = band_spreadsheet_dt_2$noct_activity,
    prop_crh_surv = band_spreadsheet_dt_2$prop_crh_surv,
    dens_month = band_spreadsheet_dt_2$bird_dens_df,
    prop_upwind = band_spreadsheet_dt_2$prop_upwd,
    gen_fhd = band_spreadsheet_dt_2$fhd,
    rotor_speed = band_spreadsheet_dt_2$rotor_speed,
    rotor_radius = band_spreadsheet_dt_2$rotor_radius,
    blade_width = band_spreadsheet_dt_2$blade_width,
    blade_pitch = band_spreadsheet_dt_2$blade_pitch,
    n_blades = band_spreadsheet_dt_2$n_blades,
    hub_height = band_spreadsheet_dt_2$hub_ht,
    chord_prof = chord_prof_5MW,
    n_turbines = band_spreadsheet_dt_2$n_turbines,
    wf_width = band_spreadsheet_dt_2$wf_width,
    wf_latitude = band_spreadsheet_dt_2$wf_latitude,
    turb_oper_month = band_spreadsheet_dt_2$prop_oper_df,
    tidal_offset = band_spreadsheet_dt_2$tid_offset,
    lrg_arr_corr = band_spreadsheet_dt_2$lrg_arr_corr,
    xinc = 0.05,
    yinc = 0.05)

  expect_equal(out_case2$opt1, band_spreadsheet_dt_2$collisions_opt1)
  expect_equal(out_case2$opt2, band_spreadsheet_dt_2$collisions_opt2)
  expect_equal(out_case2$opt3, band_spreadsheet_dt_2$collisions_opt3,
               tolerance = 1e-7)
})
