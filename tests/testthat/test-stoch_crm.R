test_that("stoch_crm without stochasticity is same as band_crm",{
  set.seed(999)
  b_dens <- data.frame(
    month = month.abb,
    mean = runif(12, 0.8, 1.5),
    sd = runif(12, 0, 0)
  )

  # Generic FHD bootstraps from Johnson et al (2014)
  fhd_boots <- generic_fhd_bootstraps[[1]][,1:2]

  # wind speed vs rotation speed vs blade pitch
  wind_rtn_ptch <- data.frame(
    wind_speed = seq_len(30),
    rtn_speed = 10/(30:1),
    bld_pitch = c(rep(90, 4), rep(0, 8), 5:22)
  )

  # wind availability
  windavb <- data.frame(
    month = month.abb,
    pctg = runif(12, 85, 98)
  )

  # maintenance downtime
  dwntm <- data.frame(
    month = month.abb,
    mean = runif(12, 6, 10),
    sd = rep(0, 12))

  # ----------------------------------------------------------
  # Run stochastic CRM, treating rotor radius, air gap and
  # blade width as fixed parameters (i.e. not stochastic)
  stoch_output <- stoch_crm(
    model_options = c(1, 2, 3),
    n_iter = 1000,
    flt_speed_pars = data.frame(mean = 13.1, sd = 0),
    body_lt_pars = data.frame(mean = 0.85, sd = 0),
    wing_span_pars = data.frame(mean = 1.01, sd = 0),
    avoid_bsc_pars = data.frame(mean = 0, sd = 0),
    avoid_ext_pars = data.frame(mean = 0, sd = 0),
    noct_act_pars = data.frame(mean = 0.5, sd = 0),
    prop_crh_pars = data.frame(mean = 0.01, sd = 0),
    bird_dens_opt = "tnorm",
    bird_dens_dt = b_dens,
    flight_type = "flapping",
    prop_upwind = 0.5,
    gen_fhd_boots = fhd_boots,
    n_blades = 3,
    bld_pitch_pars = data.frame(mean = 15, sd = 0),
    rtn_speed_pars = data.frame(mean = 15, sd = 0),
    rtr_radius_pars = data.frame(mean = 120, sd = 0), # sd = 0, rotor radius is fixed
    air_gap_pars = data.frame(mean = 30, sd = 0),    # sd = 0, air gap is fixed
    bld_width_pars = data.frame(mean = 5, sd = 0),   # sd = 0, blade width is fixed
    rtn_pitch_opt = "probDist",
    windspd_pars = data.frame(mean = 7.74, sd = 0),
    rtn_pitch_windspd_dt = wind_rtn_ptch,
    trb_wind_avbl = windavb,
    trb_downtime_pars = dwntm,
    wf_n_trbs = 100,
    wf_width = 52,
    wf_latitude = 56.9,
    tidal_offset = 2.5,
    lrg_arr_corr = TRUE,
    verbose = TRUE,
    seed = 1234,
    out_format = "summaries",
    out_sampled_pars = TRUE,
    out_period = "months"
  )


  # Run Band model ----------------------------------------------------------


  bd_dens <- data.frame(
    month = month.abb,
    dens = b_dens$mean
  )

  gen_fhd <- fhd_boots[,c(1:2)]
  names(gen_fhd)[2] <- "prop"

  prop_oper <- data.frame(
    month = month.abb,
    prop_oper = stoch_output$sampled_pars$prop_oper_mth$mean)


  band_output <- band_crm(
    model_options = c(1,2,3),
    flight_speed = stoch_output$sampled_pars$flt_speed$mean,
    body_lt = stoch_output$sampled_pars$body_lt$mean,
    wing_span = stoch_output$sampled_pars$wing_span$mean,
    flight_type = "flapping",
    avoid_rt_basic = 0,
    avoid_rt_ext = 0,
    noct_activity = stoch_output$sampled_pars$noct_actv$mean,
    prop_crh_surv = stoch_output$sampled_pars$prop_crh$mean,
    dens_month = bd_dens,
    prop_upwind = 0.5,
    gen_fhd = gen_fhd,
    rotor_speed = stoch_output$sampled_pars$rtn_speed$mean,
    rotor_radius = stoch_output$sampled_pars$rtr_radius$mean,
    blade_width = stoch_output$sampled_pars$bld_width$mean,
    blade_pitch = stoch_output$sampled_pars$bld_pitch$mean,
    n_blades = 3,
    hub_height = 150,
    chord_prof = chord_prof_5MW,
    n_turbines = 100,
    turb_oper_month = prop_oper,
    wf_width = 52,
    wf_latitude = 56.9,
    tidal_offset = 2.5,
    lrg_arr_corr = TRUE,
    xinc=0.05,
    yinc=0.05
  )

  expect_equal(as.numeric(stoch_output$collisions$opt1$mean),expected=band_output$opt1)
  expect_equal(as.numeric(stoch_output$collisions$opt2$mean),expected=band_output$opt2)
  expect_equal(as.numeric(stoch_output$collisions$opt3$mean),expected=band_output$opt3)

})



