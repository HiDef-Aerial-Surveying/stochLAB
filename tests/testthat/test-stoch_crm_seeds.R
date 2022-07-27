
test_that("different seeds give marginal differences for stoch_crm",{
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

  stoch_output_111 <- stoch_crm(
    model_options = c(1, 2, 3),
    n_iter = 1000,
    flt_speed_pars = data.frame(mean = 7.26, sd = 1.5),
    body_lt_pars = data.frame(mean = 0.39, sd = 0.005),
    wing_span_pars = data.frame(mean = 1.08, sd = 0.04),
    avoid_bsc_pars = data.frame(mean = 0.99, sd = 0.001),
    avoid_ext_pars = data.frame(mean = 0.96, sd = 0.002),
    noct_act_pars = data.frame(mean = 0.033, sd = 0.005),
    prop_crh_pars = data.frame(mean = 0.06, sd = 0.009),
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
    windspd_pars = data.frame(mean = 7.74, sd = 3),
    rtn_pitch_windspd_dt = wind_rtn_ptch,
    trb_wind_avbl = windavb,
    trb_downtime_pars = dwntm,
    wf_n_trbs = 100,
    wf_width = 52,
    wf_latitude = 56.9,
    tidal_offset = 2.5,
    lrg_arr_corr = TRUE,
    verbose = TRUE,
    seed = 111,
    out_format = "draws",
    out_sampled_pars = TRUE,
    out_period = "months"
  )


  stoch_output_222 <- stoch_crm(
    model_options = c(1, 2, 3),
    n_iter = 1000,
    flt_speed_pars = data.frame(mean = 7.26, sd = 1.5),
    body_lt_pars = data.frame(mean = 0.39, sd = 0.005),
    wing_span_pars = data.frame(mean = 1.08, sd = 0.04),
    avoid_bsc_pars = data.frame(mean = 0.99, sd = 0.001),
    avoid_ext_pars = data.frame(mean = 0.96, sd = 0.002),
    noct_act_pars = data.frame(mean = 0.033, sd = 0.005),
    prop_crh_pars = data.frame(mean = 0.06, sd = 0.009),
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
    windspd_pars = data.frame(mean = 7.74, sd = 3),
    rtn_pitch_windspd_dt = wind_rtn_ptch,
    trb_wind_avbl = windavb,
    trb_downtime_pars = dwntm,
    wf_n_trbs = 100,
    wf_width = 52,
    wf_latitude = 56.9,
    tidal_offset = 2.5,
    lrg_arr_corr = TRUE,
    verbose = TRUE,
    seed = 222,
    out_format = "draws",
    out_sampled_pars = TRUE,
    out_period = "months"
  )

  ### Test if there is no statistical differences

  for(i in 1:12){
    pv <- wilcox.test(stoch_output_111$collisions$opt1[,i],stoch_output_222$collisions$opt1[,i])$p.value
    testthat::expect_gt(pv,expected = 0.05)
  }

})

