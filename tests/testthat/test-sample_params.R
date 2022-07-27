test_that("Param draws return expected", {
  set.seed(1234)
  bird_dens_dt <- data.frame(
     month = month.abb,
     mean = runif(12, 0.8, 1.5),
     sd = runif(12, 0.2, 0.3)
     )

  trb_wind_avbl <- data.frame(
     month = month.abb,
     pctg = runif(12, 85, 98)
     )

  trb_downtime_pars <- data.frame(
     month = month.abb,
     mean = runif(12, 6, 10),
     sd = rep(2, 12))

  wind_rtn_ptch <- data.frame(
     wind_speed = seq_len(30),
     rtn_speed = 10/(30:1),
     bld_pitch = c(rep(90, 4), rep(0, 8), 5:22)
     )

  bird_dens_opt <- "tnorm"

  b_dens_mth <- switch (bird_dens_opt,
                        tnorm = bird_dens_dt$month,
                        resample = names(bird_dens_dt),
                        qtiles = names(bird_dens_dt)[names(bird_dens_dt) != "p"]
     ) %>% format_months()

  dwntm_mth <- format_months(trb_downtime_pars$month)
  windav_mth <- format_months(trb_wind_avbl$month)

  mod_mths <- Reduce(intersect, list(b_dens_mth, dwntm_mth, windav_mth))

  mod_mths <- mod_mths[order(match(mod_mths, month.abb))]

  param_draws <- sample_parameters(
       model_options = c(1,2,3),
       n_iter = 10,
       mod_mths = mod_mths,
       flt_speed_pars = data.frame(mean=7.26,sd=1.5),
       body_lt_pars = data.frame(mean=0.39,sd=0.005),
       wing_span_pars = data.frame(mean=1.08,sd=0.04),
       avoid_bsc_pars = data.frame(mean=0.99,sd=0.001),
       avoid_ext_pars = data.frame(mean=0.96,sd=0.002),
       noct_act_pars = data.frame(mean=0.033,sd=0.005),
       prop_crh_pars = data.frame(mean=0.06,sd=0.009),
       bird_dens_opt = "tnorm",
       bird_dens_dt = bird_dens_dt,
       gen_fhd_boots = generic_fhd_bootstraps[[1]],
       site_fhd_boots = NULL,
       rtr_radius_pars = data.frame(mean=80,sd=0),
       air_gap_pars = data.frame(mean=36,sd=0),
       bld_width_pars = data.frame(mean=8,sd=0),
       rtn_pitch_opt = "windSpeedReltn",
       bld_pitch_pars = NULL,
       rtn_speed_pars = NULL,
       windspd_pars = data.frame(mean=7.74,sd=3),
       rtn_pitch_windspd_dt = wind_rtn_ptch,
       trb_wind_avbl = trb_wind_avbl,
       trb_downtime_pars = trb_downtime_pars,
       lrg_arr_corr = TRUE
       )

  expect_equal(
    length(param_draws),
    expected = 18
  )

  expect_equal(
    mean(param_draws$body_lt),
    expected = 0.38793705
  )



})
