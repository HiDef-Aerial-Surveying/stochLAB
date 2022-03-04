test_that("turbine sampler for mCRM returns results",{
  set.seed(1234)
  season_specs <- data.frame(
     season_id = c("PrBMigration", "PoBMigration","OMigration"),
     start_month = c("Mar", "May", "Oct"), end_month = c("Apr", "Sep", "Feb")
     )

  windavb <- data.frame(
     month = month.abb,
     pctg = runif(12, 85, 98)
     )

  dwntm <- data.frame(
     month = month.abb,
     mean = runif(12, 6, 10),
     sd = rep(2, 12))

  out <- sample_turbine_mCRM(rtn_speed_pars = data.frame(mean = 13.1, sd = 4),
                         bld_pitch_pars = data.frame(mean = 3, sd = 0.3),
                         rtr_radius_pars = data.frame(mean = 80, sd = 0),
                         bld_width_pars = data.frame(mean = 8, sd = 0),
                         season_specs = season_specs,
                         n_iter = 10,
                         trb_wind_avbl = windavb,
                         trb_downtime_pars = dwntm)

  expect_equal(
    ncol(out),
    expected = 10
  )

  expect_equal(
    mean(out$RotorSpeed),
    expected = 13.0749098
  )




})
