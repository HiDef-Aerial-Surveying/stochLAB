test_that("Migration collision risk model runs", {
  set.seed(1234)
  season_specs <- data.frame(
    season_id = c("PrBMigration", "PoBMigration","OMigration"),
    start_month = c("Mar", "May", "Oct"), end_month = c("Apr", "Sep", "Feb")
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
    sd = rep(2, 12))


  out <- mig_stoch_crm(
    # Wing span in m,
    wing_span_pars = data.frame(mean = 1.08, sd = 0.04),
    # Flight speed in m/s
    flt_speed_pars = data.frame(mean = 7.26, sd = 1.5),
    # Body length in m,
    body_lt_pars = data.frame(mean = 0.39, sd = 0.005),
    # Proportion of birds at CRH
    prop_crh_pars = data.frame(mean = 0.06, sd = 0.009),
    # avoidance rate
    avoid_bsc_pars = data.frame(mean = 0.99, sd = 0.001),
    n_turbines = 150,
    n_blades = 3,
    # rotation speed in m/s of turbine blades
    rtn_speed_pars = data.frame(mean = 13.1, sd = 4),
    # pitch in degrees of turbine blades
    bld_pitch_pars = data.frame(mean = 3, sd = 0.3),
    # sd = 0, rotor radius is fixed
    rtr_radius_pars = data.frame(mean = 80, sd = 0),
    # sd = 0, blade width is fixed
    bld_width_pars = data.frame(mean = 8, sd = 0),
    wf_width = 100,
    wf_latitude = 54.1,
    prop_upwind = 0.5,
    flight_type = "flapping",
    # population flying through windfarm,
    popn_estim_pars = data.frame(mean = 21584, sd = 2023),
    season_specs = season_specs,
    chord_profile = chord_prof_5MW,
    trb_wind_avbl = windavb,
    trb_downtime_pars = dwntm,
    n_iter = 1000,
    LargeArrayCorrection = TRUE,
    log_file = NULL,
    seed = 1234,
    verbose = TRUE)

  expect_equal(
    mean(unlist(out$collisions[,1])),
    expected = 0.224047655
  )
  ## Expect output to have 2 in the list
  expect_equal(
    length(out),2
  )
  ## Make sure table output has expected dimensions
  expect_equal(
    dim(out$collisions),
    c(1000,nrow(season_specs))
  )

  expect_equal(
    dim(out$flux_rates),
    c(1000,nrow(season_specs))
  )

  expect_equal(
    names(out$collisions),
    c("PrBMigration", "PoBMigration","OMigration")
  )

})
