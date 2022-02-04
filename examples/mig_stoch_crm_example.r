# ------------------------------------------------------
# Run with arbitrary parameter values, for illustration
# ------------------------------------------------------
season_specs <- data.frame(
  season_id = c("PrBMigration", "PoBMigration", "Omigration"),
  start_month = c("Mar", "May", "Oct"), end_month = c("Apr", "Sep", "Feb")
)

# wind availability
windavb <- data.frame(
  month = month.abb,
  pctg = runif(12, 85, 98)
)
head(windavb)

# maintenance downtime
dwntm <- data.frame(
  month = month.abb,
  mean = runif(12, 6, 10),
  sd = rep(2, 12))
head(dwntm)


mig_stoch_crm(
  wing_span_pars = data.frame(mean = 1.08, sd = 0.04),      # Wing span in m,
  flt_speed_pars = data.frame(mean = 7.26, sd = 1.5),       # Flight speed in m/s
  body_lt_pars = data.frame(mean = 0.39, sd = 0.005),       # Body length in m,
  prop_crh_pars = data.frame(mean = 0.06, sd = 0.009),      # Proportion of birds at CRH
  avoid_bsc_pars = data.frame(mean = 0.99, sd = 0.001),     # avoidance rate
  n_turbines = 150,
  n_blades = 3,
  rtn_speed_pars = data.frame(mean = 13.1, sd = 4),         # rotation speed in m/s of turbine blades
  bld_pitch_pars = data.frame(mean = 3, sd = 0.3),          # pitch in degrees of turbine blades
  rtr_radius_pars = data.frame(mean = 80, sd = 0),          # sd = 0, rotor radius is fixed
  bld_width_pars = data.frame(mean = 8, sd = 0),            # sd = 0, blade width is fixed
  wf_width = 100,
  wf_latitude = 54.1,
  prop_upwind = 0.5,
  flight_type = "flapping",
  popn_estim_pars = data.frame(mean = 21584, sd = 2023),    # population flying through windfarm,
  season_specs = season_specs,
  chord_profile = chord_prof_5MW,
  trb_wind_avbl = windavb,
  trb_downtime_pars = dwntm,
  n_iter = 1000,
  LargeArrayCorrection = TRUE,
  log_file = NULL,
  seed = 1234,
  verbose = TRUE)
