test_that("crm_opt4 returns data", {
  rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW)

  site_fhd_dat <- Johnston_Flight_heights_SOSS %>%
    dplyr::filter(variable=="Gannet.est") %>%
    dplyr::select(height,prop)

  site_fhd <- site_fhd_dat$prop

  surv_fhd_at_rotor <-
    get_fhd_rotor(
      hub_height = 150,
      fhd = site_fhd,
      rotor_radius = 120,
      tidal_offset = 2.5,
      yinc = 0.05)


  flux_fct <- get_flux_factor(
    n_turbines = 100,
    rotor_radius = 120,
    flight_speed = 13.1,
    bird_dens = c(1.19,0.85,1.05,1.45,1.41,1.45,1.12,1.45,0.93,0.902,1.06,1.23),
    daynight_hrs = Day_Length(52),
    noct_activity = 0.5
  )
  set.seed(1234)
  turb_oper <- data.frame(
    month = month.abb,
    prop_oper = runif(12,0.5,0.8)
  )
  turb_oper_month <- turb_oper$prop_oper

  crm_out <- crm_opt4(
    rotor_grids = rotor_grids,
    d_y_surv = surv_fhd_at_rotor,
    rotor_radius = 120,
    blade_width = 5,
    rotor_speed = 15,
    blade_pitch = 15,
    flight_type = "flapping",
    wing_span = 1.01,
    flight_speed = 13.1,
    body_lt = 0.85,
    n_blades = 3,
    prop_upwind = 0.5,
    avoidance_rate = 0.981,
    flux_factor = flux_fct,
    mth_prop_oper = turb_oper_month,
    lac_factor = 0.9998299
  )

  expect_equal(
    mean(crm_out),
    expected = 8.5556105
  )

})

