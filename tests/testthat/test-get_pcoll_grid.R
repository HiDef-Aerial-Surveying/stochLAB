test_that("Rotor grids being generated correctly",{
  rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW)

  out <- get_pcoll_grid(
    rotor_grids = rotor_grids,
    direction = 1,
    rotor_radius = 120,
    blade_width = 5,
    rotor_speed = 15,
    blade_pitch = 15,
    flight_type = "flapping",
    n_blades = 3,
    flight_speed = 13.1,
    wing_span = 1.01,
    body_lt = 0.85)

  expect_equal(
    out[1],
    expected = 0.048664122
  )

  expect_equal(
    out[10],
    expected = 0.159734514
  )

})


