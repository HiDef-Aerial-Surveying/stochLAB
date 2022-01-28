test_that("get_collisions_extended equates Band spreadsheet output (page 2 - option 3)", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    get_collisions_extended(
      rotor_grids = generate_rotor_grids(chord_prof = chord_prof_5MW),
      d_y = band_spreadsheet_dt$d_y,
      rotor_radius = band_spreadsheet_dt$rotor_radius,
      blade_width = band_spreadsheet_dt$blade_width,
      rotor_speed = band_spreadsheet_dt$rotor_speed,
      blade_pitch = band_spreadsheet_dt$blade_pitch,
      flight_type = band_spreadsheet_dt$flight_type,
      wing_span = band_spreadsheet_dt$wing_span,
      flight_speed = band_spreadsheet_dt$flight_speed,
      body_lt = band_spreadsheet_dt$body_length,
      n_blades = band_spreadsheet_dt$n_blades,
      prop_upwind = band_spreadsheet_dt$prop_upwd,
      avoidance_rate = band_spreadsheet_dt$avoid_rate_ext,
      flux_factor = band_spreadsheet_dt$flux_fct,
      mth_prop_oper = band_spreadsheet_dt$prop_oper,
      lac_factor = band_spreadsheet_dt$lac_factor
    ),
    expected = band_spreadsheet_dt$collisions_opt3,
    tolerance = 1e-7)

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    get_collisions_extended(
      rotor_grids = generate_rotor_grids(chord_prof = chord_prof_5MW),
      d_y = band_spreadsheet_dt_2$d_y,
      rotor_radius = band_spreadsheet_dt_2$rotor_radius,
      blade_width = band_spreadsheet_dt_2$blade_width,
      rotor_speed = band_spreadsheet_dt_2$rotor_speed,
      blade_pitch = band_spreadsheet_dt_2$blade_pitch,
      flight_type = band_spreadsheet_dt_2$flight_type,
      wing_span = band_spreadsheet_dt_2$wing_span,
      flight_speed = band_spreadsheet_dt_2$flight_speed,
      body_lt = band_spreadsheet_dt_2$body_length,
      n_blades = band_spreadsheet_dt_2$n_blades,
      prop_upwind = band_spreadsheet_dt_2$prop_upwd,
      avoidance_rate = band_spreadsheet_dt_2$avoid_rate_ext,
      flux_factor = band_spreadsheet_dt_2$flux_fct,
      mth_prop_oper = band_spreadsheet_dt_2$prop_oper,
      lac_factor = band_spreadsheet_dt_2$lac_factor
    ),
    expected = band_spreadsheet_dt_2$collisions_opt3,
    tolerance = 1e-7)
})
