test_that("get_avg_prob_collision equates Band page 3 spreadsheet output", {

  # case 1 - Gannet, 96% avoidance, specific wind farm features
  expect_equal(
    get_avg_prob_collision(
      chord_prof = band_spreadsheet_dt$chord_profile,
      flight_speed = band_spreadsheet_dt$flight_speed,
      body_lt = band_spreadsheet_dt$body_length,
      wing_span = band_spreadsheet_dt$wing_span,
      prop_upwind = band_spreadsheet_dt$prop_upwd,
      flap_glide = band_spreadsheet_dt$flap_glide_fact,
      rotor_speed = band_spreadsheet_dt$rotor_speed,
      rotor_radius = band_spreadsheet_dt$rotor_radius,
      blade_width = band_spreadsheet_dt$blade_width,
      blade_pitch = band_spreadsheet_dt$blade_pitch,
      n_blades = band_spreadsheet_dt$n_blades),
    expected = band_spreadsheet_dt$avg_prob_coll
  )

  # case 2 - Kittiwake, 98% avoidance, specific wind farm features
  expect_equal(
    get_avg_prob_collision(
      chord_prof = band_spreadsheet_dt_2$chord_profile,
      flight_speed = band_spreadsheet_dt_2$flight_speed,
      body_lt = band_spreadsheet_dt_2$body_length,
      wing_span = band_spreadsheet_dt_2$wing_span,
      prop_upwind = band_spreadsheet_dt_2$prop_upwd,
      flap_glide = band_spreadsheet_dt_2$flap_glide_fact,
      rotor_speed = band_spreadsheet_dt_2$rotor_speed,
      rotor_radius = band_spreadsheet_dt_2$rotor_radius,
      blade_width = band_spreadsheet_dt_2$blade_width,
      blade_pitch = band_spreadsheet_dt_2$blade_pitch,
      n_blades = band_spreadsheet_dt_2$n_blades),
    expected = band_spreadsheet_dt_2$avg_prob_coll
  )

})
