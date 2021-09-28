test_that("get_prob_collision equates Band page 3 spreadsheet output", {

  expect_equal(
    get_prob_collision(chord_prof = band_spreadsheet_dt$chord_profile,
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
    expected = band_spreadsheet_dt$prob_single_collision
  )
})
