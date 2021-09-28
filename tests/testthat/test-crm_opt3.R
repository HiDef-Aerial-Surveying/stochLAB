test_that("crm_opt3 equates Band spreadsheet output (page 3)", {

  expect_equal(
    crm_opt3(
      gen_d_y = band_spreadsheet_dt$d_y,
      rotor_radius = band_spreadsheet_dt$rotor_radius,
      blade_width = band_spreadsheet_dt$blade_width,
      rotor_speed = band_spreadsheet_dt$rotor_speed,
      blade_pitch = band_spreadsheet_dt$blade_pitch,
      flight_type_num = ifelse(band_spreadsheet_dt$flight_type == "Flapping", 1, 0),
      wing_span = band_spreadsheet_dt$wing_span,
      flight_speed = band_spreadsheet_dt$flight_speed,
      body_lt = band_spreadsheet_dt$body_length,
      n_blades = band_spreadsheet_dt$n_blades,
      prop_upwind = band_spreadsheet_dt$prop_upwd,
      avoidance_rate_ext = band_spreadsheet_dt$avoid_rate_ext,
      flux_factor = band_spreadsheet_dt$flux_fct,
      prop_operational = band_spreadsheet_dt$prop_oper,
      lac_factor = band_spreadsheet_dt$lac_factor
    ),
    expected = band_spreadsheet_dt$collisions_opt3)
})
