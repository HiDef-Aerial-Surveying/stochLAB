test_that("validate inputs returns nothing", {

  # # No error raised if chord_prof meets expectations
  expect_silent(
    validate_inputs(model_options=c(1),
                    avoid_bsc_pars=data.frame(mean=0.99,sd=0.001),
                    prop_crh_pars=data.frame(mean=0.01,sd=0.01),
                    air_gap_pars = data.frame(mean=21,sd=0),
                    rtr_radius_pars = data.frame(mean=100,sd=0),
                    bld_pitch_pars = data.frame(mean=15,sd=0),
                    rtn_pitch_opt = "probDist",
                    rtn_speed_pars = data.frame(mean=14,sd=5),
                    out_period = "months",
                    lrg_arr_corr = TRUE,
                    fn="scrm")
    )


  # # missing columns or wrong column names  -------------------------------------
  # inval_chord <- chord_prof_5MW
  # names(inval_chord) <- c("pp_radiu", "chord")
  #
  # expect_error(
  #   val_chord_prof(chord_prof = inval_chord),
  #   "Invalid 'chord_prof' argument: missing column\\(s\\)"
  # )
  #
  # # non-constant increments on radius columns ----------------------------------
  # inval_chord <- chord_prof_5MW
  # inval_chord$pp_radius <- c(0.02, seq(0,1,0.05)[-1])
  #
  # expect_error(
  #   val_chord_prof(chord_prof = inval_chord),
  #   "Invalid 'chord_prof' argument: all values in"
  # )
  #
  # inval_chord <- chord_prof_5MW[-1, ]
  # expect_error(
  #   val_chord_prof(chord_prof = inval_chord),
  #   "Invalid 'chord_prof' argument: values in"
  # )
  #
  # inval_chord <- chord_prof_5MW[-21, ]
  # expect_error(
  #   val_chord_prof(chord_prof = inval_chord),
  #   "Invalid 'chord_prof' argument: values in"
  # )

})
