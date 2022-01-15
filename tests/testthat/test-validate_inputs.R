test_that("val_chord_prof finds invalid inputs in chord_prof", {

  # # No error raised if chord_prof meets expectations
  # expect_silent(
  #   val_chord_prof(chord_prof = chord_prof_5MW)
  #   )
  #
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
