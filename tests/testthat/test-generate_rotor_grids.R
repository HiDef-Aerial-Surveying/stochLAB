test_that("Test for rotor grids",{
  expect_equal(generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW),
               expected = rotor_grids_test)
})
