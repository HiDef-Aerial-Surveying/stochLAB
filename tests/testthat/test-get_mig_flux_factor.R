test_that("Migration flux factor calculation works",{
  expect_equal(
    get_mig_flux_factor(
        n_turbines = 100,
        rotor_radius = 120,
        wf_width = 51,
        popn_est = 10000
    ),
    expected = 3695.9914
  )
})
