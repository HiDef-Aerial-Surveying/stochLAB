test_that("x and y grids are returned correctly",{
  x_grid <- get_x_grid(xinc=0.05,yinc=0.05)
  expect_equal(
    mean(x_grid[15:27,21]),
    expected=0.98221594
  )

  y_grid <- get_y_grid(x_grid,yinc=0.05)

  expect_equal(
    mean(y_grid[15:27,21]),
    expected = 5.978541e-17
  )

  phi_grid <- get_phi_grid(x_grid,y_grid)
  expect_equal(
    mean(phi_grid[15:27,21]),
    expected=1.57079633
  )

})
