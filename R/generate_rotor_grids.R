#' Geometric attributes at equidistant points within the rotor's unit circle
#'
#' @description Generates a list of grids containing geometric attributes of the
#'   rotor disk at equidistant locations, taking the center of the rotor as
#'   the reference point and overlaying the left-half of the rotor disk. The
#'   size of grid cells are determined by `xinc` and `yinc`, and their
#'   values map properties of the rotor at the cell's location.
#'
#' @details These grids are required for an alternative approach to the
#'   calculation of probability of collision under the extended model (i.e.
#'   non-uniform flight distribution at risk height).
#'
#'   Functions `xrisksum2`, `pcollxy` and `pcoll` used in Masden
#'   & Cook implementation (`PCollFunctions.r` script) were based on Visual
#'   Basic computations available in the original Band worksheet. This Visual
#'   Basic code was devised under a deterministic context, i.e. for one single
#'   set of collision calculations for one given species and turbine scenario.
#'   However, in a stochastic context, where potentially thousands of collision
#'   calculations are performed per species and turbine scenario, it became
#'   clear that `xrisksum2` and associated functions were highly
#'   inefficient for the task at hand.
#'
#'   The alternative approach streamlines computations by calculating (relative)
#'   rotor geometric attributes outside the stochastic sampling loop, which
#'   remain constant over iterations. These elements, calculated via
#'   `generate_rotor_grids`, are then applied to sampled parameters
#'   via vectorized operations. The number of calculations per iteration are
#'   thence substantially reduced, leading to significant gains in computational
#'   speed - over 30x faster than Masden's implementation for a 1000 iterations
#'   run.
#'
#' @param yinc,xinc numeric values, the grid increments along the y-axis and
#'   x-axis (i.e. grid cell dimensions)
#' @inheritParams get_avg_prob_collision
#'
#' @return A list with the following elements, taking the center of the rotor as
#'   the origin in the rotor's plane:
#'   \itemize{
#'    \item{`x_grid`, a 2D array of horizontal distances from the rotor's
#'    horizontal axis, as proportion of rotor radius, at each grid point}
#'    \item{`y_grid`, a 2D array of vertical distances from the rotor's
#'    vertical axis, as proportion of rotor radius, at each grid point}
#'    \item{`r_grid`, a 2D array of radial distances from rotor center,
#'       as proportion of rotor radius, at each grid point}
#'    \item{`phi_grid`, a 2D array of angles, relative to vertical, at
#'    each grid point}
#'    \item{`chord_grid`, a 2D array of blade chord width at each grid
#'    point}
#'   }
#'   All elements are representative of the left-half of the rotor circle
#'
#' @seealso [get_x_grid()], [get_y_grid()],
#'   [get_phi_grid()]
#'
#' @examples
#' rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW)
#'
#' @export

generate_rotor_grids <- function(yinc = 0.05, xinc = 0.05, chord_prof) {

  # horizontal distances from vertical axis of rotor's circle
  x_grid <- get_x_grid(yinc, xinc)

  # vertical distances from horizontal axis of rotor's circle
  y_grid <- get_y_grid(x_grid, yinc)

  # angles (in radians) between grid cells and the rotor vertical axis
  phi_grid <- get_phi_grid(x_grid, y_grid)

  # generate grid with radial distances to rotor center, as a proportion of
  # rotor radius (i.e. the radius at each grid cell)
  r_grid <- sqrt(x_grid^2 + y_grid^2)

  # generate grid with the blade chord width at different points of the rotor
  # circle (i.e. chord width at each grid cell)
  chord_grid <- apply(r_grid, c(1, 2), function(r){

    # Assumes intervals of chord_prof radius are equidistant
    c_prof_inc <- round(chord_prof$pp_radius[2] - chord_prof$pp_radius[1], 3)

    # Find index of radius in chord profile that sits immediately above current
    # value r
    cell_val <- which(round(chord_prof$pp_radius, 3) == round(ceiling(r/c_prof_inc)*c_prof_inc, 3))

    # get chord-profile radius interval above and below current r
    upper <- chord_prof$pp_radius[cell_val]
    lower <- chord_prof$pp_radius[cell_val - 1]

    p <- (r - lower) / (upper-lower)

    # linearly interpolate the chord at current r
    chord_at_r <- chord_prof$chord[cell_val-1] +
      p * (chord_prof$chord[cell_val] - chord_prof$chord[cell_val-1])

    if(length(chord_at_r) == 0){
      return(NA)
    }else{
      return(chord_at_r)
    }
  })

  list(x_grid = x_grid,
       y_grid = y_grid,
       r_grid = r_grid,
       phi_grid = phi_grid,
       chord_grid =chord_grid)

}



#' Grid of horizontal distances from points in the rotor circle to its vertical
#' axis
#'
#' @description Taking the center of the rotor circle as the origin,
#'   `get_x_grid` generates a grid containing horizontal distances \eqn{x}
#'   (by `xinc` increments) from the y-axis up to the outer edge of the
#'   rotor circle, at equidistant height bands \eqn{y} (by `yinc`
#'   increments) between minimum and maximum rotor height.
#'
#'   Distances are expressed as proportion of rotor radius (i.e. \eqn{x} is
#'   dimensionless).
#'
#'   Returned grid represents the left-half of the rotor's circle.
#'
#' @param xinc,yinc numeric values, the grid increments along the y-axis and
#'   x-axis (i.e. grid cell dimensions)
#'
#' @return A 2D array giving a grid of horizontal distances \eqn{x} from rotor's
#'   vertical axis, expressed as the proportion of rotor radius (i.e. \eqn{[0,
#'   1]}), for the left-half of the rotor circle area.
#'
get_x_grid <- function(xinc = 0.05, yinc = 0.05){

  # horizontal grid size
  n_x_cells <- as.integer(1/xinc + 1)
  # vertical distances from minimum to maximum rotor height
  y_i <- seq(-1, 1, yinc)

  # initiate object for horizontal distances grid
  x_grid <- matrix(NA, ncol = n_x_cells, nrow = length(y_i))

  # fill up horizontal strips
  for(i in length(y_i):1){

    # maximum distance from y-axis at current height
    xmax <- (1- y_i[i] * y_i[i]) ^ 0.5

    if(xmax == 0){
      x_grid[i, 1] <- 0
    }else{
      if(xmax == 1){
        x_grid[i, ] <- seq(0, xmax, by = xinc)
      }else{
        x <- c(seq(0, floor(xmax/0.05)*0.05, by = xinc), xmax)
        x_grid[i, 1:length(x)] <- x
      }
    }
  }
  x_grid
}


#' Grid of vertical distances from points in the rotor circle to its horizontal
#' axis
#'
#' @description Taking the center of the rotor circle as the origin,
#'   `get_y_grid` generates a grid of vertical distances \eqn{y} (by
#'   `yinc` increments) from the x-axis to the outer edge of the rotor
#'   circle, across width intervals \eqn{x} (by `xinc` increments) between
#'   the center and maximum rotor width.
#'
#'   Returned grid represents the left-half of the rotor's circle.
#'
#'   Distances are expressed as proportion of rotor radius (i.e. \eqn{y} is
#'   dimensionless).
#'
#' @param x_grid A 2D array, with horizontal distances from the rotor's vertical axis,
#'   expressed as the proportion of rotor radius, for the left-half of the
#'   rotor circle area.
#' @param yinc a numeric value, the grid increment along the y-axis
#'
#' @return 2D array giving a grid of vertical distances \eqn{y} from the rotor's
#'   horizontal axis, expressed as the proportion of rotor radius, for
#'   the left-half of the rotor circle area. Negative values represent distances
#'   from the bottom half of the rotor circle.
#'
get_y_grid <- function(x_grid, yinc = 0.05){

  y <- seq(-1, 1, yinc)

  y_grid <- x_grid/x_grid * y
  y_grid[, 1] <- y

  y_grid
}


#' Grid with angles between points \eqn{(x, y)} and the rotor's vertical axis
#'
#' @description Given grids of vertical and horizontal distances from rotor axes
#'   at points \eqn{(x, y)} inside the rotor circle, `get_phi_grid`
#'   calculates the associated radial angles, relative to the rotor vertical axis.
#'
#'   Returned grid represents the left-half of the rotor's circle.
#'
#' @param x_grid A 2D array, with horizontal distances \eqn{x} from the rotor's
#'   vertical axis, expressed as the proportion of rotor radius, for the
#'   left-half of the rotor circle area.
#' @param y_grid A 2D array, with vertical distances (\eqn{y} from the rotor's
#'   horizontal axis, expressed as the proportion of rotor radius, for the
#'   left-half of the rotor circle area.
#'
#' @return A 2D array, giving a grid of angles (in radians) between points
#'   \eqn{(x, y)} inside the rotor circle and the rotor center, for the
#'   left-half of the rotor circle area.
#'
#' @seealso [get_x_grid()], [get_y_grid()]
#'
get_phi_grid <- function(x_grid, y_grid){

  phi_grid <- atan(x_grid/y_grid)

  # apply correction for angles on negative side of y-axis
  phi_crtn <- rep(0, dim(y_grid)[1])
  phi_crtn[which(y_grid[, 1] < 0)] <- pi
  phi_grid + phi_crtn
}






