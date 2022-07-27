#' Number of collisions based on the extended model
#'
#' Calculates the expected number of collisions under the extended model, i.e.
#' taking into account the distribution of bird flight heights at risk height
#' (above the minimum and below the  maximum height of the rotor blade)
#'
#' @param d_y a vector with the proportion of birds at height bands within the
#'  rotor disc
#' @inheritParams get_pcoll_grid
#' @inheritParams get_collisions_basic
#' @inheritParams get_avg_prob_collision
#' @inheritParams crm_opt1
#' @param avoidance_rate a numeric value within the interval \eqn{[0, 1]}. The
#'   avoidance rate, expressing the probability that a bird flying on a
#'   collision course with a turbine will take evading action to avoid
#'   collision.
#'
#' @note
#' The original Masden code for extendend model (Option 3) included the
#' calculation of the "Flux Integral" and "Average collision risk for single
#' rotor transit". These quantities are not required for the calculation of
#' collisions under the extended model, nor are used anywhere else in the package.
#' Therefore, those calculations were dropped from the current implementation to
#' keep computations to a minimum required.
#'
#' @seealso [generate_rotor_grids()]
#'
#' @return Numeric vector with the expected number of collisions per month based
#'   on the extended model
#' @examples
#'
#'  rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW)
#'
#'  gen_fhd_dat <- Johnston_Flight_heights_SOSS %>%
#'       dplyr::filter(variable=="Gannet.est") %>%
#'       dplyr::select(height,prop)
#'
#'  gen_fhd <- gen_fhd_dat$prop
#'
#'  gen_fhd_at_rotor <-
#'     get_fhd_rotor(
#'       hub_height = 150,
#'       fhd = gen_fhd,
#'       rotor_radius = 120,
#'       tidal_offset = 2.5,
#'       yinc = 0.05)
#'
#'
#'  flux_fct <- get_flux_factor(
#'       n_turbines = 100,
#'       rotor_radius = 120,
#'       flight_speed = 13.1,
#'       bird_dens = c(1.19,0.85,1.05,1.45,1.41,1.45,1.12,1.45,0.93,0.902,1.06,1.23),
#'       daynight_hrs = Day_Length(52),
#'       noct_activity = 0.5
#'       )
#'
#' turb_oper <- data.frame(
#'    month = month.abb,
#'    prop_oper = runif(12,0.5,0.8)
#'    )
#' turb_oper_month <- turb_oper$prop_oper
#'
#' get_collisions_extended(
#'   rotor_grids = rotor_grids,
#'   d_y = gen_fhd_at_rotor, # ... with generic FHD = Option 3, with site FHD = Option 4
#'   rotor_radius = 120,
#'   blade_width = 5,
#'   rotor_speed = 15,
#'   blade_pitch = 15,
#'   flight_type = "flapping",
#'   wing_span = 1.01,
#'   flight_speed = 13.1,
#'   body_lt = 0.85,
#'   n_blades = 3,
#'   prop_upwind = 0.5,
#'   avoidance_rate = 0.981,
#'   flux_factor = flux_fct,
#'   mth_prop_oper = turb_oper_month,
#'   lac_factor = 0.9998299
#'   )
#'
#' @export
get_collisions_extended <- function(rotor_grids,
                                    d_y,
                                    rotor_radius,
                                    blade_width,
                                    rotor_speed,
                                    blade_pitch,
                                    flight_type,
                                    wing_span,
                                    flight_speed,
                                    body_lt, n_blades,
                                    prop_upwind,
                                    avoidance_rate,
                                    flux_factor,
                                    mth_prop_oper,
                                    lac_factor){

  # number of height bands
  y_lt <- dim(rotor_grids$r_grid)[1]

  # input validation
  if(y_lt != length(d_y)){
    stop("Arguments with inconsistent object sizes: nr of rows of dataset elements
         in list 'rotor_grids' must match lenght of vector 'd_y'")
  }

  ## Upwind collision risk  -------------------------------------------------------------
  pcollxy_grid_up <-
    get_pcoll_grid(
      rotor_grids = rotor_grids,
      direction = 1,
      rotor_radius = rotor_radius,
      blade_width = blade_width,
      rotor_speed = rotor_speed,
      blade_pitch = blade_pitch,
      flight_type = flight_type,
      n_blades = n_blades,
      flight_speed = flight_speed,
      wing_span = wing_span,
      body_lt = body_lt)

  risk_up <- rep(NA, y_lt)
  for(i in 1:y_lt){
    risk_up[i] <- get_risk_y(rotor_grids$x_grid[i, ], pcollxy_grid_up[i, ])
  }

  dy_riskup <- d_y * risk_up

  # (trapezoidal) integration across height bands
  CollIntUP <- sum(dy_riskup[1]/2, dy_riskup[y_lt]/2, dy_riskup[2:(y_lt - 1)]) * 0.05 * (2/pi)

  ## Downwind collision risk ------------------------------------------------------------
  pcollxy_grid_down <-
    get_pcoll_grid(
      rotor_grids = rotor_grids,
      direction = -1,
      rotor_radius = rotor_radius,
      blade_width = blade_width,
      rotor_speed = rotor_speed,
      blade_pitch = blade_pitch,
      flight_type = flight_type,
      n_blades = n_blades,
      flight_speed = flight_speed,
      wing_span = wing_span,
      body_lt = body_lt)

  risk_down <- rep(NA, y_lt)
  for(i in 1:y_lt){
    risk_down[i] <- get_risk_y(rotor_grids$x_grid[i, ], pcollxy_grid_down[i,])
  }

  dy_riskdown <- d_y * risk_down

  # (trapezoidal) integration across height bands
  CollIntDown <- sum(dy_riskdown[1]/2, dy_riskdown[y_lt]/2, dy_riskdown[2:(y_lt - 1)]) * 0.05 * (2/pi)

  ## Collision Integral --------------------------------------------------------
  CollInt <- (prop_upwind * CollIntUP) + ((1-prop_upwind) * CollIntDown)

  ## Expected number of collisions per time period (month)   -------------------
  flux_factor * CollInt * mth_prop_oper * (1 - avoidance_rate) * lac_factor

}
