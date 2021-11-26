#' Number of collisions under model option 3
#'
#' Top-level function to run CRM model under option 3:
#'    \itemize{
#'       \item Extended model, i.e. takes into account the distribution of bird
#'       flight heights at risk height (above the minimum and below the maximum
#'       height of the rotor blade)
#'       \item Uses a generic flight height distribution
#'    }
#'
#' @param gen_d_y a vector with the proportion of birds at height bands across #
#'    the rotor disc, calculated from a generic flight height distribution
#' @inheritParams get_rotor_transits
#' @inheritParams get_collisions_basic
#'
#' @seealso [get_fhd_rotor()], [get_flux_factor()]
#'
#' @note The original Masden code included the calculation of the "Flux Integral" and
#'    "Average collision risk for single rotor transit". These quantities are not
#'    required for the calculation of collisions under option 3, nor are used
#'    anywhere else in the remaining code. Therefore, those calculations were
#'    dropped from the current implementation.
#'
#' @return Expected number of collisions per month based on model option 2
crm_opt3 <- function(gen_d_y, rotor_radius, blade_width, rotor_speed, blade_pitch,
                     flight_type_num, wing_span, flight_speed, body_lt,
                     n_blades, prop_upwind, avoidance_rate_ext,
                     flux_factor, prop_operational, lac_factor){

  # list of parameter values for convenience in passing variables down the chain
  # of lower level functions
  pars <- list(rotor_radius = rotor_radius,
               blade_width = blade_width,
               rotor_speed = rotor_speed,
               blade_pitch = blade_pitch,
               flight_type_num = flight_type_num,
               wing_span = wing_span,
               flight_speed = flight_speed,
               body_lt = body_lt,
               n_blades = n_blades)


  # vector of distances from rotor center to vertical equidistant points between minimum
  # and maximum rotor heights, expressed as a proportion of rotor radius.
  # -1: bottom blade tip; 1: top blade tip; 0: rotor center
  y <- round(seq(-1,1,0.05),2)


  ## Up wind risk
  CollMinUP <- gen_d_y[1] * xrisksum2(y[1], 0.05, 1, pars) / 2 ### risk at lowest point on rotor blade
  CollIntUP <- CollMinUP + gen_d_y[41] * xrisksum2(y[41], 0.05, 1, pars) / 2 ### risk at highest point on rotor blade

  for (v in 2:40) {

    CollIntUP <- CollIntUP + gen_d_y[v] * xrisksum2(y[v], 0.05, 1, pars)  #### Fill in intermediate heights

  }


  CollIntUP = CollIntUP * 0.05 * (2/pi)

  ## Down wind risk -------
  CollMinDown <- gen_d_y[1] * xrisksum2(y[1], 0.05, -1, pars) / 2 ### risk at lowest point on rotor blade
  CollIntDown <- CollMinDown + gen_d_y[41] * xrisksum2(y[41], 0.05, -1, pars) / 2 ### risk at highest point on rotor blade

  for (w in 2:40) {

    CollIntDown = CollIntDown + gen_d_y[w] * xrisksum2(y[w], 0.05, -1, pars)  #### Fill in intermediate heights

  }

  CollIntDown <- CollIntDown * 0.05 * (2/pi)

  ## Average Collision Integral

  CollInt <- (prop_upwind * CollIntUP) + ((1-prop_upwind) * CollIntDown)

  flux_factor * CollInt * prop_operational * (1 - avoidance_rate_ext) * lac_factor
}
