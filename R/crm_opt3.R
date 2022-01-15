#' Number of collisions under model Option 3
#'
#' @description
#' Wrapper function to run CRM calculations under option 3, i.e.:
#'  \itemize{
#'     \item Using the extended model, which takes into account the distribution
#'     of bird flight heights at risk height (above the minimum and below the
#'     maximum height of the rotor blade)
#'     \item Using generic flight height distribution data
#'  }
#'
#' @param d_y_gen a vector with the proportion of birds at height bands within
#'   the rotor disc, from a generic flight height distribution
#' @inheritParams get_collisions_extended
#'
#' @return A numeric vector, the expected number of collisions per month based
#'    on model option 3
#'
#' @export
crm_opt3 <- function(rotor_grids,
                     d_y_gen,
                     rotor_radius,
                     blade_width,
                     rotor_speed,
                     blade_pitch,
                     flight_type,
                     wing_span,
                     flight_speed,
                     body_lt,
                     n_blades,
                     prop_upwind,
                     avoidance_rate,
                     flux_factor,
                     mth_prop_oper,
                     lac_factor){

  # number of collisions for the extended model...
  get_collisions_extended(
    rotor_grids,
    d_y = d_y_gen, # ... with generic FHD = Option 3!
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
    lac_factor
  )

}
