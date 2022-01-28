#' Number of collisions under model Option 4
#'
#' @description
#' Wrapper function to run the CRM calculations under option 4, i.e.:
#'  \itemize{
#'     \item Using the extended model, which takes into account the distribution
#'     of bird flight heights at risk height (above the minimum and below the
#'     maximum height of the rotor blade)
#'   \item Using site-specific flight height distribution of the species
#'   obtained from site survey data
#'  }
#'
#' @param d_y_surv a vector with the proportion of birds at height bands within
#'   the rotor disc, from a site-specific flight height distribution
#'
#' @inheritParams get_collisions_extended
#'
#' @return A numeric vector, the expected number of collisions per month based
#'    on model option 4
#'
#' @export
crm_opt4 <- function(rotor_grids,
                     d_y_surv,
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
    d_y = d_y_surv, # ... with site-specific FHD = Option 4!
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
