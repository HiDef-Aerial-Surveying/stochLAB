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
#' @export
#'
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
#'       daynight_hrs = DayLength(52),
#'       noct_activity = 0.5
#'       )
#'
#' turb_oper <- data.frame(
#'    month = month.abb,
#'    prop_oper = runif(12,0.5,0.8)
#'    )
#' turb_oper_month <- turb_oper$prop_oper
#'
#' crm_opt3(
#'   rotor_grids = rotor_grids,
#'   d_y_gen = gen_fhd_at_rotor,
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
