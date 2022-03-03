#' Number of collisions under model option 2
#'
#' @description
#' Wrapper function to run CRM calculations under option 2, i.e.:
#' \itemize{
#'    \item Basic model, i.e. flights across collision risk height are uniformly
#'    distributed
#'    \item Proportion at collision risk height derived from a flight height
#' distribution (\eqn{Q'_2R})
#' }
#'
#' @param d_y a vector with the proportion of bird flights at height bands
#'   within the rotor disc
#' @param flux_factor a vector containing the flux factor for each month
#' @inheritParams get_collisions_basic
#'
#' @seealso `get_fhd_rotor()`, `get_flux_factor()`
#'
#' @return A numeric vector, the expected number of collisions per month based
#'   on model Option 2.
#'
#' @examples
#'
#'avg_collision_risk <-
#'  get_avg_prob_collision(
#'    flight_speed = 13.1,
#'    body_lt = 0.85,
#'    wing_span = 1.01,
#'    prop_upwind = 0.5,
#'    flap_glide = 1,
#'    rotor_speed = 15,
#'    rotor_radius = 120,
#'    blade_width = 5,
#'    blade_pitch = 15,
#'    n_blades = 3,
#'    chord_prof = chord_prof_5MW
#'  )
#'
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
#' crm_opt2(
#'     flux_factor = flux_fct,
#'     d_y = gen_fhd_at_rotor,
#'     avg_prob_coll = avg_collision_risk,
#'     mth_prop_oper = turb_oper_month,
#'     avoidance_rate = 0.989,
#'    lac_factor = 0.9998299)
#'
#' @export
crm_opt2 <- function(d_y,
                     flux_factor,
                     avg_prob_coll,
                     mth_prop_oper,
                     avoidance_rate,
                     lac_factor){

  # total proportion of bird flights at collision risk (integrate d_y over rotor height)
  prop_chr_fhd <- get_prop_crh_fhd(d_y)

  # Potential number of bird flights transiting through the rotors of the wind
  # farm per month, assuming birds take no avoiding action ("Stage B" step in
  # Band's documentation)
  n_transits_opt2 <- flux_factor * prop_chr_fhd

  # collisions under basic model
  get_collisions_basic(
    n_transits = n_transits_opt2,
    avg_prob_coll,
    mth_prop_oper,
    avoidance_rate,
    lac_factor
  )

}
