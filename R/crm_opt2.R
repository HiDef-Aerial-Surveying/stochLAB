#' Number of collisions under model option 2
#'
#' Top-level function to run CRM model under option 2:
#'    \itemize{
#'       \item Basic model, i.e. flights across collision risk height are
#'        uniformly distributed
#'       \item Proportion at collision risk height derived from a generic flight
#'       height distribution
#'    }
#'
#' @param gen_d_y a vector with the proportion of birds at height bands across #
#'    the rotor disc, calculated from a generic flight height distribution
#' @inheritParams get_rotor_transits
#' @inheritParams get_collisions_basic
#'
#' @seealso [get_fhd_rotor()], [get_flux_factor()]
#'
#' @return Expected number of collisions per month based on model option 2
crm_opt2 <- function(gen_d_y, flux_factor, prob_single_collision, prop_operational,
                     avoidance_rate, lac_factor){

  # total proportion of bird flights at collision risk (integrate d_y over rotor height)
  prop_chr_fhd <- get_prop_crh_fhd(gen_d_y)

  # transits based on prop of flights at collision risk height derived from site survey
  n_transits <- get_rotor_transits(flux_factor, prop_crh = prop_chr_fhd)

  # collisions under basic model
  get_collisions_basic(n_transits, prob_single_collision, prop_operational,
                       avoidance_rate, lac_factor)

}
