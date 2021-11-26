#' Number of collisions under model option 1
#'
#' Top-level function to run CRM model under option 1:
#'    \itemize{
#'       \item Basic model, i.e. flights across collision risk height are
#'        uniformly distributed
#'       \item proportion at collision risk height derived from site survey
#'    }
#'
#' @inheritParams get_collisions_basic
#' @param prop_crh_surv The proportion of flights at collision risk height derived
#'    from site survey (\eqn{Q_2R})
#' @inheritParams get_rotor_transits
#'
#' @return Expected number of collisions per month based on model option 1
#' @export
crm_opt1 <- function(flux_factor, prop_crh_surv, prob_single_collision,
                     prop_operational, avoidance_rate, lac_factor){

  # transits based on prop of flights at collision risk height derived from site survey
  n_transits <-
    get_rotor_transits(
      flux_factor,
      prop_crh = prop_crh_surv
    )

  # collisions under basic model
  get_collisions_basic(n_transits, prob_single_collision,
                       prop_operational, avoidance_rate, lac_factor)

}
