#' Number of collisions under model option 1
#'
#' @description
#' Wrapper function to run CRM calculations under option 1:
#'    \itemize{
#'       \item Basic model, i.e. flights across collision risk height are
#'        uniformly distributed.
#'       \item Proportion at collision risk height derived from site survey.
#'    }
#'
#' @param flux_factor a vector containing the flux factor for each month
#' @param prop_crh_surv The proportion of flights at collision risk height derived
#'    from site survey (\eqn{Q_2R}). Only required for model Option 1.
#' @inheritParams get_collisions_basic
#'
#' @seealso [get_flux_factor()] for calculating the flux factor
#'
#' @return A numeric vector, the expected number of collisions per month based
#'   on model option 1
#'
#' @examples
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
#' crm_opt1(
#'  flux_factor = flux_fct,
#'  prop_crh_surv = 0.13,
#'  avg_prob_coll = 0.1494609,
#'  mth_prop_oper = turb_oper_month,
#'  avoidance_rate = 0.989,
#'  lac_factor = 0.9998287)
#'
#' @export
crm_opt1 <- function(flux_factor,
                     prop_crh_surv,
                     avg_prob_coll,
                     mth_prop_oper,
                     avoidance_rate,
                     lac_factor) {

  # Potential number of bird flights transiting through the rotors of the wind
  # farm per month, assuming birds take no avoiding action ("Stage B" step in
  # Band's documentation)
  n_transits_opt1 <- flux_factor * prop_crh_surv

  # collisions under basic model
  get_collisions_basic(
    n_transits = n_transits_opt1,
    avg_prob_coll,
    mth_prop_oper,
    avoidance_rate,
    lac_factor
  )

}
