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
