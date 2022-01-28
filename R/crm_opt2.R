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
