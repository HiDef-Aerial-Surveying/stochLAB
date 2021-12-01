#' Large array correction factor
#'
#' A correction factor for large windfarms with a large array of turbines, to take
#' into account the depletion of bird density in later rows of the site.
#'
#' @param n_turbines The number of turbines on the wind farm (\eqn{T}).
#' @param rotor_radius The radius of the rotor (\eqn{R}), in metres
#' @param avoidance_rate The avoidance rate assumed (\eqn{A})
#' @param prob_single_collision The average probability of collision for a single bird
#'   transit through a rotor, assuming no avoidance action.
#' @param mean_prop_operational the mean proportion of time turbines are
#'   operational through the year (decimal)
#' @param wf_width The approximate longitudinal width of the wind farm,
#'   in kilometers (\eqn{w})
#'
#' @return The large array correction factor to be applied
#' @export
get_lac_factor <- function(n_turbines,
                           rotor_radius,
                           avoidance_rate,
                           prob_single_collision,
                           mean_prop_operational,
                           wf_width){

  NTurbRows = n_turbines^ 0.5

  if(NTurbRows == 0){stop("Error: There are no turbines in your WFArea, please check your input data")}


  # the collision risk for a single bird due to any one turbine (i.e. disregarding risks by other turbines)
  CollRiskSinglePassage = n_turbines * (pi * rotor_radius^2)/(2 * rotor_radius * wf_width * 1000) *
    prob_single_collision * mean_prop_operational * (1-avoidance_rate)

  L_ArrayCF = 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage +
    (NTurbRows - 1) * (NTurbRows - 2)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)


  # BC --> OK!! Found an error in Masden's code!!! The 2 terms after the '+' sign should
  # be (n-1)*(n-2), *NOT* (n-1)*(2n). Causes a neglegible difference (order of 1e-6),
  # but enough to fail the consistency test with Band's spreadsheet
  # L_ArrayCF = 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage +
  #   (NTurbRows - 1) * (2*NTurbRows)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)

  return(L_ArrayCF)
}


