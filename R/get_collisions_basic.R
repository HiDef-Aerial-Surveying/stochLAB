#' Get expected collisions based on the basic model
#'
#' Provides the number of collisions expected to occur by month. The basic model
#' assumes a uniform distribution of bird flights at risk height (i.e. between
#' min and max rotor height)
#'
#' @param n_transits Vector with the estimated number of bird flights crossing
#' the rotors of the wind farm at each time period
#' @param prob_single_collision The probability of collision for a single bird
#' transit through a rotor, assuming no avoidance action.
#' @param prop_operational The proportion of time during which turbines are
#' operational per month
#' @param avoidance_rate The proportion of birds likely to take action to avoid
#' collision with rotor
#' @param lac_factor The large array correction factor. Defaults to 1,
#' meaning large array correction is not applicable
#'
#' @return A numeric vector. The expected number of collisions at each time period
get_collisions_basic <- function(n_transits,
                                 prob_single_collision,
                                 prop_operational,
                                 avoidance_rate,
                                 lac_factor = 1){

  if(length(n_transits) != length(prop_operational)){
    stop("Numeric vectors `n_transits` and `prop_operational` must be of
         equal length", call. = FALSE)
  }

  n_transits * prob_single_collision * prop_operational *
    (1 - avoidance_rate) * lac_factor
}
