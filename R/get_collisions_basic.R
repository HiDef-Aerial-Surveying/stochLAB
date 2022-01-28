#' Get expected collisions based on the basic model
#'
#' Provides the number of collisions expected to occur by month. The basic model
#' assumes a uniform distribution of bird flights at risk height (i.e. between
#' min and max rotor height).
#'
#' @param n_transits A numeric vector, the estimated number of bird flights crossing
#'   the rotors of the wind farm at each time period.
#' @param avg_prob_coll A numeric value, the average probability of collision for a single bird
#'   transit through a rotor, assuming no avoidance action (\eqn{p_{average}}).
#' @param mth_prop_oper A numeric vector, the proportion of time during which
#'   turbines are operational per month.
#' @param avoidance_rate A numeric value within the interval \eqn{[0, 1]}. The
#'   avoidance rate, expressing the probability that a bird flying on a
#'   collision course with a turbine will take evading action to avoid collision.
#' @param lac_factor A numerical value, the large array correction factor.
#'   Defaults to 1, meaning large array correction is not applicable.
#'
#' @return A numeric vector. The expected number of collisions at each time
#'   periods
#'
#' @export
get_collisions_basic <- function(n_transits,
                                 avg_prob_coll,
                                 mth_prop_oper,
                                 avoidance_rate,
                                 lac_factor = 1){

  if(length(n_transits) != length(mth_prop_oper)){
    stop("Numeric vectors `n_transits` and `mth_prop_oper` must be of
         equal length", call. = FALSE)
  }

  n_transits * avg_prob_coll * mth_prop_oper *
    (1 - avoidance_rate) * lac_factor
}
