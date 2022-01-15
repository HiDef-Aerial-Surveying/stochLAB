#' Calculate the total proportion of bird flights at collision risk based
#' on a flight height distribution

#'@description Calculate the expected proportion of bird flights at collision risk
#'   height (i.e. at rotor height, between bottom and top of the rotor) based on the
#'   bird's flight height distribution  (\eqn{Q'_{2R}}).
#'
#'@param d_y Numeric vector with the proportion of birds at height bands across the rotor
#'   disc
#'
#'@return The total proportion of birds at collision risk height derived from a flight
#'   height distribution
#'
#' @export
get_prop_crh_fhd <- function(d_y) {

  # number of height bands
  n_bands <- length(d_y)

  # integration over the rotor's height bands
  0.05 * (d_y[1]/2 + d_y[n_bands]/2 + sum(d_y[c(2:(n_bands-1))]))
}


