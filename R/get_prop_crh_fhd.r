#' Calculate the total proportion of bird flights at collision risk based
#' on a flight height distribution

#' @description Calculate the expected proportion of bird flights at collision risk
#'   height (i.e. at rotor height, between bottom and top of the rotor) based on the
#'   bird's flight height distribution  (\eqn{Q'_{2R}}).
#'
#' @param d_y List containing the following elements:
#' \itemize{
#'    \item{`y`, numeric vector with distance from rotor center to vertical equidistant
#'    points between minimum and maximum rotor heights, expressed as a proportion
#'    of rotor radius.}
#'    \item{`dy`, numeric vector with the proportion of birds at height bands
#'    defined by list element `y`.}
#'   }
#'   Important: use function [get_fhd_rotor()] to derive `d_y`.
#'
#'
#' @return The total proportion of birds at collision risk height derived from a flight
#'   height distribution
#'
#' @examples
#'  gen_fhd_dat <- Johnston_Flight_heights_SOSS %>%
#'       dplyr::filter(variable=="Gannet.est") %>%
#'       dplyr::select(height,prop)
#'
#'  gen_fhd <- gen_fhd_dat$prop
#'
#'  d_y <-
#'     get_fhd_rotor(
#'       hub_height = 150,
#'       fhd = gen_fhd,
#'       rotor_radius = 120,
#'       tidal_offset = 2.5,
#'       yinc = 0.05)
#'
#'  prop_chr_fhd <- get_prop_crh_fhd(d_y)
#'
#' @export
get_prop_crh_fhd <- function(d_y) {

  # number of height bands
  n_bands <- length(d_y$y)

  # width of height slices
  yinc <- d_y$y[2] - d_y$y[1]

  # integration over the rotor's height bands
  dy <- d_y$dy
  yinc * (dy[1]/2 + dy[n_bands]/2 + sum(dy[c(2:(n_bands-1))]))
}


