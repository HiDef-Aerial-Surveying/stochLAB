#' Returns the proportion of birds at equidistant height points along the rotor
#'
#' @description Derive the expected proportion of bird flights at equidistant
#'   heights between the lowest and highest points of the rotor. In other
#'   words, returns the flight height distribution at collision risk
#'   (\eqn{d(y)}).
#'
#' @param hub_height A numeric value, the height of the rotor hub (\eqn{H}), given by the
#'   sum of rotor radius and minimum blade clearance above the highest
#'   astronomical tide (HAT), in metres.
#' @param fhd A numeric vector, the proportion of bird flights per-1 metre
#'   height bands from sea surface, starting from 0-1 metre band.
#' @param rotor_radius A numeric value, the radius of the rotor (\eqn{R}), in
#'   metres.
#' @param tidal_offset A numeric value, the tidal offset, the difference between
#'   HAT and mean sea level, in metres.
#' @param yinc A numeric value, the increment for height points between the
#'   lowest and highest points of the rotor, expressed as a proportion of rotor
#'   radius (defaults is 0.05).
#'
#' @return A list containing two elements:
#'   \itemize{
#'    \item{`y`, vector with distance from rotor center to vertical equidistant
#'    points between minimum and maximum rotor heights, expressed as a proportion
#'    of rotor radius.}
#'    \item{`dy`, vector with the flight height distribution at collision risk,
#'   i.e. between minimum and maximum rotor height.}
#'   }
#'
#'
#' @examples
#'
#'  gen_fhd_dat <- Johnston_Flight_heights_SOSS %>%
#'        dplyr::filter(variable=="Gannet.est") %>%
#'        dplyr::select(height,prop)
#'  if(is.data.frame(gen_fhd_dat)) gen_fhd <- gen_fhd_dat$prop
#'
#'  get_fhd_rotor(
#'    hub_height = 150,
#'    fhd = gen_fhd,
#'    rotor_radius = 120,
#'    tidal_offset = 2.5,
#'    yinc = 0.05)
#'
#' @export

get_fhd_rotor <- function(hub_height,
                          fhd,
                          rotor_radius,
                          tidal_offset,
                          yinc = 0.05) {

  # vector of distances from rotor center to vertical equidistant points between
  # minimum and maximum rotor heights, expressed as a proportion of rotor
  # radius. -1: bottom blade tip; 1: top blade tip; 0: rotor centre
  y <- seq(-1, 1, yinc)

  # sequence of heights at collision risk, relative to sea level
  height <- hub_height + (y * rotor_radius) + tidal_offset

  # interpolate proportion of flights at each height across rotor from provided
  # FHD. FHD starts at 0m, so '+1' term is required for correct indexing
  dy <- rotor_radius * (
    fhd[floor(height)+1] +
      ((fhd[floor(height)+1] - fhd[ceiling(height)+1]) * (floor(height)-height))
    )

  list(y = y, dy = dy)

}
