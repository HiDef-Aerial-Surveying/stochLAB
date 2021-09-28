#'Returns the proportion of birds at height bands across the rotor height

#'@description Derive the expected proportion of bird flights at equidistant height
#'bands between the lowest and highest points of the rotor. In other words, returns
#'the flight height distribution at collision risk (\eqn{d(y)}).
#'
#'@param hub_height The height of the rotor hub (\eqn{H}), the sum of rotor
#' radius and minimum blade clearance above the highest astronomical tide (HAT)
#'@param fhd A vector, each element with the proportion of bird flights per-1 metre
#'   height intervals from sea surface, starting from 0-1 metre band.
#'@param rotor_radius The radius of the rotor (\eqn{R}), in metres.
#'@param tide_off The tidal offset, the difference between HAT and mean sea
#'   level, in metres
#'
#'@return The flight height distribution at collision risk, i.e. between
#'   minimum and maximum rotor height.
get_fhd_rotor <- function(hub_height, fhd, rotor_radius, tide_off) {

  # vector of distances from rotor center to vertical equidistant points between minimum
  # and maximum rotor heights, expressed as a proportion of rotor radius.
  # -1: bottom blade tip; 1: top blade tip; 0: rotor center
  y <- round(seq(-1,1,0.05),2)

  # height bands at collision risk, relative to sea level
  height <- hub_height + (y * rotor_radius) + tide_off

  # derive proportion of flights at height bands across rotor from provided FHD
  rotor_radius * (fhd[floor(height)+1] + ((fhd[floor(height)+1] - fhd[ceiling(height)+1]) * (floor(height)-height)))

}
