#' Returns the proportion of birds at height bands along the rotor
#'
#' @description Derive the expected proportion of bird flights at equidistant
#'   height bands between the lowest and highest points of the rotor. In other
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
#' @param yinc A numeric value, the increment for height bands between the
#'   lowest and highest points of the rotor, expressed as a proportion of rotor
#'   radius (defaults is 0.05).
#'
#' @return A numeric vector, the flight height distribution at collision risk,
#'   i.e. between minimum and maximum rotor height.
#'
get_fhd_rotor <- function(hub_height,
                          fhd,
                          rotor_radius,
                          tidal_offset,
                          yinc = 0.05) {

  # vector of distances from rotor center to vertical equidistant points between
  # minimum and maximum rotor heights, expressed as a proportion of rotor
  # radius. -1: bottom blade tip; 1: top blade tip; 0: rotor centre
  y <- round(seq(-1, 1, yinc), 2)

  # height bands at collision risk, relative to sea level
  height <- hub_height + (y * rotor_radius) + tidal_offset

  # interpolate proportion of flights at height bands across rotor from provided
  # FHD. FHD starts at 0m, so '+1' term is required for correct indexing
  rotor_radius * (
    fhd[floor(height)+1] +
      ((fhd[floor(height)+1] - fhd[ceiling(height)+1]) * (floor(height)-height))
    )

}
