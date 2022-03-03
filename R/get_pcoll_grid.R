#' Grid of probabilities of single transit collision at points in rotor circle
#'
#' Calculates single transit collision risk at grid points (X, Y) inside the
#' left-half of the rotor circle, for a given flight direction (upwind or
#' downwind), and assuming no avoidance action. The origin of the (X, Y)
#' coordinates is the rotor center
#'
#' @details Methodology and assumptions underpinning `get_pcoll_grid` are
#'   described in section "Extended Approach Taking Account Of Flight Heights" of
#'   [Band
#'    (2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf). Note that collision risk calculations are based on equation (3)
#'   in "Stage C" section, but using (X, Y) coordinates to reference transit
#'   points instead of the equivalent (r, phi) coordinates.
#'
#' @param rotor_grids A list object containing geometric attributes of the rotor
#'   at equidistant points within its unit circle. This object should be
#'   built via the function `generate_rotor_grids()`.
#' @param flight_type A character string, either 'flapping' or 'gliding',
#'   indicating the species' characteristic flight type.
#' @param direction an integer, indicating the flight direction: 1 for upwind;
#'   -1 for downwind.
#' @inheritParams get_avg_prob_collision
#'
#' @seealso [generate_rotor_grids]
#'
#' @examples
#'
#'   rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW)
#'
#'   get_pcoll_grid(
#'     rotor_grids = rotor_grids,
#'     direction = 1,
#'     rotor_radius = 120,
#'     blade_width = 5,
#'     rotor_speed = 15,
#'     blade_pitch = 15,
#'     flight_type = "flapping",
#'     n_blades = 3,
#'     flight_speed = 13.1,
#'     wing_span = 1.01,
#'     body_lt = 0.85)
#'
#' @export

get_pcoll_grid <- function(rotor_grids, direction, rotor_radius,
                           blade_width, rotor_speed, blade_pitch,
                           flight_type, n_blades, flight_speed, wing_span,
                           body_lt){

  if(direction %nin% c(-1, 1)){
    stop("Invalid argument value: 'direction' must take values -1 (downwind)
         or 1 (upwind)")
  }

  if(flight_type %nin% c("flapping", "gliding")){
    stop("Invalid argument value: 'flight_type' must be either 'flapping'
         or 'gliding' (lowercase)")
  }

  pp_radius <- rotor_radius * rotor_grids$r_grid
  pp_chord <- blade_width * rotor_grids$chord_grid
  omega <- rotor_speed * 2 * pi /60   # conversion to angular velocity, i.e. radians/sec
  pitch <- blade_pitch

  if(flight_type == "gliding") {
    F_fact <- abs(cos(rotor_grids$phi_grid))
  }else{
    F_fact <- 1
  }

  multiplier <- n_blades * omega /(2 * pi *  flight_speed)
  alpha <-  flight_speed/(pp_radius * omega)

  # factor related to time taken for bird to clear the depth of the blade
  depth_clear <- direction * pp_chord * sin(pitch)

  # probability of the bird striking the front face of the blades
  p_blade_strike <- alpha * pp_chord * cos(pitch)

  # factor related to time taken for the full length and wingspan of bird to
  # clear the sweep of the rotors
  rtr_sweep_clear <- wing_span * alpha * F_fact
  rtr_sweep_clear[which(body_lt > rtr_sweep_clear, arr.ind = TRUE)] <- body_lt

  CollideLength <- abs(depth_clear + p_blade_strike) + rtr_sweep_clear

  pcoll <- multiplier * CollideLength

  pcoll[which(pp_radius == 0, arr.ind = TRUE)] <- 1

  pcoll
}


