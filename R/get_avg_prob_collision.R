#' Average single transit collision risk with no avoidance
#'
#' @description
#' Calculate the average probability of collision for a single bird transit at
#' any point across the rotor, assuming no avoidance action. Required for the
#' Basic model calculations, where flights at collision risk height are
#' assumed to be uniformly distributed.
#'
#' @details Methodology and assumptions underpinning `get_avg_prob_collision`
#' are described in "Stage C" of
#' [Band (2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf).
#'
#' @param flight_speed Numeric value. The bird flying speed (\eqn{v}), in
#'   metres/sec.
#' @param body_lt Numeric value. The length of the bird (\eqn{L}), in metres.
#' @param wing_span Numeric value. The wingspan of the bird (\eqn{W}), in
#'   metres.
#' @param prop_upwind Numeric value between 0-1 giving the proportion of
#'   flights upwind - defaults to 0.5.
#' @param flap_glide Numeric value representing the correction for flapping or
#'   gliding birds (\eqn{F}).
#' @param rotor_speed Numeric value. The operational rotation speed, in
#'   revolutions/min.
#' @param rotor_radius Numeric value. The radius of the rotor (\eqn{R}), in
#'   metres.
#' @param blade_width Numeric value, giving the maximum blade width, in metres.
#' @param blade_pitch Numeric value. The average blade pitch angle, the angle
#'   between the blade surface and the rotor plane (\eqn{\gamma}), in radians.
#' @param n_blades An integer, the number of blades in rotor (\eqn{b}).
#' @param chord_prof A data frame with the chord taper profile of the rotor
#'   blade. Function expects two named columns:
#'   \itemize{
#'    \item{`pp_radius`, equidistant intervals of radius at bird passage point,
#'    as a proportion of `rotor_radius`, within the range \eqn{[0, 1]}.}
#'    \item{`chord`, the chord width at `pp_radius`, as a proportion
#'       of `blade_width`.}
#'   }
#'   Defaults to a generic profile for a typical modern 5MW turbine. See
#'   [chord_prof_5MW()] for details.
#'
#' @return A numeric value. The average collision probability (risk) for a bird
#'   flying through any point of the rotor circle area.
#'
#' @examples
#'        get_avg_prob_collision(
#'            flight_speed = 13.1,
#'            body_lt = 0.85,
#'            wing_span = 1.01,
#'            prop_upwind = 0.5,
#'            flap_glide = 1,
#'            rotor_speed = 15,
#'            rotor_radius = 120,
#'            blade_width = 5,
#'            blade_pitch = 15,
#'            n_blades = 3,
#'            chord_prof = chord_prof_5MW
#'            )
#'
#' @export
get_avg_prob_collision <- function(flight_speed,
                                   body_lt,
                                   wing_span,
                                   prop_upwind = 0.5,
                                   flap_glide,
                                   rotor_speed,
                                   rotor_radius,
                                   blade_width,
                                   blade_pitch,
                                   n_blades,
                                   chord_prof = chord_prof_5MW) {

  if(prop_upwind < 0 | prop_upwind > 1){
    stop("Value specified for `prop_upwind` should be bounded between 0 and 1")
  }

  # drop chord at radius 0, for consistency with Band calculations (spreadsheet 3)
  chord_prof <- chord_prof[chord_prof$pp_radius != 0, ]

  # rotation speed needs conversion to angular velocity, i.e. radians/sec (\eqn{\Omega})
  omega <- 2 * pi * rotor_speed/60

  # compute terms comprised in probability of collision risk equation
  alpha <- flight_speed/(chord_prof$pp_radius * omega * rotor_radius)

  # upwind length
  Up_length <- collide_length(alpha, chord_prof$chord, body_lt, wing_span,
                              blade_width, blade_pitch, flap_glide,
                              upwind_case = TRUE)

  # upwind probability of collision
  Up_P <- pmin(1, n_blades * omega/(2*pi * flight_speed) * Up_length)

  # downwind length
  Down_length <- collide_length(alpha, chord_prof$chord, body_lt, wing_span,
                               blade_width, blade_pitch, flap_glide,
                               upwind_case = FALSE)

  # downwind probability of collision
  Down_P <- pmin(1, n_blades * omega/(2 * pi * flight_speed) * Down_length)


  # vectors length, for indexing - required for integration step performed below
  lt_p <- length(chord_prof$pp_radius)

  # integration interval
  intgr_int <- 0.05 #chord_prof$pp_radius[2] - chord_prof$pp_radius[1]

  Total_Up_Wind_P <- 2 * (sum(chord_prof$pp_radius[1:(lt_p-1)] * Up_P[1:(lt_p-1)]) +
                            Up_P[lt_p]/2) * intgr_int

  Total_Down_Wind_P <- 2 * (sum(chord_prof$pp_radius[1:(lt_p-1)] * Down_P[1:(lt_p-1)]) +
                              Down_P[lt_p]/2) * intgr_int

  P_Collision <- (prop_upwind * Total_Up_Wind_P) + ((1-prop_upwind) * Total_Down_Wind_P)

  P_Collision
}



# Helper -------------------------------------------------------------------------

collide_length <- function(alpha, chord, body_lt, wing_span, blade_width,
                           blade_pitch, flap_glide, upwind_case) {

  # aspect ratio of bird
  beta <- body_lt/wing_span

  # sign of c_sin_gamma determined by flight direction: upwind (+) or downwind (-)
  updwn_sign <- ifelse(upwind_case, 1, -1)

  # factor related to time taken for bird to clear the depth of the blade
  c_sin_gamma <- updwn_sign * blade_width * chord * sin(blade_pitch)

  # probability of the bird striking the front face of the blades
  alpha_c_cos_gamma <- alpha * blade_width * chord * cos(blade_pitch)

  dplyr::if_else(
    alpha < beta,
    abs(c_sin_gamma + alpha_c_cos_gamma) + body_lt,
    abs(c_sin_gamma + alpha_c_cos_gamma) + (wing_span * flap_glide * alpha)
  )
}

