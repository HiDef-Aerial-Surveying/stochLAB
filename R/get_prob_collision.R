#' Average single transit collision risk with no avoidance
#'
#' Calculate the average probability of collision for a single bird transit
#' at any point across the rotor, assuming no avoidance action.
#'
#' @details Methodology and assumptions underpinning `get_prob_collision`
#' are described in "Stage C" of
#' [Band (2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf)
#'
#' @param chord_prof a data.frame with the blade's chord taper profile. Function
#'   expects two named columns:
#'   \itemize{
#'    \item{`pp_radius`, radius at bird passage point, as a proportion
#'       of `rotor_radius`}, starting from 0.05 to 1
#'    \item{`chord`, chord width at `pp_radius`, as a proportion
#'       of `blade_width`}
#'   }
#' @param flight_speed The bird flying speed (\eqn{v}), in metres/sec
#' @param body_lt The length of the bird (\eqn{L}), in metres
#' @param wing_span The wingspan of the bird (\eqn{W}), in metres
#' @param prop_upwind A value between 0-1 bounded as proportion of flights
#'   upwind - default of 0.5.
#' @param flap_glide A decimal value representing the correction for flapping
#'   or gliding birds (\eqn{F}).
#' @param rotor_speed operational rotation speed, in revolutions/min
#' @param rotor_radius The radius of the rotor (\eqn{R}), in metres
#' @param blade_width The maximum blade width, in metres
#' @param blade_pitch The average blade pitch angle, the angle between the blade surface and
#'   the rotor plane (\eqn{\gamma}), in radians
#' @param n_blades The number of blades in rotor (\eqn{b}).
#'
#' @return A numeric value. The average collision probability (risk) for a bird
#'   flying through a rotor's circle area
#'
#' @export
get_prob_collision <- function(flight_speed,
                               body_lt,
                               wing_span,
                               prop_upwind = 0.5,
                               flap_glide,
                               rotor_speed,
                               rotor_radius,
                               blade_width,
                               blade_pitch,
                               n_blades,
                               chord_prof) {

  if(prop_upwind < 0 | prop_upwind > 1){
    stop("Value specified for `prop_upwind` should be bounded between 0 and 1")
  }

  # rotation speed needs conversion to angular velocity, i.e. radians/sec (\eqn{\Omega})
  rtr_speed_radps <- 2 * pi * rotor_speed/60

  # compute terms comprised in probability of collision risk equation
  alpha <- flight_speed/(chord_prof$pp_radius * rtr_speed_radps * rotor_radius)

  # upwind length
  Up_length <- collide_length(alpha, chord_prof$chord, body_lt, wing_span,
                              blade_width, blade_pitch, flap_glide,
                              upwind_case = TRUE)

  # upwind probability of collision
  Up_P <- pmin (1, n_blades * rtr_speed_radps/(2*pi * flight_speed) * Up_length)

  # downwind length
  Down_length <- collide_length(alpha, chord_prof$chord, body_lt, wing_span,
                               blade_width, blade_pitch, flap_glide,
                               upwind_case = FALSE)

  # downwind probability of collision
  Down_P <- pmin(1, n_blades * rtr_speed_radps/(2 * pi * flight_speed) * Down_length)


  # vectors length, for indexing - required for integration step performed below
  lt_p <- length(chord_prof$pp_radius)

  Total_Up_Wind_P <- 2 * (sum(chord_prof$pp_radius[1:(lt_p-1)] * Up_P[1:(lt_p-1)]) +
                            Up_P[lt_p]/2) * 0.05

  Total_Down_Wind_P <- 2 * (sum(chord_prof$pp_radius[1:(lt_p-1)] * Down_P[1:(lt_p-1)]) +
                              Down_P[lt_p]/2) * 0.05

  P_Collision <- (prop_upwind * Total_Up_Wind_P) + ((1-prop_upwind) * Total_Down_Wind_P)

  P_Collision
}





collide_length <- function(alpha, chord, body_lt, wing_span, blade_width,
                           blade_pitch, flap_glide, upwind_case = TRUE) {

  # aspect ratio of bird
  beta <- body_lt/wing_span

  # sign of c_sin_gamma terms depend on whether the flight is upwind (+) or downwind (-).
  updwn_sign <- ifelse(upwind_case, 1, -1)

  c_sin_gamma <- updwn_sign * blade_width * chord * sin(blade_pitch)
  alpha_c_cos_gamma <- alpha * blade_width * chord * cos(blade_pitch)

  dplyr::if_else(
    alpha < beta,
    abs(c_sin_gamma + alpha_c_cos_gamma) + body_lt,
    abs(c_sin_gamma + alpha_c_cos_gamma) + (wing_span * flap_glide * alpha)
  )
}

