#' Flux factor
#'
#' Returns the flux factor, expressing the total number of bird flights through
#' the rotors of the wind farm per month if all flights occurred within the
#' rotor's circle area of all turbines, i.e. before the proportion at risk
#' height and avoidance level are taken into account.
#'
#'
#' @param n_turbines An integer, the number of turbines on the wind farm (\eqn{T}).
#' @param rotor_radius A numeric value, the radius of the rotor (\eqn{R}), in metres.
#' @param flight_speed A numeric value, the bird flight speed (\eqn{v}), in metres/sec.
#' @param bird_dens A numeric vector with daytime in-flight bird densities (\eqn{D_A}), for
#'   each month, in birds/km^2.
#' @param daynight_hrs A data frame with the total number of daylight
#'   hours and night hours at the wind farm site's location, in each month.
#'   It must contain, at least, the following columns:
#'   * `Month`, name of the month.
#'   * `Day`, daylight duration, in decimal hours.
#'   * `Night`, night time duration, in decimal hour
#'
#' @param noct_activity A numeric value. The nocturnal flight activity level,
#'   expressed as a proportion of daytime activity levels (\eqn{f_night}).
#'
#' @details
#' The flux factor is used for other model calculations.
#'    Methodology and assumptions underpinning
#'    `get_flux_factor` are described in "Stage B" of
#'    [Band (2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf)
#'
#'
#' @return The number of bird flights potentially transiting through rotors at
#'   each time period (assuming no avoidance), if all flights occur within the
#'   rotor's circular area.
#'
#' @examples
#'   get_flux_factor(
#'       n_turbines = 100,
#'       rotor_radius = 120,
#'       flight_speed = 13.1,
#'       bird_dens = c(1.19,0.85,1.05,1.45,1.41,1.45,1.12,1.45,0.93,0.902,1.06,1.23),
#'       daynight_hrs = Day_Length(52),
#'       noct_activity = 0.5
#'       )
#' @export



get_flux_factor <- function(n_turbines, rotor_radius, flight_speed,
                            bird_dens, daynight_hrs, noct_activity){

  if(length(bird_dens) != nrow(daynight_hrs)){
    stop("Length of vector 'bird_dens' must be identical to number of rows of
         'daynight_hrs'")
  }

  tot_frontal_area <- n_turbines * pi * rotor_radius^2

  # convert density to birds/m^2
  bird_dens_sqrm <- bird_dens/1e+06

  active_secs <- (daynight_hrs$Day + noct_activity * daynight_hrs$Night) * 3600

  flight_speed * (bird_dens_sqrm/(2*rotor_radius)) * tot_frontal_area * active_secs

}
