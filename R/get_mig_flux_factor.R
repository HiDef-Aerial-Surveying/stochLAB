#' Flux factor
#'
#' Returns the flux factor, expressing the total number of
#' bird flights through the rotors of the wind farm per month, if all flights
#' occur within the rotor's circle area of all turbines, and assuming birds take no
#' avoiding action.
#'
#' @details The flux factor is used for other model calculations (e.g.
#'    \code{get_rotor_transits}). Methodology and assumptions underpinning
#'    \code{get_flux_factor} are described in "Stage B" of
#'    \href{https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf}{Band (2012)}
#'
#' @param n_turbines The number of turbines on the wind farm (\eqn{T}).
#' @param rotor_radius The radius of the rotor (\eqn{R}), in metres
#' @param flight_speed The bird flight speed (\eqn{v}), in metres/sec
#' @param bird_dens Vector with daytime in-flight bird densities per unit area
#'   (\eqn{D_A}), for each month, in birds/km^2
#' @param daynight_hrs Two-column data frame with the total number of daylight
#'   hours and night hours at the wind farm site's location, in each month.
#'   Expected column names:
#'   \itemize{
#'    \item{\code{Day}, daylight duration, in decimal hours}
#'    \item{\code{Night}, night time duration, in decimal hours}
#'   }
#' @param noct_activity Nocturnal flight activity level, expressed as a #
#'   proportion of observed levels of daytime activity (\eqn{f_night})
#'
#' @return
#' The number of bird flights potentially transiting through rotors
#' at each time period (assuming no avoidance), if all flights occur within the
#' rotor's circular area

get_mig_flux_factor <- function(n_turbines, rotor_radius,
                            popn_est, daynight_hrs, noct_activity){

  if(length(bird_dens) != nrow(daynight_hrs)){
    stop("Length of vector 'bird_dens' and number of rows of 'daynight_hrs'
         must be equal")
  }

  tot_frontal_area <- n_turbines * pi * rotor_radius^2

  # density needs conversion to birds/m^2
  bird_dens_sqrm <- bird_dens/1e+06

  active_secs <- (daynight_hrs$Day + noct_activity * daynight_hrs$Night) * 3600

  flight_speed * (bird_dens_sqrm/(2*rotor_radius)) * tot_frontal_area * active_secs

}
