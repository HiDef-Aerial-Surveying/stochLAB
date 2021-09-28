#' Number of birds transiting through rotors assuming no avoidance
#'
#' Calculate the potential number of bird flights transiting through the rotors
#' of the wind farm per month, assuming birds take no avoiding action
#'
#' @details Methodology and assumptions underpinning \code{get_rotor_transits}
#' are described in "Stage B" of
#' \href{https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf}{Band (2012)}
#'
#' @param flux_factor a vector containing the flux factor for each month
#' @param prop_crh The proportion of flights at risk height (\eqn{Q_2R} or
#'    (\eqn{Q'_2R})
#'
#' @seealso [get_flux_factor()] for calculating the flux factor
#'
#' @return
#' The number of bird flights potentially transiting through rotors
#' at each time period (assuming no avoidance)
get_rotor_transits <- function(flux_factor, prop_crh){
  flux_factor * prop_crh
}
