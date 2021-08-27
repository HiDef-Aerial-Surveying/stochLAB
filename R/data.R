#' Bird parameters for the CRM
#'
#' A dataframe of species biological parameters for the model
#'
#' @format A dataframe
#' \describe{
#'   \item{Species}{Species name}
#'   \item{AvoidanceBasic}{Basic avoidance rate}
#'   \item{AvoidanceBasicSD}{Basic avoidance rate SD}
#'   ...
#' }
"Bird_Data"


#' Bird densities for the CRM
#'
#' A dataframe of Bird densities for each month
#'
#' @format A dataframe
#' \describe{
#'   \item{Species}{Species name}
#'   \item{AvoidanceBasic}{Basic avoidance rate}
#'   \item{AvoidanceBasicSD}{Basic avoidance rate SD}
#'   ...
#' }
"Count_Data"

#' Bird parameters for the CRM
#'
#' A dataframe of species biological parameters for the model
#'
#' @format A dataframe
#' \describe{
#'   \item{Species}{Species name}
#'   \item{AvoidanceBasic}{Basic avoidance rate}
#'   \item{AvoidanceBasicSD}{Basic avoidance rate SD}
#'   ...
#' }
"Flight_Height"


#' Bird parameters for the CRM
#'
#' A dataframe of species biological parameters for the model
#'
#' @format A dataframe
#' \describe{
#'   \item{Species}{Species name}
#'   \item{AvoidanceBasic}{Basic avoidance rate}
#'   \item{AvoidanceBasicSD}{Basic avoidance rate SD}
#'   ...
#' }
"Johnston_flight_heights"



#' Bird parameters for the CRM
#'
#' A dataframe of species biological parameters for the model
#'
#' @format A dataframe
#' \describe{
#'   \item{Species}{Species name}
#'   \item{AvoidanceBasic}{Basic avoidance rate}
#'   \item{AvoidanceBasicSD}{Basic avoidance rate SD}
#'   ...
#' }
"Turbine_Data"


#' Rotor blade sizes
#'
#' A dataframe giving the width of the chord relative to its maximum width at given points along the radius of the rotor
#'
#' @format A dataframe
#' \describe{
#'   \item{rad}{radius}
#'   \item{circ}{Circumference}
#'   ...
#' }
"coverC"


#' Start up values for the mCRM
#'
#' A dataset of startup values for the app to run
#'
#' @format A nested list with 3 upper levels
#' \describe{
#'   \item{turbinepars}{Parameters for the wind farm turbines}
#'   \item{windfarmpars}{Parameters for the wind farms themselves}
#'   \item{speciespars}{Parameters for species}
#'   ...
#' }
"startUpValues"
