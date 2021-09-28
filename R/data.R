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


#' Bootstrap samples of generic FHDs of 25 seabird species
#'
#' A list object comprising bootstrap samples of the generic flight height distribution
#' (FHD) of 25 seabird species. FHD is expressed as the proportion of bird flights at 1
#' height intervals
#'
#' @format A list object with 25 elements, one for each species, containing a data
#' frame with 500 bootstrap samples of the distribution of bird flights with height.
#' Each nested data frame contains:
#' \describe{
#'   \item{Height_m}{Height above sea level, in metres. Height interval must be 1 metre}
#'   \item{bootId_1}{First bootstrap sample of the proportion of birds flights within each
#'   height interval}
#'   ...
#' \item{bootId_200}{200th bootstrap sample of the proportion of birds flights within each
#'   height interval}
#' }
#' @source \url{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.12191}
"generic_fhd_bootstraps"



#' Parameter values and outputs from Band's Collision Risk spreadsheet
#' ("Final_Report_SOSS02_BandSpreadSheetWorkedExampl1.xlsm")
#'
#' A list object comprising parameter values and outputs from Band's worked example
#' for testing the consistency of package functions with the original model and
#' its implementation
#'
#' @format A list object with ...:
#' \describe{
#'   \item{flight_speed}{Bird flight speed, in m/s}
#'   \item{rotor_radius}{rotor radius, in meters}
#'   ...
#' }
"band_spreadsheet_dt"

