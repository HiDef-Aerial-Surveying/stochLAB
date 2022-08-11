#' Sample rotor grids for generated_rotor_grids unit test
#'
#' @format list of data frames
#'
"rotor_grids_test"



#' Summarized flight height profiles from Johnston et al (2014)
#'
#' A dataframe containing flight height profiles for all species in Johnston
#' et al. (2014). Values are expressed as the proportion
#' of birds in 1 metre height intervals.
#'
#' @format A data frame object with 3 columns containing the maximum likelihood,
#' the median, and Upper/Lower confidence limits of flight height distributions.
#' Flight height bands go from 1 - 300m ASL.
#'
#' \describe{
#'   \item{height}{Height above sea level, in metres. First element represents the
#'   0-1 meters height band, and height interval is 1 metre.}
#'   \item{variable}{The species name and variable from Johnston et al 2014 estimates.
#'   E.G., ArcticSkua.est is the maximum likelihood estimate from  those models.
#'   .est = maximum likelihood, .lcl and .ucl are the lower and upper 95% CLs, and
#'   .med is the median estimate.}
#'   \item{prop}{The proportion of birds within the 1m flight height bands.}
#' }
#' @source \url{https://www.bto.org/our-science/wetland-and-marine/soss/projects}
"Johnston_Flight_heights_SOSS"





#' Rotor blade chord profile
#'
#' A data.frame giving the blade's chord width profile, i.e. the chord width
#' along the length of the blade, provided as a proportion of its maximum width.
#'
#' This is a generic profile for a typical modern 5MW turbine used for offshore generation.
#' Due to commercial sensitivities by blade manufacturers, some of this detailed
#' information may not be readily available for each make/model of blade and
#' hence generic information may have to be used.
#'
#' @note `"chord_prof_5MW"` is numerically identical to `"coverC"`.
#'   `"coverC"` should become deprecated in future versions of the code
#'
#' @format A dataframe
#' \describe{
#'   \item{pp_radius}{radius at bird passage point, as a proportion of rotor radius (R)}
#'   \item{chord}{chord width at pp_radius, as a proportion of the maximum chord width}
#'   ...
#' }
"chord_prof_5MW"



#' Bootstrap samples of generic FHDs of 25 seabird species
#'
#' A list object comprising bootstrap samples of the generic flight height
#' distribution (FHD) of 25 seabird species. FHD is expressed as the proportion
#' of bird flights at 1 metre height intervals.
#'
#' @format A list object with 25 elements, one for each species, containing a data
#' frame with 500 bootstrap samples of the distribution of bird flights with height.
#' Each nested data frame contains:
#' \describe{
#'   \item{height}{Height above sea level, in metres. First element represents the
#'   0-1 meters height band, and height interval is 1 metre.}
#'   \item{bootId_1}{First bootstrap sample of the proportion of birds flights
#'   within each height interval}
#'   ...
#' \item{bootId_200}{200th bootstrap sample of the proportion of birds flights
#' within each height interval}
#' }
#' @source \url{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.12191}
"generic_fhd_bootstraps"



#' Parameter values and outputs from Band's Collision Risk spreadsheet
#' ("Final_Report_SOSS02_BandSpreadSheetWorkedExampl1.xlsm")
#'
#' A list object comprising parameter values and outputs from Band's worked example
#' for testing the consistency of package functions with the original model and
#' its implementation.
#'
#' @format A list object containing:
#' \describe{
#'   \item{flight_speed}{Bird flight speed, in m/s}
#'   \item{rotor_radius}{rotor radius, in meters}
#'   ...
#' }
"band_spreadsheet_dt"


#' Parameter values and outputs from Band's Collision Risk spreadsheet, example
#' nr. 2
#'
#' A list object comprising parameter values and outputs from Band's worked example
#' for testing the consistency of package functions with the original model and
#' its implementation.
#'
#' @format A list object containing:
#' \describe{
#'   \item{flight_speed}{Bird flight speed, in m/s}
#'   \item{rotor_radius}{rotor radius, in meters}
#'   ...
#' }
"band_spreadsheet_dt_2"


#' Example of bird parameters stored in wide format
#'
#' A data frame of (fake) data on biological parameters of three seabird
#' species.
#'
#' Intended to illustrate the application of `stoch_scrm()` to a multiple
#' scenario setting, where parameter data is available from tables in wide format.
#'
#' @format A 3 x 16 data frame, with the biological parameters (columns) for
#'   each of the 3 species (rows). Columns include:
#' \describe{
#'   \item{Species}{Species name}
#'   \item{AvoidanceBasic}{Mean of basic avoidance rate}
#'   \item{AvoidanceBasicSD}{SD of basic avoidance rate}
#'   \item{AvoidanceExtended}{Mean of extended avoidance rate}
#'   \item{AvoidanceExtendedSD}{SD of extended avoidance rate}
#'   \item{Body_Length}{Mean body length}
#'   \item{Body_LengthSD}{SD of body length}
#'   ...
#' }
"bird_pars_wide_example"


#' Example of Truncated Normal parameters for monthly estimates of bird density
#'
#' A data frame of (fake) monthly bird density parameters for three seabird
#' species.
#'
#' Intended to illustrate the application of `stoch_scrm()` to a multiple
#' scenario setting, where parameter data is available from tables in wide
#' format.
#'
#' @format A 3 x 25 data frame, with the monthly density parameters (columns) for
#'   each of the 3 species (rows). Columns include:
#' \describe{
#'   \item{Species}{Species name}
#'   \item{Jan}{January mean density}
#'   \item{JanSD}{SD of density in January}
#'   \item{Feb}{February mean density}
#'   \item{FebSD}{SD of density in February}
#'   ...
#' }
"dens_tnorm_wide_example"


#' Example of turbine and windfarm parameters stored in wide format
#'
#' A data frame of (fake) data on turbine and wind farm features for 3 scenarios.
#'
#' Intended to illustrate the application of `stoch_scrm()` to a multiple
#' scenario setting, where parameter data is available from tables in wide format.
#'
#' @format A 3 x 51 data frame, with the turbine and windfarm parameters (columns) for
#'   each of the 3 development scenarios (rows). Columns include:
#' \describe{
#'   \item{TurbineModel }{The turbine/windfarm scenario ID}
#'   \item{Blades}{Nr of blades}
#'   \item{RotorRadius}{Mean of rotor radius}
#'   \item{RotorRadiusSD}{SD of rotor radius}
#'   \item{HubHeightAdd}{Mean of air gap, the distance between sea surface and lowest tip height.}
#'   \item{HubHeightAddSD}{SD of air gap, as explained above.}
#'   ...
#' }
"turb_pars_wide_example"


#' Example of data with relationship between wind speed, rotation speed and
#' blade pitch
#'
#' Intended to illustrate the application of `stoch_scrm()`
#'
#' @format A 30 x 3 data frame with columns:
#' \describe{
#'   \item{wind_speed}{Wind speed in metres per second.}
#'   \item{rtn_speed}{Blade rotation speed, in revolutions per minute}
#'   \item{bld_pitch}{Blade pitch, in degrees}
#' }
"wndspd_rtn_ptch_example"
