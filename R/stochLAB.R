#' stochLAB: Stochastic Collision Risk Model
#'
#' @description
#' {stochLAB} is a tool to run stochastic (and deterministic) Collision Risk
#' Models (CRM) for seabirds on offshore wind farms.
#'
#' The main functions of stochLAB are:
#'  * [stoch_crm()]: Stochastic Collision Risk Model,
#'  * [band_crm()]: Deterministic Collision Risk Model,
#'  * [mig_stoch_crm()]: Stochastic Migration Collision risk Model
#'
#' @docType package
#' @name stochLAB
#'
#' @section Overview:
#'
#' The `{stochLAB}` package is an adaptation of the [R
#' code](https://data.marine.gov.scot/dataset/developing-avian-collision-risk-model-incorporate-variability-and-uncertainty-r-code-0)
#' developed by [Masden
#' (2015)](https://data.marine.gov.scot/dataset/developing-avian-collision-risk-model-incorporate-variability-and-uncertainty)
#' to incorporate variability and uncertainty in the avian collision risk model
#' originally developed by [Band
#' (2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf).
#'
#' Code developed under `{stochLAB}` substantially re-factored and re-structured
#' Masden's (heavily script-based) implementation into a user-friendly,
#' streamlined, well documented and easily distributed tool. Furthermore, this
#' package lays down the code infrastructure for easier incorporation of new
#' functionality, e.g. extra parameter sampling features, model expansions,
#' etc.
#'
#' Previous code underpinning key calculations for the extended model has been
#' replaced by an alternative approach, resulting in significant gains in
#' computational speed over Masden's code. This optimization
#' is particularly beneficial under a stochastic context, when Band calculations
#' are computed repeatedly. See [generate_rotor_grids()] for further details.
#'
#'
#' @section Function `stoch_crm()`:
#'
#' `stoch_crm()` is essentially a replacement function for script *BandModel.r*
#' in Masden's approach. Main changes in terms of user interface include:
#'
#'  * Collision model runs for one single scenario (i.e. one species for one
#'  turbine scenario) at each `stoch_crm()` call. Apart from gains in development
#'  code structure, this unit-based approach was considered to offer greater end
#'  user flexibility for setting up and managing multiple scenarios (including
#'  parallel computation).
#'
#'  * Input parameters now entered explicitly into feature-specific arguments,
#'  instead of bundled together into wide-column tables. This improves code
#'  readability and reduces the quantity of hard-coded parameter names,
#'  therefore making referencing errors less likely.
#'
#'  * Outputs no longer saved automatically to external files.
#'
#' ## Seasonal Outputs
#'
#' `stoch_crm()` now provides an option for user-defined seasonal outputs, allowing
#' for country/region specific seasonal definitions.
#'
#' Currently implemented CRM calculations produce collision estimates on a
#' monthly basis to reflect changing bird abundance within the windfarm area.
#' Seasonal estimates are obtained by aggregating monthly estimates over each
#' season definition, with uncertainty in collision estimates being suitably
#' propagated to seasonal outputs.
#'
#'
#' ## Sampling distributions
#'
#' Masden's implementation used Normal and Truncated Normal distributions to
#' generate random parameter values. However, the Normal distribution is
#' unbounded, and so there was the risk of randomly drawing inappropriate values in
#' some cases.
#'
#' `stoch_crm()` extends the use of bounded probability distributions to all model
#' parameters. Strictly positive features (e.g. bird densities, body length,
#' turbine downtime, etc.) are sampled from Truncated Normal distributions, while
#' features constrained between 0 and 1 (e.g. nocturnal activity, proportion
#' of flights at collision risk height, etc) are assumed to follow Beta
#' distributions.
#'
#' In addition, new functionality has been incorporated to allow the use of bird
#' density resamples (e.g. bootstrap samples) or quantile estimates from density
#' estimation models in the simulations.
#'
#'
#' @section Function `band_crm()`:
#' `band_crm()` performs the core CRM calculations required to estimate the
#' number of collisions, as per [Band
#' (2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf).
#' While being the computing workhorse of the `stoch_crm()` function, it can
#' also be used alone, providing backward compatibility with the original Band
#' spreadsheet outputs.
#'
#' @example examples/band_crm_example.r
#'
#' @seealso
#' Useful links:
#' * <https://github.com/HiDef-Aerial-Surveying/stochLAB>
#' * Report bugs at <https://github.com/HiDef-Aerial-Surveying/stochLAB/issues>



NULL
#> NULL
