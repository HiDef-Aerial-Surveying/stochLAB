#' Collision risk model, for a single species and one turbine scenario
#'
#' @description
#' Core function of the Collision Risk Model (CRM). Calculates the expected
#' number of in-flight collisions per month of a seabird species on a given
#' offshore windfarm, for a choice of model options.
#'
#' Calculations are equivalent to those performed on the original CRM
#' [spreadsheet](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band2Tool.xlsm),
#' as per [Band
#' (2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf),
#' providing backward compatibility with the original outputs.
#'
#' @param model_options Character vector, the model options for calculating collision
#'   risk (see **Details** section below).
#'
#' @param avoid_rt_basic,avoid_rt_ext Numeric values. The avoidance rate for,
#'   respectively, the basic model (i.e. required for model Options 1 and 2) and
#'   the extended model (i.e. required for Options 3 and 4). Avoidance rate
#'   expresses the probability that a bird flying on a collision course with a
#'   turbine will take evading action to avoid collision.
#'
#' @param dens_month Data frame, containing estimates of daytime in-flight
#'   bird densities per month within the windfarm footprint, in birds/km^2. It
#'   must contain the following named columns:
#'   \itemize{
#'    \item{ `month`, the month names.}
#'    \item{`dens`, the number of birds in flight at any height per
#'    square kilometre in each month.}
#'   }
#'
#' @param turb_oper_month Data frame, holding the proportion of time during
#'   which turbines are operational per month. The following named column are
#'   expected:
#'   \itemize{
#'    \item{`month`, the month names.}
#'    \item{`prop_oper`, the proportion of time operating, per month.}
#'   }
#'
#' @param gen_fhd,site_fhd Data frame objects, with flight height distributions
#'   (fhd) of the species - the relative frequency distribution of bird flights
#'   at 1-metre height intervals from sea surface.
#'   Specifically:
#'
#'   \itemize{
#'    \item{`gen_fhd`, Data frame with the species' generic fhd derived
#'    from combining wider survey data. Only required for model Options 2 and 3}
#'    \item{`site_fhd`, Data frame with the species' site-specific fhd
#'    derived from local survey data. Only required for model Option 4}
#'   }
#'
#'   Data frames must contain the following named columns:
#'   \itemize{
#'    \item{`height`, integers representing height bands from sea surface,
#'    in metres. Function expects 0 as the first value, representing the 0-1m
#'    band.}
#'    \item{`prop`, the proportion of flights at each height band.}
#'   }
#'
#' @param yinc,xinc numeric values, the increments along the y-axis and x-axis
#'   for numerical integration across segments of the rotor circle. Chosen
#'   values express proportion of rotor radius. By default these are set to
#'   0.05, i.e. integration will be performed at a resolution of one twentieth
#'   of the rotor radius.
#'
#' @param lrg_arr_corr Boolean value. If TRUE, the large array correction will
#'   be applied. This is a correction factor to account for the decay in
#'   bird density at later rows in wind farms with a large array of turbines.
#'
#' @param ... Additional arguments to pass on when function is called in
#'   `stoch_crm()`, namely `rotor_grids` and `wf_daynight_hrs_month`.
#'
#' @inheritParams get_avg_prob_collision
#' @inheritParams get_lac_factor
#' @inheritParams get_flux_factor
#' @inheritParams get_fhd_rotor
#' @inheritParams crm_opt1
#' @inheritParams crm_opt3
#' @inheritParams DayLength
#'
#'
#' @details
#' Collision risk can be calculated under 4 options, specified by `model_options`:
#' \itemize{
#'    \item **Option 1** - Basic model with proportion at
#'    collision risk height derived from site survey (`prop_crh_surv`).
#'    \item **Option 2** - Basic model with proportion
#'    at collision risk height derived from a generic flight height distribution
#'    (`gen_fhd`).
#'    \item **Option 3** - Extended model using a
#'    generic flight height distribution (`gen_fhd`).
#'    \item **Option 4** - Extended model using a
#'    site-specific flight height distribution (`site_fhd`).
#' }
#' Where,
#' \itemize{
#'    \item Basic model - assumes a uniform distribution of bird flights at
#'    collision risk height (i.e. above the minimum and below the maximum height
#'    of the rotor blade).
#'    \item Extended model - takes into account the distribution of bird flight
#'    heights at collision risk height.
#' }
#'
#' @section Validation and pre-processing of inputs:
#'  `band_crm()` requirements and behaviour are dependent on how it is called:
#'  \describe{
#'     \item{As a stand-alone function}{
#'     \itemize{
#'        \item All arguments are expected to be specified as describe above
#'        \item Input validation and pre-processing are carried out.
#'        }
#'      }
#'     \item{Inside `stoch_crm()`}{
#'     \itemize{
#'     \item Assumes inputs have already been pre-processed and validated, and
#'     thence it skips those steps.
#'     \item  Additional arguments `rotor_grids` and
#'     `wf_daynight_hrs_month` need to be passed to the function. Under the
#'     stochastic context, these quantities can be calculated ahead of the
#'     simulation loop to maximize performance. \item Furthermore,
#'     `gen_fhd`, `site_fhd`, `dens_month` and
#'     `turb_oper_month` can be provided as numeric vectors
#'     }}
#'     }
#'
#'
#' @section Code revision and optimization:
#'
#' Core code performing Band calculations in [Masden
#' (2015)](https://www.gov.scot/publications/scottish-marine-freshwater-science-vol-6-14-developing-avian-collision/) implementation
#' was substantially re-factored, re-structured and streamlined to conform with
#' conventional R packages requirements.
#'
#' Furthermore, previous code underpinning key calculations for the extended
#' model was replaced by an alternative approach, resulting in significant gains
#' in computational speed - over 30x faster than Masden's approach. This
#' optimization is particularly beneficial under a stochastic context, when Band
#' calculations are called repeatedly, potentially thousands of times. See
#' [generate_rotor_grids()] for further details.
#'
#'
#' @return Returns the expected number of bird collisions per month, for each of
#'   the chosen CRM Options. Returned months are those shared between
#'   `dens_month` and `turb_oper_month`. Output format is specific to how
#'   the function is called:
#' \itemize{
#'    \item data frame object, if called as a stand-alone function.
#'    \item list object, if called inside `stoch_crm()`.
#' }
#'
#' @example examples/band_crm_example.r
#'
#' @export
band_crm <- function(model_options = c('1', '2', '3', '4'),
                     flight_speed,
                     body_lt,
                     wing_span,
                     flight_type,
                     avoid_rt_basic = NULL,
                     avoid_rt_ext = NULL,
                     noct_activity,
                     prop_crh_surv = NULL,
                     dens_month,
                     prop_upwind,
                     site_fhd = NULL,
                     gen_fhd = NULL,
                     rotor_speed,
                     rotor_radius,
                     blade_width,
                     blade_pitch,
                     n_blades,
                     hub_height,
                     chord_prof = chord_prof_5MW,
                     n_turbines,
                     turb_oper_month,
                     wf_width,
                     wf_latitude,
                     tidal_offset,
                     lrg_arr_corr = TRUE,
                     xinc = 0.05,
                     yinc = 0.05,
                     ...) {

  # Input validation 1: chosen model options Vs. required data ----------------
  if (any(model_options == '1')) {
    if (is.null(prop_crh_surv)) {
      stop("Missing argument value: `prop_crh_surv` needs to be provided if
           `model_options` comprise '1'. The proportion of flights at
           collision risk height is required for calculations under Model
           Option 1.")
    }}

  if (lrg_arr_corr|any(model_options %in% c('1', '2'))) {
    if (is.null(avoid_rt_basic)) {
      stop("Missing argument value: `avoid_rt_basic` needs to be provided if
           `model_options`comprise '1' and/or '2', or `lrg_arr_corr` == TRUE.
           Calculations under the Basic Model underlying Options 1 and 2, and the
           large array correction, expect a specific value of avoidance rate.")
    }}

  if (any(model_options %in% c('3', '4'))) {
    if (is.null(avoid_rt_ext)) {
      stop("Missing argument value: `avoid_rt_ext` needs to be provided if
           `model_options`comprise values '3' and/or '4'. Calc2lations under the
           Extended Model underlying Options 3 and 4 expect a specific value of
           avoidance rate.")
    }}


  if (any(model_options %in% c('2', '3'))) {
    if (is.null(gen_fhd)) {
      stop("Missing argument value: `gen_fhd` needs to be provided if `model_options`
             comprise values '2' and/or '3'. Calculations under Model Options 2 and
             3 require a generic FHD.")
    }}

  if (any(model_options == '4')) {
    if (is.null(site_fhd)) {
      stop("Missing argument value: `site_fhd` needs to be provided if value '4' is
             comprised in vector `model_options`. Calculations under Model
             Option 4 require a site-specific FHD based on survey data.")
    }}



  # Conditional processing -----------------------------------------------------
  #
  # Validation and data pre-processing if band_crm() is not being called
  # inside stoch_crm().
  #
  # Implied else streamlines process for stoch_crm() use, and assumes inputs have
  # already been validated and pre-processed
  if(parent_fn_name() != "stoch_crm"){
  # parent_fun <- parent_fn_name()
  # if(parent_fun %notin% c("stoch_crm", "stoch_crm_old_v2")){

    # Check if mandatory args are missing
    mandatory_args <- c("flight_speed", "body_lt", "wing_span", "flight_type",
                        "noct_activity", "dens_month", "prop_upwind",
                        "rotor_speed", "rotor_radius", "blade_width",
                        "blade_pitch", "n_blades", "hub_height", "n_turbines",
                        "turb_oper_month", "wf_width", "wf_latitude", "tidal_offset")

    for(arg in mandatory_args){
      is_missing <- eval(rlang::expr(missing(!!rlang::sym(arg))))
      if(is_missing){
        rlang::abort(paste0("Argument `", arg, "` is missing, with no default."))
      }
    }


    ## column names to lower-case
    names(dens_month) <- tolower(names(dens_month))
    names(turb_oper_month) <- tolower(names(turb_oper_month))
    if(!is.null(gen_fhd)){ names(gen_fhd) <- tolower(names(gen_fhd))  }
    if(!is.null(site_fhd)){ names(site_fhd) <- tolower(names(site_fhd)) }

    ## variables to lower-case
    flight_type <- tolower(flight_type)


    ## input validation 2: input formatting
    validate_inputs(
      model_options = model_options,
      flight_speed = flight_speed,
      body_lt = body_lt,
      chord_prof = chord_prof,
      avoid_rt_basic = avoid_rt_basic,
      avoid_rt_ext = avoid_rt_ext,
      noct_activity = noct_activity,
      dens_month = dens_month,
      rotor_speed = rotor_speed,
      rotor_radius = rotor_radius,
      blade_width = blade_width,
      blade_pitch = blade_pitch,
      n_blades = n_blades,
      hub_height = hub_height,
      n_turbines = n_turbines,
      turb_oper_month = turb_oper_month,
      flight_type = flight_type,
      gen_fhd = gen_fhd,
      site_fhd = site_fhd,
      tidal_offset = tidal_offset,
      wf_width = wf_width,
      wf_latitude = wf_latitude,
      lrg_arr_corr = lrg_arr_corr,
      xinc = xinc,
      yinc = yinc,
      fn = "crm")

    ## Rotor's grid data for extended models
    if (any(model_options %in% c('3', '4'))) {
      rotor_grids <- generate_rotor_grids(yinc = yinc, xinc = xinc, chord_prof)
    }

    ## Daylight hours and night hours per month based on the windfarm's latitude
    wf_daynight_hrs_month <- DayLength(wf_latitude)


    ## check consistency between monthly data sets
    if (nrow(dens_month) != nrow(turb_oper_month)){
      warning("Inconsistent set of months provided in density dataset `dens_month`
              and windfam operational data `turb_oper_month`. Calculations
              will only be performed for common months.")
    }

    ## months in abbreviated format
    dens_month$month <- format_months(dens_month$month)
    turb_oper_month$month <- format_months(turb_oper_month$month)

    ## months to model: only those in common between density and operational datasets
    model_months <- intersect(dens_month$month, turb_oper_month$month)

    ## subset monthly data to modelled months
    wf_daynight_hrs_month <- subset(wf_daynight_hrs_month, Month %in% model_months)
    dens_month <- subset(dens_month, month %in% model_months)
    turb_oper_month <- subset(turb_oper_month, month %in% model_months)

    ## sort monthly data by month, in chronological order
    dens_month <- dens_month[order(match(dens_month$month, month.abb)), ]
    turb_oper_month <- turb_oper_month[order(match(turb_oper_month$month, month.abb)), ]

  } else {

    # fetch the arguments passed on in `...`, if function called in stoch_crm
    additional_args <- list(...)
    wf_daynight_hrs_month <- additional_args$wf_daynight_hrs_month
    rotor_grids <- additional_args$rotor_grids
  }


  # data.frame inputs: overwrite with data vectors ----------------------------
  if(is.data.frame(gen_fhd)) gen_fhd <- gen_fhd$prop
  if(is.data.frame(site_fhd)) site_fhd <- site_fhd$prop
  if(is.data.frame(dens_month)) dens_month <- dens_month$dens
  if(is.data.frame(turb_oper_month)) turb_oper_month <- turb_oper_month$prop_oper


  # Further model inputs   -----------------------------------------------------

  ## flight-type correction factor
  flap_glide_fctr <- if(flight_type == "flapping") 1 else 2/pi

  ## proportion of time operational over the year
  avg_prop_oper <- mean(turb_oper_month)

  # Collision risk steps -----------------------------------------------------

  # STEP 1 - Calculate average collision probability for a single transit at
  # any point of the rotor disc [Stage C in Band (2012)]
  avg_collision_risk <-
    get_avg_prob_collision(
      flight_speed = flight_speed,
      body_lt = body_lt,
      wing_span = wing_span,
      prop_upwind = prop_upwind,
      flap_glide = flap_glide_fctr,
      rotor_speed = rotor_speed,
      rotor_radius = rotor_radius,
      blade_width = blade_width,
      blade_pitch = blade_pitch,
      n_blades = n_blades,
      chord_prof = chord_prof
    )


  # STEP 2 - Set up Large Array Correction Factor
  if (lrg_arr_corr == TRUE) {
    L_ArrayCF <-
      get_lac_factor(
        n_turbines = n_turbines,
        rotor_radius = rotor_radius,
        avoidance_rate = avoid_rt_basic,
        avg_prob_coll = avg_collision_risk,
        avg_prop_operational = avg_prop_oper,
        wf_width = wf_width
      )
  } else{
    # set multiplier to 1 to neutralise large array correction
    L_ArrayCF <- 1
  }


  # STEP 3 - Calculate bird flux per month
  flux_fct <-
    get_flux_factor(
      n_turbines = n_turbines,
      rotor_radius = rotor_radius,
      flight_speed = flight_speed,
      bird_dens = dens_month,
      daynight_hrs = wf_daynight_hrs_month,
      noct_activity = noct_activity
    )


  # STEP 4 - calculate FHD across rotor height

  ## for model options 2 or 3, i.e. using generic FHD
  if (any(model_options %in% c('2', '3'))) {
    gen_fhd_at_rotor <-
      get_fhd_rotor(
        hub_height = hub_height,
        fhd = gen_fhd,
        rotor_radius = rotor_radius,
        tidal_offset = tidal_offset,
        yinc = yinc)
  }

  ## for model option 4, i.e. using survey FHD
  if (any(model_options == '4')) {
    surv_fhd_at_rotor <-
      get_fhd_rotor(
        hub_height = hub_height,
        fhd = site_fhd,
        rotor_radius = rotor_radius,
        tidal_offset = tidal_offset,
        yinc = yinc)
  }

  browser()
  # STEP 5 - Calculate collisions per month under each model option

  band_outputs <- list()
  if (any(model_options == '1')){
    band_outputs$opt1 <-
      crm_opt1(
        flux_factor = flux_fct,
        prop_crh_surv = prop_crh_surv,
        avg_prob_coll = avg_collision_risk,
        mth_prop_oper = turb_oper_month,
        avoidance_rate = avoid_rt_basic,
        lac_factor = L_ArrayCF)
  }

  if(any(model_options == '2')){
    band_outputs$opt2 <-
      crm_opt2(
        flux_factor = flux_fct,
        d_y = gen_fhd_at_rotor,
        avg_prob_coll = avg_collision_risk,
        mth_prop_oper = turb_oper_month,
        avoidance_rate = avoid_rt_basic,
        lac_factor = L_ArrayCF)
  }


  if(any(model_options == '3')){
    band_outputs$opt3 <-
      crm_opt3(
        rotor_grids = rotor_grids,
        d_y_gen = gen_fhd_at_rotor,
        rotor_radius = rotor_radius,
        blade_width = blade_width,
        rotor_speed = rotor_speed,
        blade_pitch = blade_pitch,
        flight_type = flight_type,
        wing_span = wing_span,
        flight_speed = flight_speed,
        body_lt = body_lt,
        n_blades = n_blades,
        prop_upwind = prop_upwind,
        avoidance_rate = avoid_rt_ext,
        flux_factor = flux_fct,
        mth_prop_oper = turb_oper_month,
        lac_factor = L_ArrayCF
      )
  }


  if(any(model_options == '4')){
    band_outputs$opt4 <-
      crm_opt4(
        rotor_grids = rotor_grids,
        d_y_surv = surv_fhd_at_rotor,
        rotor_radius = rotor_radius,
        blade_width = blade_width,
        rotor_speed = rotor_speed,
        blade_pitch = blade_pitch,
        flight_type = flight_type,
        wing_span = wing_span,
        flight_speed = flight_speed,
        body_lt = body_lt,
        n_blades = n_blades,
        prop_upwind = prop_upwind,
        avoidance_rate = avoid_rt_ext,
        flux_factor = flux_fct,
        mth_prop_oper = turb_oper_month,
        lac_factor = L_ArrayCF
      )
  }

  # Conditional outputting  -----------------------------------------------------
  if(parent_fn_name() != "stoch_crm"){
  #parent_fun <- parent_fn_name()
  #if(parent_fun %notin% c("stoch_crm", "stoch_crm_old_v2")){
    dplyr::bind_cols(band_outputs) %>%
      tibble::add_column(
        month = model_months[order(match(model_months, month.abb))],
        .before = 1
      )
  } else {
    band_outputs
  }

}
