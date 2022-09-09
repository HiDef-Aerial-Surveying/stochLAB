#' Stochastic migration collision risk model
#'
#' Run migration stochastic collision risk model for a single species and one turbine scenario
#'
#' @details
#' This function is an adaption of code from Masden(2015) used for estimating
#' the collision risk of seabirds in offshore windfarm sites and is a further
#' adaptation from Band(2012). It is a further adaptation of the stoch_crm function.
#'
#' The collision risk model evaluates risk for each defined migratory period where
#' flux rate is simply the number of birds travelling through the windfarm.
#'
#' Changes in relation to previous top-line function \code{stoch_crm}
#' \itemize{
#'   \item function will run only option 1 for migratory species
#' }
#'
#'
#' @return Estimates of number of collisions per migratory season for the n
#'   number of iterations specified
#'
#' @param wing_span_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the species wingspan, in metres. Assumed
#'   to follow a *tnorm-lw0* distribution.
#' @param flt_speed_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the species flying speed, in metres/sec.
#'   Assumed to follow a Truncated Normal with lower bound at 0 (*tnorm-lw0*).
#' @param body_lt_pars A single row data frame with columns `mean` and `sd`, the
#'   mean and standard deviation of the species body length, in metres. Assumed
#'   to follow a *tnorm-lw0* distribution.
#' @param prop_crh_pars  A single row data frame with columns `mean` and `sd`.
#'   The mean and standard deviation of the proportion of flights at
#'   collision risk height.
#' @param avoid_bsc_pars Single row data frame with columns
#'   `mean` and `sd`, the mean and standard deviation of the species avoidance
#'   rate to be used. Avoidance rate expresses the probability that a bird
#'   flying on a collision course with a turbine will take evading action
#'   to avoid collision, and it is assumed to follow a Beta distribution.
#' @param n_blades An integer. The number of blades per turbine (defaults to 3)
#' @param n_turbines An integer. The number of turbines expected to be installed
#' @param n_iter An integer > 0. The number of iterations for the model simulation.
#' @param chord_profile A data frame with the chord taper profile of the rotor
#'   blade. It must contain the columns:
#'   * `pp_radius`, equidistant intervals of radius at bird passage point,
#'    as a proportion of `rotor_radius`, within the range \eqn{[0, 1]}.
#'   * `chord`, the chord width at `pp_radius`, as a proportion of `blade_width`.
#'   Defaults to a generic profile for a typical modern 5MW turbine. See
#'   [chord_prof_5MW()] for details.
#' @param season_specs A data frame
#'   defining the seasons for aggregating over collision estimates. It must
#'   comprise the following columns:
#'   * `season_id`, (unique) season identifier,
#'   * `start_month`, name of the season's first month,
#'   * `end_month`, name of the season's last month.
#' @param popn_estim_pars A single row data frame with columns `mean` and `sd`.
#'   The population estimate of the species expected to fly through the wind farm
#'   area.
#' @param LargeArrayCorrection A boolean. Should the large array correction be calculated
#' @param log_file Path to log file to store session info and main model run
#'   options. If set to NULL (default value), log file is not created.
#' @param seed Integer, the random seed for [random number
#'   generation][base::set.seed()], for analysis reproducibility.
#' @param verbose boolean. TRUE for a verbose output
#' @param flight_type A character. Either "flying" or "gliding" representing
#'   the type of flight most commonly used by the species
#'
#' @inheritParams sample_turbine_mCRM
#' @inheritParams get_lac_factor
#' @inheritParams get_avg_prob_collision
#' @inheritParams Day_Length
#'
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @export
#
#' @example examples/mig_stoch_crm_example.r


mig_stoch_crm <- function(
  wing_span_pars,
  flt_speed_pars,
  body_lt_pars,
  prop_crh_pars,
  avoid_bsc_pars,
  n_turbines,
  n_blades = 3,
  rtn_speed_pars,
  bld_pitch_pars,
  rtr_radius_pars,
  bld_width_pars,
  wf_width,
  wf_latitude,
  flight_type,
  prop_upwind = 0.5,
  popn_estim_pars,
  season_specs,
  chord_profile = chord_prof_5MW,
  trb_wind_avbl,
  trb_downtime_pars,
  n_iter = 10,
  LargeArrayCorrection = TRUE,
  log_file = NULL,
  seed = NULL,
  verbose = TRUE) {


  if(verbose) cli::cli_h2("Stochastic CRM for Migratory birds")

  # Setting up logger
  if(!is.null(log_file)){
    if(is.character(log_file) & nchar(log_file)>0){
      logger <- TRUE
      logger_dir <- dirname(log_file)
      if(!dir.exists(logger_dir)){
        dir.create(logger_dir, recursive = TRUE)
      }
      # Open logger for run session info
      lf <- logr::log_open(log_file, show_notes = FALSE)
      # Send message to log
      logr::log_print(paste("Migration stochastic CRM run details",
                            "---------------------------",
                            paste0("Number of iterations: ", n_iter),
                            #paste0("Bird density sampling: ", bird_dens_opt),
                            #paste0("Rotation speed and blade pitch sampling: ",
                            #       rtn_pitch_opt),
                            #paste0("Output format: ", out_format),
                            #paste0("Output period: ", out_period),
                            sep = "\n"),
                      console = FALSE)
    }else{
      rlang::abort("`log_file` argument must be a non-empty character string.")
    }
  }else{
    if(logr::log_status() == "open") logr::log_close()
    logger <- FALSE
  }


  # Input management --------------------------------------------------------
  if(verbose) cli::cli_progress_step("Checking for manditory arguments")

  mandatory_args <- c("flt_speed_pars",  "body_lt_pars",  "wing_span_pars",
                      "prop_crh_pars", "avoid_bsc_pars","n_turbines",
                      "n_blades","rtn_speed_pars", "bld_pitch_pars",
                      "rtr_radius_pars","bld_width_pars","wf_width",
                      "wf_latitude", "prop_upwind", "popn_estim_pars",
                      "season_specs", "chord_profile", "n_iter", "LargeArrayCorrection")

  for(arg in mandatory_args){
    is_missing <- eval(rlang::expr(missing(!!rlang::sym(arg))))
    if(is_missing){
      rlang::abort(paste0("Argument `", arg, "` is missing with no default."))
    }
  }


  # Validate inputs ---------------------------------------------------------

  if(verbose) cli::cli_progress_step("Data validation")

  validate_inputs(
    model_options = 1,
    wing_span_pars = wing_span_pars,
    flt_speed_pars = flt_speed_pars,
    body_lt_pars = body_lt_pars,
    prop_crh_pars = prop_crh_pars,
    avoid_bsc_pars = avoid_bsc_pars,
    n_turbines = n_turbines,
    n_blades = n_blades,
    flight_type = flight_type,
    rtn_speed_pars = rtn_speed_pars,
    bld_pitch_pars = bld_pitch_pars,
    rtr_radius_pars = rtr_radius_pars,
    bld_width_pars = bld_width_pars,
    wf_width = wf_width,
    wf_latitude = wf_latitude,
    prop_upwind = prop_upwind,
    popn_estim_pars = popn_estim_pars,
    season_specs = season_specs,
    chord_prof = chord_profile,
    trb_wind_avbl = trb_wind_avbl,
    trb_downtime_pars = trb_downtime_pars,
    n_iter = n_iter,
    lrg_arr_corr = LargeArrayCorrection,
    seed = seed,
    verbose = verbose,
    fn="mcrm"
  )




  # Initiate objects to harvest results ----------------------------------------
  sampledBirdParams <- list()

  mcrm_outputs <- data.matrix(
    matrix(data = NA, ncol = nrow(season_specs), nrow = n_iter,
           dimnames = list(NULL, season_specs$season_id))
  )

  mcrm_flux <- data.matrix(
    matrix(data = NA, ncol = nrow(season_specs), nrow = n_iter,
           dimnames = list(NULL, season_specs$season_id))
  )

  # Prepare inputs  ------------------------------------------------------------

  ## For the migration app we make the assumption to be precautionary, that the animals are
  ## passing through the windfarm area during the day time.
  ## Thus, flux is simply a count of the number of birds passing through the area



  # Calculate day length ----------------------------------------------------

  ## get daylight hours and night hours per month based on the latitude
  ## This is only for future proofing, 2021 model does not assume day hours have
  ## impact on birds
  daynight_hrs_month <- stochLAB::Day_Length(wf_latitude)

  if(verbose) cli::cli_progress_step("Sampling data")
  # Sample bird attributes --------------------------------------------------

  Flap_Glide <- ifelse (flight_type == "flapping", 1, 2/pi)
  # set random seed, if required, for reproducibility
  if(!is.null(seed)){
    set.seed(seed)
  }


  sampledBirdParams$WingSpan <- stochLAB::sampler_hd(dat = wing_span_pars$sd,
                                           mode = 'rtnorm',
                                           n = n_iter,
                                           mean=wing_span_pars$mean,
                                           sd = wing_span_pars$sd,
                                           lower = 0)

  sampledBirdParams$BodyLength <- stochLAB::sampler_hd(dat = body_lt_pars$sd,
                                             mode = 'rtnorm',
                                             n = n_iter,
                                             mean = body_lt_pars$mean,
                                             sd = body_lt_pars$sd,
                                             lower = 0)


  sampledBirdParams$FlightSpeed <- stochLAB::sampler_hd(dat = flt_speed_pars$sd,
                                              mode = 'rtnorm',
                                              n = n_iter,
                                              mean=flt_speed_pars$mean,
                                              sd = flt_speed_pars$sd,
                                              lower = 0)

  sampledBirdParams$Avoidance <- sampler_hd(dat = avoid_bsc_pars$sd,
                                            mode = 'rbeta',
                                            n = n_iter,
                                            mean=avoid_bsc_pars$mean,
                                            sd = avoid_bsc_pars$sd)

  ### Sampler deactivated Nov 2021 as PCH is a point estimate
  sampledBirdParams$PCH <- rep(prop_crh_pars$mean,n_iter)

  ### Nocturnal activity set to 0 for future proofing
  sampledBirdParams$NocturnalActivity <- rep(0,n_iter)



  # Sample the turbine parameters  ------------------------------------------
  sampledTurbine <- sample_turbine_mCRM(rtn_speed_pars = rtn_speed_pars,
                                  bld_pitch_pars = bld_pitch_pars,
                                  rtr_radius_pars = rtr_radius_pars,
                                  bld_width_pars = bld_width_pars,
                                  season_specs = season_specs,
                                  n_iter = n_iter,
                                  trb_wind_avbl = trb_wind_avbl,
                                  trb_downtime_pars = trb_downtime_pars)


  # Sample the counts -------------------------------------------------------
  SampledCounts <- stochLAB::sampler_hd(dat = popn_estim_pars$sd,
                              mode = 'rtnorm',
                              n = n_iter,
                              mean=popn_estim_pars$mean,
                              sd = popn_estim_pars$sd,
                              lower = 0)


  ### Iterate over seasons, then over sampled parameters
  if(verbose) cli::cli_progress_step("Running simulation...")
  for(bp in season_specs$season_id){
    sampTurb <- sampledTurbine %>% dplyr::select(RotorRadius,BladeWidth,RotorSpeed,Pitch,dplyr::contains(bp))
    if(ncol(sampTurb)>4){

      for(i in 1:n_iter){
        # STEP 1 - Calculate the probability of collision assuming no avoidance ----
        p_single_collision <-
          get_avg_prob_collision(
            flight_speed = sampledBirdParams$FlightSpeed[i],
            body_lt = sampledBirdParams$BodyLength[i],
            wing_span = sampledBirdParams$WingSpan[i],
            prop_upwind = prop_upwind,
            flap_glide = Flap_Glide,
            rotor_speed = sampTurb$RotorSpeed[i],
            rotor_radius = sampTurb$RotorRadius[i],
            blade_width = sampTurb$BladeWidth[i],
            blade_pitch = sampTurb$Pitch[i],
            n_blades = n_blades,
            chord_prof = chord_profile
          )

        # STEP 2 - Set up Large Array Correction Factor ----
        if (LargeArrayCorrection == TRUE) {
          L_ArrayCF <-
            get_lac_factor(
              n_turbines = n_turbines,
              rotor_radius = sampTurb$RotorRadius[i],
              avoidance_rate = sampledBirdParams$Avoidance[i],
              avg_prob_coll = p_single_collision,
              avg_prop_operational = sampTurb[i,paste0(bp,"_OT")],
              wf_width = wf_width
            )

        } else{
          # set multiplier to 1 to dismiss large array correction
          L_ArrayCF <- 1
        }

        # STEP 3 - Calculate the flux factor in the wind farm using the popn estimate ----
        flux_fct <- get_mig_flux_factor(n_turbines = n_turbines,
                                        rotor_radius = sampTurb$RotorRadius[i],
                                        wf_width = wf_width,
                                        popn_est = SampledCounts[i])

        mcrm_flux[i,bp] <- flux_fct
        # Step 4 - Apply option 1 of the CRM ----
        mcrm_outputs[i,bp] <- crm_opt1(
          flux_factor = flux_fct,
          prop_crh_surv = sampledBirdParams$PCH[i],
          avg_prob_coll = p_single_collision,
          mth_prop_oper = sampTurb[i,paste0(bp,"_OT")],
          avoidance_rate = sampledBirdParams$Avoidance[i],
          lac_factor = L_ArrayCF)

      } # i in 1:niter end

    }
  }

  if(verbose) cli::cli_progress_step("Creating outputs")

  return(list(collisions=as_tibble(mcrm_outputs),flux_rates=as_tibble(mcrm_flux)))
}

