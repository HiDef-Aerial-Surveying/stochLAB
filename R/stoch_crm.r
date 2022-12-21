#' Stochastic collision risk model for a single species and one wind farm scenario
#'
#' Runs a Stochastic Collision Risk Model (SCRM) for estimating the number of
#' in-flight collisions with offshore windfarm turbines, for given species and
#' windfarm scenario. Core calculations follow the work developed by [Masden
#' (2015)](https://data.marine.gov.scot/dataset/developing-avian-collision-risk-model-incorporate-variability-and-uncertainty).
#' See **Background and Updates** section below for more details.
#'
#'
#' @param n_iter An integer. The number of iterations for the model simulation.
#' @param flt_speed_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the species flying speed, in metres/sec.
#'   Assumed to follow a Truncated Normal with lower bound at 0 (*tnorm-lw0*).
#' @param body_lt_pars A single row data frame with columns `mean` and `sd`, the
#'   mean and standard deviation of the species body length, in metres. Assumed
#'   to follow a *tnorm-lw0* distribution.
#' @param wing_span_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the species wingspan, in metres. Assumed
#'   to follow a *tnorm-lw0* distribution.
#' @param avoid_bsc_pars,avoid_ext_pars Single row data frames with columns
#'   `mean` and `sd`, the mean and standard deviation of the species avoidance
#'   rate to be used in the basic model (Options 1 and 2) and extended model
#'   (Options 3 and 4) calculations (see **Details** section). Avoidance rate
#'   expresses the probability that a bird flying on a collision course with a
#'   turbine will take evading action to avoid collision, and it is assumed to
#'   follow a Beta distribution.
#' @param noct_act_pars A single row data frame with columns `mean` and `sd`,
#'   The mean and standard deviation of the species nocturnal flight activity
#'   level, expressed as a proportion of daytime activity levels, and assumed to
#'   be Beta distributed.
#' @param prop_crh_pars Required only for model Option 1, a single row data
#'   frame with columns `mean` and `sd`. The mean and standard deviation of the
#'   proportion of flights at collision risk height derived from site survey,
#'   assumed to be Beta distributed.
#'
#' @param bird_dens_opt Option for specifying the random sampling mechanism for bird
#'   densities:
#'   * `"tnorm"`: Sampling of density estimates from a *tnorm-lw0* distribution
#'   (default value),
#'   * `"resample"`: Re-sample draws of bird density estimates (e.g. bootstrap
#'   samples),
#'   * `"qtiles"`: Sampling from a set of quantile estimates of bird densities.
#'
#' @param bird_dens_dt A data frame with monthly estimates of bird density
#'   within the windfarm footprint, expressed as the number of daytime in-flight
#'   birds/km^2 per month. Data frame format requirements:
#'   * If `bird_dens_opt = "tnorm"`, `bird_dens_dt` must contain the following
#'   columns:
#'      * `month`, (unique) month names,
#'      * `mean`, the mean number of birds in flight at any height per square
#'      kilometre in each month,
#'      * `sd`, idem, for standard deviation.
#'   * If `bird_dens_opt = "resample"`, `bird_dens_dt` columns must be named as
#'   months (i.e. `Jan`, `Feb`, ...), each containing random samples of monthly
#'   density estimates.
#'   * If `bird_dens_opt = "qtiles"`, `bird_dens_dt` must comply with:
#'      * First column named as `p`, giving reference probabilities,
#'      * Remaining columns named as months (i.e. `Jan`, `Feb`, ...), each
#'      giving the quantile estimates of bird density in a given month, for the
#'      reference probabilities in column `p`.
#'
#' @param gen_fhd_boots Required only for model Options 2 and 3, a data frame
#'   with bootstrap samples of flight height distributions (FHD) of the species
#'   derived from general (country/regional level) data. FHD provides relative
#'   frequency distribution of bird flights at 1-+
#'   -metre height bands, starting
#'   from sea surface. The first column must be named as `height`,
#'   expressing the lower bound of the height band (thus it's first element must
#'   be 0). Each remaining column should provide a bootstrap sample of the
#'   proportion of bird flights at each height band, with no column naming
#'   requirements.
#'
#'   **NOTE:** [generic_fhd_bootstraps] is a list object with generic FHD
#'   bootstrap estimates for 25 seabird species from Johnson et al
#'   (2014) \doi{10.1111/1365-2664.12191}
#'    (see usage in Example Section below).
#'
#' @param site_fhd_boots Required only for model Option 4, a data frame similar
#'   to `gen_fhd_boots`, but for FHD estimates derived from site-specific
#'   data.
#' @param air_gap_pars A single row data frame with columns `mean` and `sd`, the
#'   mean and standard deviation of the tip clearance gap, in metres, i.e. the
#'   distance between the minimum rotor tip height and the highest astronomical
#'   tide (HAT). Assumed to follow a *tnorm-lw0* distribution.
#' @param rtr_radius_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the radius of the rotor, in metres.
#'   Assumed to follow a *tnorm-lw0* distribution.
#' @param bld_width_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the maximum blade width, in metres.
#'   Assumed to be *tnorm-lw0* distribution.
#' @param bld_chord_prf A data frame with the chord taper profile of the rotor
#'   blade. It must contain the columns:
#'   * `pp_radius`, equidistant intervals of radius at bird passage point,
#'    as a proportion of `rotor_radius`, within the range \eqn{[0, 1]}.
#'   * `chord`, the chord width at `pp_radius`, as a proportion of `blade_width`.
#'
#'   Defaults to a generic profile for a typical modern 5MW turbine. See
#'   [chord_prof_5MW()] for details.
#'
#' @param rtn_pitch_opt a character string, the option for specifying
#'   the sampling mechanism for rotation speed and blade pitch:
#'  * `"probDist"`: sample rotation speed and blade pitch values from a
#'  *tnorm-lw0* distribution (default value).
#'  * `"windSpeedReltn"`: generate rotation speed and blade pitch values as a
#'  function of wind speed intensity.
#' @param bld_pitch_pars Only required if `rtn_pitch_opt = "probDist"`, a single
#'   row data frame with columns `mean` and `sd`, the mean and standard
#'   deviation of the blade pitch angle, i.e. the angle between the blade
#'   surface and the rotor plane, in degrees. Assumed to follow a
#'   *tnorm-lw0* distribution.
#' @param rtn_speed_pars Only required if `rtn_pitch_opt = "probDist"`, a
#'   single row data frame with columns `mean` and `sd`, the mean and standard
#'   deviation of the operational rotation speed, in revolutions per minute.
#'   Assumed to follow a *tnorm-lw0* distribution.
#' @param windspd_pars Only required if `rtn_pitch_opt = "windSpeedReltn"`,
#'   a single row data frame with columns `mean` and `sd`, the mean and the
#'   standard deviation of wind speed at the windfarm site, in metres/sec.
#'   Assumed to follow a *tnorm-lw0* distribution.
#' @param rtn_pitch_windspd_dt Only required if `rtn_pitch_opt = "windSpeedReltn"`,
#'   a data frame giving the relationship between wind speed, rotation speed
#'   and blade pitch values. It must contain the columns:
#'   * `wind_speed`, wind speed in m/s,
#'   * `rtn_speed`, rotation speed in rpm,
#'   * `bld_pitch`, blade pitch values in degrees.
#'
#' @param trb_wind_avbl A data frame with the monthly estimates of operational
#'   wind availability. It must contain the columns:
#'   * `month`, (unique) month names,
#'   * `pctg`, the percentage of time wind conditions allow for turbine operation
#'   per month.
#'
#' @param trb_downtime_pars A data frame with monthly estimates of maintenance
#'   downtime, assumed to follow a *tnorm-lw0* distribution. It
#'   must contain the following columns:
#'   * `month`, (unique) month names,
#'   * `mean`, numeric, the mean percentage of time in each month when turbines
#'   are not operating due to maintenance,
#'   * `sd`, the standard deviation of monthly maintenance downtime.
#'
#' @param wf_n_trbs Integer, the number of turbines on the windfarm.
#' @param out_format Output format specification. Possible values are:
#' * `"draws"`: returns stochastic draws of collisions estimates (default value),
#' * `"summaries"`: returns summary statistics of collisions estimates.
#' @param out_sampled_pars Logical, whether to output summary statistics of values
#'   sampled for each stochastic model parameter.
#' @param out_period Controls level of temporal aggregation of collision
#'   outputs. Possible values are:
#'    * `"months"`: monthly collisions (default value),
#'    * `"seasons"`: collisions per user-defined season,
#'    * `"annum"`: total collisions over 12 months.
#' @param season_specs Only required if `out_period = "seasons"`, a data frame
#'   defining the seasons for aggregating over collision estimates. It must
#'   comprise the following columns:
#'   * `season_id`, (unique) season identifier,
#'   * `start_month`, name of the season's first month,
#'   * `end_month`, name of the season's last month.
#' @param verbose Logical, print model run progress on the console?
#' @param log_file Path to log file to store session info and main model run
#'   options. If set to NULL (default value), log file is not created.
#' @param seed Integer, the random seed for [random number
#'   generation][base::set.seed()], for analysis reproducibility.
#'
#' @inheritParams band_crm
#' @param yinc,xinc numeric values, the increments along the y-axis and x-axis
#'   for numerical integration across segments of the rotor circle. Chosen
#'   values express proportion of rotor radius. By default these are set to
#'   0.05, i.e. integration will be performed at a resolution of one twentieth
#'   of the rotor radius.
#'
#' @inherit band_crm details
#'
#'
#' @return
#' If `out_sampled_pars = FALSE`, returns a list with estimates of number of
#' collisions per chosen time periods, with elements containing the outputs for
#' each CRM Option.
#'
#' If `out_sampled_pars = TRUE`, returns a list object with two top-level
#' elements:
#' * `collisions`, a list comprising collision estimates for each CRM Option,
#' * `sampled_pars`, a list with summary statistics of values sampled for
#' stochastic model parameters.
#'
#' @example examples/stoch_crm_example.r
#' @importFrom rlang .data
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats sd
#' @export
stoch_crm <- function(model_options = c('1', '2', '3', '4'),
                      n_iter = 1000,
                      flt_speed_pars,
                      body_lt_pars,
                      wing_span_pars,
                      avoid_bsc_pars = NULL,
                      avoid_ext_pars = NULL,
                      noct_act_pars,
                      prop_crh_pars = NULL,
                      bird_dens_opt = c("tnorm", "resample", "qtiles"),
                      bird_dens_dt,
                      flight_type,
                      prop_upwind,
                      gen_fhd_boots = NULL,
                      site_fhd_boots = NULL,
                      n_blades,
                      air_gap_pars,
                      rtr_radius_pars,
                      bld_width_pars,
                      bld_chord_prf = chord_prof_5MW,
                      rtn_pitch_opt = c("probDist", "windSpeedReltn"),
                      bld_pitch_pars = NULL,
                      rtn_speed_pars = NULL,
                      windspd_pars  = NULL,
                      rtn_pitch_windspd_dt = NULL,
                      trb_wind_avbl,
                      trb_downtime_pars,
                      wf_n_trbs,
                      wf_width,
                      wf_latitude,
                      tidal_offset,
                      lrg_arr_corr = TRUE,
                      xinc = 0.05,
                      yinc = 0.05,
                      out_format = c("draws", "summaries"),
                      out_sampled_pars = FALSE,
                      out_period = c("months", "seasons", "annum"),
                      season_specs = NULL,
                      verbose = TRUE,
                      log_file = NULL,
                      seed = NULL
) {

  # Preamble -------------------------------------------------------

  if(verbose) cli::cli_h2("Stochastic CRM")

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
      logr::log_print(paste("Stochastic CRM run details",
                            "---------------------------",
                            paste0("Number of iterations: ", n_iter),
                            paste0("Chosen CRM Options: `",
                                   glue::glue_collapse(model_options, sep = "', '", last = "' and '"),
                                   "'"),
                            paste0("Bird density sampling: ", bird_dens_opt),
                            paste0("Rotation speed and blade pitch sampling: ",
                                   rtn_pitch_opt),
                            paste0("Output format: ", out_format),
                            paste0("Output period: ", out_period),
                            sep = "\n"),
                      console = FALSE)
    }else{
      rlang::abort("`log_file` argument must be a non-empty character string.")
    }
  }else{
    if(logr::log_status() == "open") logr::log_close()
    logger <- FALSE
  }




  # Input management -----------------------------------------------------------

  mandatory_args <- c("flt_speed_pars",  "body_lt_pars",  "wing_span_pars",
                      "noct_act_pars", "bird_dens_dt", "flight_type", "prop_upwind",
                      "n_blades", "air_gap_pars", "rtr_radius_pars", "bld_width_pars",
                      "trb_wind_avbl", "trb_downtime_pars", "wf_n_trbs",
                      "wf_width", "wf_latitude", "tidal_offset")

  for(arg in mandatory_args){
    is_missing <- eval(rlang::expr(missing(!!rlang::sym(arg))))
    if(is_missing){
      rlang::abort(paste0("Argument `", arg, "` is missing with no default."))
    }
  }


  # Setup default values for option arguments if unspecified by user
  bird_dens_opt <- match.arg(bird_dens_opt)
  rtn_pitch_opt <- match.arg(rtn_pitch_opt)
  out_period <- match.arg(out_period)
  out_format <- match.arg(out_format)


  # -- For a subset of the data frames, convert column names to lower-case --- ##
  fun_args <- rlang::fn_fmls_names()
  non_df_args <- c("model_options", "n_iter", "bird_dens_opt", "flight_type",
                   "prop_upwind", "n_blades", "rtn_pitch_opt", "wf_n_trbs",
                   "wf_width", "wf_latitude", "tidal_offset", "lrg_arr_corr",
                   "xinc", "yinc", "verbose", "seed", "out_format",
                   "out_sampled_pars", "out_period")

  convert_df_names <- fun_args[-match(non_df_args, fun_args)]
  for(df_name in convert_df_names){
    df_sym <- rlang::sym(df_name)
    null_df <- eval(rlang::expr(is.null(!!df_sym)))
    if(!null_df){
      eval(rlang::expr(names(!!df_sym) <- tolower(names(!!df_sym))))
    }
  }


  # flight-type to lower-case
  flight_type <- tolower(flight_type)


  # Input Format Validation ----------------------------------------------------------
  if(verbose) cli::cli_progress_step("Checking inputs")

  validate_inputs(
    model_options = model_options,
    n_iter = n_iter,
    flt_speed_pars = flt_speed_pars,
    body_lt_pars = body_lt_pars,
    wing_span_pars = wing_span_pars,
    avoid_bsc_pars = avoid_bsc_pars,
    avoid_ext_pars = avoid_ext_pars,
    noct_act_pars = noct_act_pars,
    prop_crh_pars = prop_crh_pars,
    bird_dens_opt = bird_dens_opt,
    bird_dens_dt = bird_dens_dt,
    flight_type = flight_type,
    chord_prof = bld_chord_prf,
    prop_upwind = prop_upwind,
    gen_fhd_boots = gen_fhd_boots,
    site_fhd_boots = site_fhd_boots,
    n_blades = n_blades,
    air_gap_pars = air_gap_pars,
    rtr_radius_pars = rtr_radius_pars,
    bld_width_pars = bld_width_pars,
    rtn_pitch_opt = rtn_pitch_opt,
    bld_pitch_pars = bld_pitch_pars,
    rtn_speed_pars = rtn_speed_pars,
    windspd_pars = windspd_pars,
    rtn_pitch_windspd_dt = rtn_pitch_windspd_dt,
    trb_wind_avbl = trb_wind_avbl,
    trb_downtime_pars = trb_downtime_pars,
    wf_n_trbs = wf_n_trbs,
    wf_width = wf_width,
    wf_latitude = wf_latitude,
    tidal_offset = tidal_offset,
    lrg_arr_corr = lrg_arr_corr,
    xinc = xinc,
    yinc = yinc,
    out_format = out_format,
    out_sampled_pars = out_sampled_pars,
    out_period = out_period,
    season_specs = season_specs,
    verbose = verbose,
    seed = seed,
    fn = "scrm"
  )


  # Data preparation -------------------------------------------------------
  if(verbose) cli::cli_progress_step("Preparing data")

  ## ---- Set months under modelling ----- #

  ### extract and standardize month format from monthly data sets
  b_dens_mth <- switch (bird_dens_opt,
                        tnorm = bird_dens_dt$month,
                        resample = names(bird_dens_dt),
                        qtiles = names(bird_dens_dt)[names(bird_dens_dt) != "p"]
  ) %>% format_months()

  dwntm_mth <- format_months(trb_downtime_pars$month)
  windav_mth <- format_months(trb_wind_avbl$month)

  ### Check consistency
  if(!isTRUE(all.equal(b_dens_mth, dwntm_mth, windav_mth))){
    rlang::warn(
      message = c("Inconsistent set of months provided in monthly datasets:",
                  i = "`bird_dens_opt`, `trb_downtime_pars` and `trb_wind_avbl` cover different set of months.",
                  i = "Calculations only performed for common months.")
    )
  }

  ### Set months to model: only those in common amongst monthly data sets
  mod_mths <- Reduce(intersect, list(b_dens_mth, dwntm_mth, windav_mth))
  ### Order chronologically
  mod_mths <- mod_mths[order(match(mod_mths, month.abb))]
  n_months <- length(mod_mths)



  ## ---- Seasons, checking consistency with modelling months ----- #
  if(out_period == "seasons"){
    seasons_months <- purrr::pmap(season_specs, ~seq_months(..2, ..3))

    #names(seasons_months) <- season_specs$season_id
    names(seasons_months) <- paste0(season_specs$start_month, "_", season_specs$end_month)

    purrr::iwalk(seasons_months, function(x, y){
      if(any(x %nin% mod_mths)){
        rlang::abort(
          message = c(
            paste0("Unavailable monthly data for period specified for season '",
                   y, "'."),
            i = "Ensure you supply suitable monthly data for each season.",
            i = "Monthly data arguments are: `bird_dens_opt`, `trb_downtime_pars` and `trb_wind_avbl`.")
        )
      }
    })
  }

  ## ---- Annum, checking consistency with modelling months ----- #
  if(out_period == "annum"){
    if(any(mod_mths %nin% month.abb)){
      rlang::abort(
        message = c("Monthly data must be supplied for all 12 months when `out_period = annum`.",
                    i = "Monthly data arguments are: `bird_dens_opt`, `trb_downtime_pars` and `trb_wind_avbl`.")
      )
    }
  }

  ## ----- Daylight and night hours per month from latitude  ------ #
  wf_daynight_hrs_month <- Day_Length(wf_latitude)
  # subset for modelling months
  wf_daynight_hrs_month <- subset(wf_daynight_hrs_month, Month %in% mod_mths)


  # ------ Generate grid data for extended model  -------- #
  if(any(model_options %in% c('3', '4'))){
    rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, bld_chord_prf)
  }else{
    rotor_grids <- NULL
  }

  ## ----- Initiate objects to harvest results ----- #
  scrm_draws <- list()
  for(i in model_options){
    scrm_draws[[paste0("opt", i)]] <-
      data.matrix(
        matrix(data = NA, ncol = n_months, nrow = n_iter,
               dimnames = list(NULL, mod_mths))
      )
  }


  # Generate random draws of parameters ----------------------------------------
  if(verbose) cli::cli_progress_step("Sampling parameters")

  # set random seed, if required, for reproducibility
  if(!is.null(seed)){
    set.seed(seed)
  }

  param_draws <- sample_parameters(
    model_options = model_options,
    n_iter = n_iter,
    mod_mths = mod_mths,
    flt_speed_pars = flt_speed_pars,
    body_lt_pars = body_lt_pars,
    wing_span_pars = wing_span_pars,
    avoid_bsc_pars = avoid_bsc_pars,
    avoid_ext_pars = avoid_ext_pars,
    noct_act_pars = noct_act_pars,
    prop_crh_pars = prop_crh_pars,
    bird_dens_opt = bird_dens_opt,
    bird_dens_dt = bird_dens_dt,
    gen_fhd_boots = gen_fhd_boots,
    site_fhd_boots = site_fhd_boots,
    rtr_radius_pars = rtr_radius_pars,
    air_gap_pars = air_gap_pars,
    bld_width_pars = bld_width_pars,
    rtn_pitch_opt = rtn_pitch_opt,
    bld_pitch_pars = bld_pitch_pars,
    rtn_speed_pars = rtn_speed_pars,
    windspd_pars = windspd_pars,
    rtn_pitch_windspd_dt = rtn_pitch_windspd_dt,
    trb_wind_avbl = trb_wind_avbl,
    trb_downtime_pars = trb_downtime_pars,
    lrg_arr_corr = lrg_arr_corr
  )

  # n_iterating over sampled parameters  -----------------------------------------

  if(verbose) cli::cli_progress_step("Calculating collisions | {i}/{n_iter} iterations",
                                     spinner = TRUE)
  #if(verbose) cli::cli_progress_bar("Calculating collisions", total = n_iter, clear = FALSE, )

  for (i in 1:n_iter){

    # Collisions under chosen model Options for current sampled parameters
    collisions_i <-
      band_crm(
        model_options = model_options,
        flight_speed = param_draws$flt_speed[i],
        body_lt = param_draws$body_lt[i],
        wing_span = param_draws$wing_span[i],
        flight_type = flight_type,
        avoid_rt_basic = param_draws$avoid_bsc[i],
        avoid_rt_ext = param_draws$avoid_ext[i],
        noct_activity = param_draws$noct_actv[i],
        prop_crh_surv = param_draws$prop_crh[i],
        dens_month = param_draws$dens_mth[i, ],
        prop_upwind = prop_upwind,
        gen_fhd = param_draws$gen_fhd[, i],
        site_fhd = param_draws$site_fhd[, i],
        rotor_speed = param_draws$rtn_speed[i],
        rotor_radius = param_draws$rtr_radius[i],
        blade_width = param_draws$bld_width[i],
        blade_pitch = param_draws$bld_pitch[i],
        n_blades = n_blades,
        hub_height = param_draws$hub_height[i],
        chord_prof = bld_chord_prf,
        n_turbines = wf_n_trbs,
        turb_oper_month = param_draws$prop_oper_mth[i, ],
        wf_width = wf_width,
        wf_latitude = wf_latitude,
        tidal_offset = tidal_offset,
        lrg_arr_corr = lrg_arr_corr,
        xinc = xinc,
        yinc = yinc,
        rotor_grids = rotor_grids,
        wf_daynight_hrs_month = wf_daynight_hrs_month)


    # store results
    for(option in names(collisions_i)){
      scrm_draws[[option]][i, ] <- collisions_i[[option]]
    }

    if(verbose) cli::cli_progress_update()

  } # end of i to n_iter


  # Gathering Outputs ----------------------------------------------------------
  if(verbose) cli::cli_progress_step("Sorting outputs")

  #scrm_ouputs <- list()

  # ---- Choice for output period ------- #
  if(out_period == "annum"){
    scrm_draws <- scrm_draws %>%
      purrr::map(function(dt){
        dt %>%
          as.data.frame() %>%
          rowSums()
      })
  }

  if(out_period == "seasons"){
    scrm_draws <- scrm_draws %>%
      purrr::map(function(dt){
        purrr::map_dfc(seasons_months, ~rowSums(dt[, .]))
      })
  }

  # ----- Choice for output format ------- #
  if(out_format == "draws") scrm_ouputs <- scrm_draws

  if(out_format == "summaries"){
    scrm_ouputs <- scrm_draws %>%
      purrr::map(function(dt){
        summ_dt <- dt %>%
          as.data.frame() %>%
          dplyr::summarise(
            dplyr::across(dplyr::everything(),
                   list(mean=mean, sd=sd, median = median, #iqr = IQR,
                        `pctl_2.5` = ~quantile(.x, 0.025),
                        `pctl_25` = ~quantile(.x, 0.25),
                        `pctl_75` = ~quantile(.x, 0.75),
                        `pctl_97.5` = ~quantile(.x, 0.975),
                        `pctl_99` = ~quantile(.x, 0.999)),
                   .names = "{.col}---{.fn}")
          ) %>%
          tidyr::pivot_longer(cols = dplyr::everything()) %>%
          tidyr::separate("name", sep = "---", into = c("period", "stat")) %>%
          tidyr::pivot_wider(id_cols = "period", names_from = "stat")

        if(out_period == "annum") summ_dt$period <- "annum"
        if(out_period == "seasons"){
          summ_dt <- summ_dt %>%
            tibble::add_column(season_id = season_specs$season_id, .before = 1)
        }
        return(summ_dt)
      })
  }

  # ---- Output summaries of sampled parameters ------- #
  if(out_sampled_pars){

    scrm_collisions <- scrm_ouputs
    scrm_ouputs <- list()
    scrm_ouputs$collisions <- scrm_collisions

    scrm_ouputs$sampled_pars <- param_draws %>%
      purrr::imap(function(dt, param){

        if(param %nin% c("gen_fhd", "site_fhd")){
          summ_dt <- dt %>%
            as.data.frame() %>%
            dplyr::summarise(
              dplyr::across(dplyr::everything(),
                            list(mean=mean, sd=sd, median = median,
                                 `pctl_2.5` = ~quantile(.x, 0.025),
                                 `pctl_97.5` = ~quantile(.x, 0.975)),
                            .names = "{.col}---{.fn}")
            ) %>%
            tidyr::pivot_longer(cols = dplyr::everything()) %>%
            tidyr::separate("name", sep = "---", into = c("period", "stat")) %>%
            tidyr::pivot_wider(id_cols = "period", names_from = "stat")

          if(any(summ_dt$period == ".")){
            summ_dt <- dplyr::select(summ_dt, -c("period"))
          }
        }else{
          if(param == "gen_fhd"){
            heights <- gen_fhd_boots$height
          }else{
            heights <- site_fhd_boots$height
          }

          summ_dt <- apply(dt, 1, function(x){
            list(
              mean = mean(x),
              sd = sd(x),
              median = median(x),
              `pctl_2.5` = quantile(x, 0.025),
              `pctl_97.5` = quantile(x, 0.975)
            )
          }) %>%
            dplyr::bind_rows() %>%
            tibble::add_column(height = heights, .before = 1)
        }
        return(summ_dt)
      })
  }

  if(verbose) cli::cli_progress_done()

  # Wrap up ----------------------------------------------------------
  if(verbose) cli::cli_alert_success("Job done!")

  if(logger){
    logr::log_close()
    # Undecided on whether to print log file to console
    # if(verbose){
    #    writeLines(c("", "", "Log File:", "",readLines(lf)))
    # }
  }

  return(scrm_ouputs)
}
