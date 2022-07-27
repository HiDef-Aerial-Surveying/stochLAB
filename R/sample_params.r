#' Parameter sampling whiz
#'
#' Generates the random samples of all the stochastic CRM parameters. For internal use.
#'
#' @inheritParams stoch_crm
#' @param mod_mths character vector, the names of months under modelling
#'
#' @inherit band_crm details
#'
#' @return A list object with each element comprising sampled values of given CRM parameter
#'
#' @examples
#'
#'   bird_dens_dt <- data.frame(
#'     month = month.abb,
#'     mean = runif(12, 0.8, 1.5),
#'     sd = runif(12, 0.2, 0.3)
#'   )
#'
#'  # wind availability
#'   trb_wind_avbl <- data.frame(
#'     month = month.abb,
#'     pctg = runif(12, 85, 98)
#'   )
#'
#'   # maintenance downtime
#'   trb_downtime_pars <- data.frame(
#'     month = month.abb,
#'     mean = runif(12, 6, 10),
#'     sd = rep(2, 12))
#'
#'   # Wind speed relationships
#'   wind_rtn_ptch <- data.frame(
#'     wind_speed = seq_len(30),
#'     rtn_speed = 10/(30:1),
#'     bld_pitch = c(rep(90, 4), rep(0, 8), 5:22)
#'     )
#'
#'
#'   bird_dens_opt <- "tnorm"
#'   ### extract and standardize month format from monthly data sets
#'   b_dens_mth <- switch (bird_dens_opt,
#'                         tnorm = bird_dens_dt$month,
#'                         resample = names(bird_dens_dt),
#'                         qtiles = names(bird_dens_dt)[names(bird_dens_dt) != "p"]
#'   ) %>% format_months()
#'   dwntm_mth <- format_months(trb_downtime_pars$month)
#'   windav_mth <- format_months(trb_wind_avbl$month)
#'   ### Set months to model: only those in common amongst monthly data sets
#'   mod_mths <- Reduce(intersect, list(b_dens_mth, dwntm_mth, windav_mth))
#'   ### Order chronologically
#'   mod_mths <- mod_mths[order(match(mod_mths, month.abb))]
#'
#'   param_draws <- sample_parameters(
#'     model_options = c(1,2,3),
#'     n_iter = 10,
#'     mod_mths = mod_mths,
#'     flt_speed_pars = data.frame(mean=7.26,sd=1.5),
#'     body_lt_pars = data.frame(mean=0.39,sd=0.005),
#'     wing_span_pars = data.frame(mean=1.08,sd=0.04),
#'     avoid_bsc_pars = data.frame(mean=0.99,sd=0.001),
#'     avoid_ext_pars = data.frame(mean=0.96,sd=0.002),
#'     noct_act_pars = data.frame(mean=0.033,sd=0.005),
#'     prop_crh_pars = data.frame(mean=0.06,sd=0.009),
#'     bird_dens_opt = "tnorm",
#'     bird_dens_dt = bird_dens_dt,
#'     gen_fhd_boots = generic_fhd_bootstraps[[1]],
#'     site_fhd_boots = NULL,
#'     rtr_radius_pars = data.frame(mean=80,sd=0),
#'     air_gap_pars = data.frame(mean=36,sd=0),
#'     bld_width_pars = data.frame(mean=8,sd=0),
#'     rtn_pitch_opt = "windSpeedReltn",
#'     bld_pitch_pars = NULL,
#'     rtn_speed_pars = NULL,
#'     windspd_pars = data.frame(mean=7.74,sd=3),
#'     rtn_pitch_windspd_dt = wind_rtn_ptch,
#'     trb_wind_avbl = trb_wind_avbl,
#'     trb_downtime_pars = trb_downtime_pars,
#'     lrg_arr_corr = TRUE
#'     )
#'
#' @export
#'
sample_parameters <- function(model_options,
                          mod_mths,
                          n_iter = 10,
                          flt_speed_pars,
                          body_lt_pars,
                          wing_span_pars,
                          avoid_bsc_pars,
                          avoid_ext_pars,
                          noct_act_pars,
                          prop_crh_pars,
                          bird_dens_opt = "tnorm",
                          bird_dens_dt,
                          gen_fhd_boots = NULL,
                          site_fhd_boots = NULL,
                          rtr_radius_pars,
                          air_gap_pars,
                          bld_width_pars,
                          rtn_pitch_opt = "probDist",
                          bld_pitch_pars,
                          rtn_speed_pars,
                          windspd_pars,
                          rtn_pitch_windspd_dt,
                          trb_wind_avbl,
                          trb_downtime_pars,
                          lrg_arr_corr
){


  # Features with no conditional requirements ----------------------------------

  ## --- Parameter table, for subsequent list-type processing
  prob_dist_pars <- tibble::tribble(
    ~feature,       ~mode,     ~mean,                ~sd,                  ~lw_trunc,
    "flt_speed",   'rtnorm',  flt_speed_pars$mean,   flt_speed_pars$sd,    0,
    "body_lt",     'rtnorm',  body_lt_pars$mean,     body_lt_pars$sd,      0,
    "wing_span",   'rtnorm',  wing_span_pars$mean,   wing_span_pars$sd,    0,
    "noct_actv",   'rbeta',   noct_act_pars$mean,    noct_act_pars$sd,     NA,
    "rtr_radius",  'rtnorm',   rtr_radius_pars$mean,  rtr_radius_pars$sd,  0, # changed from Norm to 0-bounded tNorm because parameter must be > 0
    "air_gap",     'rtnorm',   air_gap_pars$mean,     air_gap_pars$sd,     0, # idem
    "bld_width",   'rtnorm',   bld_width_pars$mean,   bld_width_pars$sd,   0  # idem
  )

  sampled_pars <- prob_dist_pars %>%
    split(.$feature) %>%
    purrr::map(function(x, ...){
      sampler_hd(dat = x$sd,
                 mode = x$mode,
                 n = n_iter,
                 mean = x$mean,
                 sd = x$sd,
                 lower = x$lw_trunc)
    })

  ## --- Compute samples for hub height (air gap + rotor radius)
  sampled_pars$hub_height <- sampled_pars$air_gap + sampled_pars$rtr_radius



  # Bird densities per month  --------------------------------------------------

  # Initiate storage dataset
  sampled_pars$dens_mth <- data.matrix(
    matrix(data = 0, ncol = length(mod_mths), nrow = n_iter,
           dimnames = list(NULL, mod_mths))
  )

  # sampling options
  if(bird_dens_opt == "tnorm"){
    # formatting
    bird_dens_dt <- bird_dens_dt %>%
      dplyr::mutate(month = format_months(month)) %>%
      dplyr::filter(month %in% mod_mths) %>%
      dplyr::arrange(match(month, month.abb))

    # from truncated normal pdf
    for(i in 1:nrow(bird_dens_dt)){

      ## If bird density is 0, then set the value to 0

      sampled_pars$dens_mth[, i] <- sampler_hd(dat = bird_dens_dt$sd[i],
                                               mode = 'rtnorm',
                                               n = n_iter,
                                               mean = bird_dens_dt$mean[i],
                                               sd = bird_dens_dt$sd[i],
                                               lower = 0)
    }

  }else if(bird_dens_opt == "resample"){

    # re-sampling random draws (e.g. bootstrap samples)
    sampled_pars$dens_mth <- bird_dens_dt %>%
      dplyr::rename_with(.fn = format_months) %>% # formatting month names
      dplyr::select(mod_mths) %>% # (no need for chronological sorting as mod_mths takes care of it)
      dplyr::slice_sample(n = n_iter, replace = TRUE) %>%
      data.matrix()

  }else if(bird_dens_opt == "qtiles"){

    # formatting month names
    # (no need for chronological sorting, as mod_mths takes care of it)
    bird_dens_dt <- dplyr::rename_with(bird_dens_dt, .fn = format_months, .cols = -p)

    # from quantiles, based on the empirical cumulative distribution function
    for(m in mod_mths){
      sampled_pars$dens_mth[, m] <- sample_qtls(n_iter,
                                                probs = bird_dens_dt$p,
                                                qtls = bird_dens_dt[[m]])
    }
  }



  # Turbines' proportion of time operational -----------------------------------

  # Initiate storage datasets
  sampled_pars$downtime <- sampled_pars$prop_oper_mth <-
    data.matrix(
      matrix(data = 0, ncol = length(mod_mths), nrow = n_iter,
             dimnames = list(NULL, mod_mths))
    )

  # formatting
  trb_downtime_pars <- trb_downtime_pars %>%
    dplyr::mutate(month = format_months(month)) %>%
    dplyr::filter(month %in% mod_mths) %>%
    dplyr::arrange(match(month, month.abb)) %>%
    tidyr::replace_na(replace = list(sd = 0)) # i.e no stochasticity if sd = NA

  trb_wind_avbl <- trb_wind_avbl %>%
    dplyr::mutate(month = format_months(month)) %>%
    dplyr::filter(month %in% mod_mths) %>%
    dplyr::arrange(match(month, month.abb)) %>%
    tidyr::replace_na(replace = list(sd = 0)) # i.e no stochasticity if sd = NA


  for(i in 1:nrow(trb_downtime_pars)){
    # Changed from Norm to 0-bounded tNorm because downtime must be > 0
    sampled_pars$downtime[, i] <- sampler_hd(dat = trb_downtime_pars$sd[i],
                                             mode = 'rtnorm',
                                             n = n_iter,
                                             mean = trb_downtime_pars$mean[i],
                                             sd = trb_downtime_pars$sd[i],
                                             lower = 0)

    sampled_pars$prop_oper_mth[, i] <- 0.01*(trb_wind_avbl$pctg[i] -
                                               sampled_pars$downtime[, i])
  }


  # Rotation speed and blade pitch ---------------------------------------------
  if(rtn_pitch_opt == "probDist"){
    sampled_pars$rtn_speed <- sampler_hd(dat = rtn_speed_pars$sd,
                                         n = n_iter,
                                         mode = "rtnorm",
                                         mean = rtn_speed_pars$mean,
                                         sd = rtn_speed_pars$sd,
                                         lower = 0)

    sampled_pars$bld_pitch <- sampler_hd(dat = bld_pitch_pars$sd,
                                         n = n_iter,
                                         mode = "rtnorm",
                                         mean = bld_pitch_pars$mean,
                                         sd = bld_pitch_pars$sd,
                                         lower = 0)

    # convert pitch angle from degrees to radians
    sampled_pars$bld_pitch <- sampled_pars$bld_pitch * pi/180

  }else if(rtn_pitch_opt == "windSpeedReltn"){

    # BC: Taking an alternative approach here:
    #  (i) windspeed sampling - now done via a while loop to achieve the
    #         required n_iter sample size. Some steps in the previous solution were
    #         confusing (specially the re-sampling step after the rtnorm sampling)
    #         and, in edge cases (e.g. mean windspeed substantially lower than the
    #         turbine's operating threshold), one could end up bootstraping from
    #         a very small sample. Outputs from new approach will differ from
    #         previous implementation.
    #
    #  (ii) look-up sampled windspeeds on windspeed to rotation speed and
    #         pitch relationship - code further streamlined using the base function
    #         findIntervals() to find indices of intervals in look-up table comprising
    #         sampled windspeeds. Results not affected by code alterations

    # sort by wind speed
    rtn_pitch_windspd_dt <- rtn_pitch_windspd_dt[sort(rtn_pitch_windspd_dt$wind_speed), ]
    # find wind speed threshold below which blades stop rotating
    wind_thresh <- rtn_pitch_windspd_dt$wind_speed[min(which(rtn_pitch_windspd_dt$rtn_speed  != 0))]

    sampled_pars$wind_speed <- c()
    n_samp <- 0
    n_loops <- 0

    while (n_samp < n_iter) {
      ws <- sampler_hd(dat = windspd_pars$sd,
                       mode = "rtnorm",
                       n = 1,
                       mean = windspd_pars$mean,
                       sd = windspd_pars$sd,
                       lower =  0)

      if(ws > wind_thresh){
        sampled_pars$wind_speed[n_samp + 1] <- ws
        n_samp <- n_samp + 1
      }

      n_loops <- n_loops + 1
      if(n_loops > 1e5){
        stop("Unable to randomly generate required size of wind speed values greater
         than the rotation speed threshold. Please check your input parameters
         for wind speed and rotation speed")
      }
    }

    # sort look-up table by increasing windspeeds, required for findInterval()
    rtn_pitch_windspd_dt <- rtn_pitch_windspd_dt[order(rtn_pitch_windspd_dt$wind_speed), ]

    # find indices in look-up table of intervals containing sampled wind speeds
    lookup_idx <- findInterval(sampled_pars$wind_speed, rtn_pitch_windspd_dt$wind_speed)

    # subset corresponding rotation speeds and blade pitch
    sampled_pars$rtn_speed <- rtn_pitch_windspd_dt$rtn_speed[lookup_idx]
    sampled_pars$bld_pitch <- rtn_pitch_windspd_dt$bld_pitch[lookup_idx] * pi/180
  }



  ## Basic avoidance rate, for Options 1 and 2 and LAC -------------------------
  if(lrg_arr_corr|any(model_options %in% c('1', '2'))){
    sampled_pars$avoid_bsc <- sampler_hd(dat = avoid_bsc_pars$sd,
                                         mode = 'rbeta',
                                         n = n_iter,
                                         mean = avoid_bsc_pars$mean,
                                         sd = avoid_bsc_pars$sd)
  }else{
    sampled_pars$avoid_bsc <- NULL
  }

  ## Extended avoidance rate, for Options 3 and 4 ------------------------------
  if(any(model_options %in% c('3', '4'))){
    sampled_pars$avoid_ext <- sampler_hd(dat = avoid_ext_pars$sd,
                                         mode = 'rbeta',
                                         n = n_iter,
                                         mean = avoid_ext_pars$mean,
                                         sd = avoid_ext_pars$sd)
  }else{
    sampled_pars$avoid_ext <- NULL
  }


  ## Proportion at CRH, for Option 1 -------------------------------------------
  if(any(model_options == '1')){
    sampled_pars$prop_crh <- sampler_hd(dat = prop_crh_pars$sd,
                                        mode = 'rbeta',
                                        n = n_iter,
                                        mean = prop_crh_pars$mean,
                                        sd = prop_crh_pars$sd)
  }else{
    sampled_pars$prop_crh <- NULL
  }

  ## Flight height distributions, for Options 2, 3, and 4 ----------------------
  if(any(model_options %in% c('2', '3'))){
    if(!is.null(gen_fhd_boots)){
      ### A little hack in case a user wants to use a single bootstrap replicate
      ### for testing
      if(ncol(gen_fhd_boots) == 2){
        sampled_pars$gen_fhd <- data.matrix(
          gen_fhd_boots[, sample(c(2,2), n_iter, replace = TRUE)]
        )
      }else if(ncol(gen_fhd_boots)>2){
        sampled_pars$gen_fhd <- data.matrix(
          gen_fhd_boots[, sample(2:ncol(gen_fhd_boots), n_iter, replace = TRUE)]
        )
      }
    } else {
      stop("`gen_fhd_boots` argument is NULL while model options 2 and/or 3 are",
           " requested in `model_options`.\n",
           "   Dataset with bootstrap samples of flight height distributions ",
           "must be provided for model Options 2 and 3")
    }
  }else{
    sampled_pars$gen_fhd <- NULL
  }

  if(any(model_options == '4')){
    if(!is.null(site_fhd_boots)){
      sampled_pars$site_fhd <- data.matrix(
        site_fhd_boots[, sample(2:ncol(site_fhd_boots), n_iter, replace = TRUE)]
      )
    } else {
      stop("`site_fhd_boots` argument is NULL while model options 2 and/or 3 are",
           " requested in `model_options`.\n",
           "   Dataset with bootstrap samples of flight height distributions ",
           "must be provided for model Options 2 and 3")
    }
  }else{
    sampled_pars$site_fhd <- NULL
  }

  return(sampled_pars)
}
