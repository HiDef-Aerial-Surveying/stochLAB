#' Input validator
#' @inheritParams stoch_crm
#' @inheritParams band_crm
#' @inheritParams mig_stoch_crm
#' @param fn a character string specifying the parent function whose inputs are being checked:
#' * `"scrm"`: checks [stoch_crm()] inputs
#' * `"crm"`: checks [band_crm()] inputs
#' * `"mcrm"`: checks [mig_stoch_crm()] inputs
#' @examples
#'   validate_inputs(model_options=c(1),
#'                avoid_bsc_pars=data.frame(mean=0.99,sd=0.001),
#'                prop_crh_pars=data.frame(mean=0.01,sd=0.01),
#'                air_gap_pars = data.frame(mean=21,sd=0),
#'                rtr_radius_pars = data.frame(mean=100,sd=0),
#'                bld_pitch_pars = data.frame(mean=15,sd=0),
#'                rtn_pitch_opt = "probDist",
#'                rtn_speed_pars = data.frame(mean=14,sd=5),
#'                out_period = "months",
#'                lrg_arr_corr = TRUE,
#'                fn="scrm")
#' @export
validate_inputs <- function(model_options,
                            n_iter = NULL,
                            flt_speed_pars = NULL,
                            flight_speed = NULL,
                            body_lt_pars = NULL,
                            body_lt = NULL,
                            wing_span_pars = NULL,
                            wing_span = NULL,
                            avoid_bsc_pars = NULL,
                            avoid_rt_basic = NULL,
                            avoid_ext_pars = NULL,
                            avoid_rt_ext = NULL,
                            noct_act_pars = NULL,
                            noct_activity = NULL,
                            prop_crh_pars = NULL,
                            bird_dens_opt = NULL,
                            bird_dens_dt = NULL,
                            chord_prof = NULL,
                            dens_month = NULL,
                            turb_oper_month = NULL,
                            flight_type = NULL,
                            prop_upwind = NULL,
                            gen_fhd_boots = NULL,
                            site_fhd_boots = NULL,
                            n_blades = NULL,
                            air_gap_pars = NULL,
                            rtr_radius_pars = NULL,
                            rotor_radius = NULL,
                            blade_width = NULL,
                            blade_pitch = NULL,
                            hub_height = NULL,
                            bld_width_pars = NULL,
                            rtn_pitch_opt = NULL,
                            bld_pitch_pars = NULL,
                            rtn_speed_pars = NULL,
                            rotor_speed = NULL,
                            n_turbines = NULL,
                            windspd_pars = NULL,
                            rtn_pitch_windspd_dt = NULL,
                            trb_wind_avbl = NULL,
                            trb_downtime_pars = NULL,
                            wf_n_trbs = NULL,
                            wf_width = NULL,
                            wf_latitude = NULL,
                            tidal_offset = NULL,
                            gen_fhd = NULL,
                            site_fhd = NULL,
                            lrg_arr_corr = NULL,
                            xinc = NULL,
                            yinc = NULL,
                            seed = NULL,
                            verbose = NULL,
                            out_format = NULL,
                            out_sampled_pars = NULL,
                            out_period = NULL,
                            season_specs = NULL,
                            popn_estim_pars = NULL,
                            fn = "scrm"){

  # Non-specific CRM function inputs --------------------------------------------------------

  # CRM modelling options
  val_model_opts(model_options)

  # number of simulations
  if(!is.null(n_iter)) val_constant(n_iter, min = 1, check_whole = TRUE)

  # Flight type
  if(!is.null(flight_type)) val_option(flight_type, valid_opts = c("gliding", "flapping"))


  # Blade chord profile
  if(!is.null(chord_prof)) {
    val_pars_df(chord_prof,
                dt_type = "chord_prof",
                exp_colnames = c("pp_radius", "chord"),
                single_row = FALSE)
  }

  # Turbine features
  if(!is.null(prop_upwind)) val_constant(prop_upwind, min = 0, max = 1)
  if(!is.null(n_blades)) val_constant(n_blades, min = 1, check_whole = TRUE)

  # Integration increments
  if(!is.null(yinc)) val_constant(yinc, min = 0.01)
  if(!is.null(xinc)) val_constant(xinc, min = 0.01)

  # wind farm features
  if(!is.null(wf_width)) val_constant(wf_width, min = 1)
  if(!is.null(wf_latitude)) val_constant(wf_latitude, min = -90, max = 90)
  if(!is.null(tidal_offset)) val_constant(tidal_offset)
  if(!is.null(lrg_arr_corr)) val_logical(lrg_arr_corr)



  # CRM specific inputs --------------------------------------------------------
  if(fn == "crm"){

    if(!is.null(flight_speed)) val_constant(flight_speed, 0)
    if(!is.null(body_lt)) val_constant(body_lt, 0)
    if(!is.null(wing_span)) val_constant(wing_span, 0)
    if(!is.null(avoid_rt_basic)) val_constant(avoid_rt_basic, 0, 1)
    if(!is.null(avoid_rt_ext)) val_constant(avoid_rt_ext, 0, 1)
    if(!is.null(noct_activity)) val_constant(noct_activity, 0, 1)
    if(!is.null(rotor_speed)) val_constant(rotor_speed, 0)
    if(!is.null(rotor_radius)) val_constant(rotor_radius, 0)
    if(!is.null(blade_width)) val_constant(blade_width, 0)
    if(!is.null(blade_pitch)) val_constant(blade_pitch, 0)
    if(!is.null(hub_height)) val_constant(hub_height, 0)
    if(!is.null(n_turbines)) val_constant(n_turbines, 1, check_whole = TRUE)



    ## Density data
    if(!is.null(dens_month)) val_df_columns(dens_month,
                                            df_name = "dens_month",
                                            exp_colnames = c("month", "dens"))

    ## FHD
    if(!is.null(gen_fhd)){
      val_pars_df(gen_fhd, dt_type = "fhd", single_row = FALSE,
                  exp_colnames = c("height", "prop"))

      check_fhd_vs_maxtip(gen_fhd, tidal_offset, hub_hght = hub_height,
                          rtr_rad = rotor_radius, fn = fn)
    }

    if(!is.null(site_fhd)){
      val_pars_df(site_fhd, dt_type = "fhd", single_row = FALSE,
                  exp_colnames = c("height", "prop"))

      check_fhd_vs_maxtip(site_fhd, tidal_offset, hub_hght = hub_height,
                          rtr_rad = rotor_radius, fn = fn)
    }

    # Turbine data
    if(!is.null(turb_oper_month)) val_prop_oper(turb_oper_month)
  }



  # SCRM specific inputs --------------------------------------------------------
  if(fn == "scrm"){

    # ------ Bird features ------------
    ## probability distribution parameters
    if(!is.null(flt_speed_pars)) val_pars_df(flt_speed_pars)
    if(!is.null(body_lt_pars)) val_pars_df(body_lt_pars)
    if(!is.null(wing_span_pars)) val_pars_df(wing_span_pars)
    if(!is.null(avoid_bsc_pars)) val_pars_df(avoid_bsc_pars)
    if(!is.null(avoid_ext_pars)) val_pars_df(avoid_ext_pars)
    if(!is.null(noct_act_pars)) val_pars_df(noct_act_pars)
    if(!is.null(prop_crh_pars)) val_pars_df(prop_crh_pars)

    ## Others
    if(!is.null(flight_type)) val_option(flight_type, valid_opts = c("gliding", "flapping"))


    # ----- Bird densities ---------------
    ## Probability distribution parameters
    if(!is.null(bird_dens_opt)){
      val_option(bird_dens_opt, valid_opts = c("tnorm", "resample", "qtiles"))

      if(!is.null(bird_dens_dt)){
        if(bird_dens_opt == "tnorm"){
          val_pars_df(bird_dens_dt,
                      dt_type = "dstn_pars",
                      exp_colnames = c("month", "mean", "sd"),
                      single_row = FALSE)
        }
        if(bird_dens_opt == "resample"){
          val_pars_df(bird_dens_dt,
                      dt_type = "samples",
                      single_row = FALSE)
        }
        if(bird_dens_opt == "qtiles"){
          val_pars_df(bird_dens_dt,
                      dt_type = "qtls",
                      single_row = FALSE)
        }
      }
    }


    # ----- Flight Height Distribution ----------
    ## Bootstrap samples
    if(!is.null(gen_fhd_boots)){
      val_pars_df(gen_fhd_boots, dt_type = "fhd", single_row = FALSE,
                  exp_colnames = "height")
    }

    if(!is.null(site_fhd_boots)){
      val_pars_df(site_fhd_boots, dt_type = "fhd", single_row = FALSE,
                  exp_colnames = "height")
    }

    # ---- Turbine features --------
    if(!is.null(air_gap_pars)) val_pars_df(air_gap_pars)
    if(!is.null(rtr_radius_pars)) val_pars_df(rtr_radius_pars)
    if(!is.null(bld_width_pars)) val_pars_df(bld_width_pars)

    if(!is.null(rtn_pitch_opt)){
      val_option(rtn_pitch_opt, valid_opts = c("probDist", "windSpeedReltn"))

      if(rtn_pitch_opt == "probDist"){
        if(!is.null(bld_pitch_pars)) val_pars_df(bld_pitch_pars)
        if(!is.null(rtn_speed_pars)) val_pars_df(rtn_speed_pars)
      }

      if(rtn_pitch_opt == "windSpeedReltn"){
        if(!is.null(windspd_pars)) val_pars_df(windspd_pars)

        if(!is.null(rtn_pitch_windspd_dt)){
          val_df_columns(df = rtn_pitch_windspd_dt, df_name = "rtn_pitch_windspd_dt",
                         exp_colnames = c("wind_speed", "rtn_speed", "bld_pitch"))
        }
      }
    }

    if(!is.null(trb_wind_avbl)){
      val_pars_df(trb_wind_avbl,
                  dt_type = "dstn_pars",
                  exp_colnames = c("month", "pctg"),
                  single_row = FALSE)
    }

    if(!is.null(trb_downtime_pars)){
      val_pars_df(trb_downtime_pars,
                  dt_type = "dstn_pars",
                  exp_colnames = c("month", "mean", "sd"),
                  single_row = FALSE)
    }

    # ---- Windfarm features --------
    if(!is.null(wf_n_trbs)) val_constant(wf_n_trbs, min = 1, check_whole = TRUE)

    # ---- Simulation and output options -----------
    if(!is.null(verbose)) val_logical(verbose)
    if(!is.null(seed)) val_constant(seed, min = 1, check_whole = TRUE)
    if(!is.null(out_format)){
      val_option(out_format, valid_opts = c("draws", "summaries"))
    }
    if(!is.null(out_sampled_pars)) val_logical(out_sampled_pars)
    if(!is.null(out_period)){
      val_option(out_period, valid_opts = c("months", "seasons", "annum"))
    }

    if(!is.null(season_specs)){
      val_df_columns(df = season_specs, df_name = "season_specs",
                     exp_colnames = c("season_id", "start_month", "end_month"))
    }

    # ------ Check consistency between dependent inputs -----------------------
    if (any(model_options == '1')) {
      if (is.null(prop_crh_pars)) {
        rlang::abort(
          message = c("Missing argument `prop_crh_surv` with no default:",
                      x = "`prop_crh_surv` must be provided if `model_options` comprise '1'",
                      i = "Proportion of flights at collision risk is required under Model Option 1.")
        )
      }}

    if (lrg_arr_corr|any(model_options %in% c('1', '2'))) {
      if (is.null(avoid_bsc_pars)) {
        rlang::abort(
          message = c(x = "Missing argument `avoid_rt_basic` with no default:",
                      x = "`avoid_rt_basic` required for `model_options` '1' and/or '2', or if `lrg_arr_corr == TRUE`.",
                      i = "Calculations under the Basic Model underlying Options 1 and 2, ",
                      i = "and the large array correction, expect a specific value of avoidance rate.")
        )
      }}

    if (any(model_options %in% c('3', '4'))) {
      if (is.null(avoid_ext_pars)) {
        rlang::abort(
          message = c(x = "Missing argument `avoid_rt_ext` with no default:",
                      x = "`avoid_rt_ext` required for `model_options` '3' and/or '4'.",
                      i = "Calculations under Extended Basic Model underlying Options 3 and 4, ",
                      i = "expect a specific value of avoidance rate.")
        )
      }}

    # calculations required for part of the next checks
    air_gap_pct99 <- qtnorm(0.999, mean = air_gap_pars$mean, sd = air_gap_pars$sd)
    rtr_rad_pct99 <- qtnorm(0.999, mean = rtr_radius_pars$mean, sd = rtr_radius_pars$sd)

    if (any(model_options %in% c('2', '3'))) {
      if (is.null(gen_fhd_boots)) {
        rlang::abort(
          message = c(x = "Missing argument `gen_fhd_boots` with no default:",
                      i = "`gen_fhd_boots` required for `model_options` '2' and/or '3'.",
                      i = "Calculations under Model Options 2 and 3 require a generic FHD.")
        )
      }else{ # Check consistency between FHD heights and maximum tip height
          check_fhd_vs_maxtip(gen_fhd_boots, tidal_offset, air_gap = air_gap_pct99,
                              rtr_rad = rtr_rad_pct99, fn = fn)
      }
    }

    if (any(model_options == '4')) {
      if (is.null(site_fhd_boots)) {
        rlang::abort(
          message = c(x = "Missing argument `site_fhd_boots` with no default:",
                      i = "`site_fhd_boots` required if value '4' is comprised in `model_options.",
                      i = " Option 4 require a site-specific FHD based on survey data.")
        )
      }else{ # Check consistency between FHD heights and maximum tip height
        check_fhd_vs_maxtip(site_fhd_boots, tidal_offset, air_gap = air_gap_pct99,
                            rtr_rad = rtr_rad_pct99, fn = fn)
      }
    }

    if(rtn_pitch_opt == "probDist"){
      if(is.null(bld_pitch_pars)){
        rlang::abort(
          message = c(x = "Missing argument `bld_pitch_pars` with no default:",
                      i = "`bld_pitch_pars` required if `rtn_pitch_opt == probDist`.")
        )
      }
      if(is.null(rtn_speed_pars)){
        rlang::abort(
          message = c(x = "Missing argument `rtn_speed_pars` with no default:",
                      i = "`rtn_speed_pars` required if `rtn_pitch_opt == probDist`.")
        )
      }
    }

    if(rtn_pitch_opt == "windSpeedReltn"){
      if(is.null(windspd_pars)){
        rlang::abort(
          message = c(x = "Missing argument `windspd_pars` with no default:",
                      i = "`windspd_pars` required if `rtn_pitch_opt == windSpeedReltn`.")
        )
      }
      if(is.null(rtn_pitch_windspd_dt)){
        rlang::abort(
          message = c(x = "Missing argument `rtn_pitch_windspd_dt` with no default:",
                      i = "`rtn_pitch_windspd_dt` required if `rtn_pitch_opt == windSpeedReltn`.")
        )
      }
    }

    if(out_period == "seasons"){
      if(is.null(season_specs)){
        rlang::abort(
          message = c(x = "Missing argument `season_specs` with no default:",
                      i = "`season_specs` required if `out_period == seasons`.")
        )
      }
    }
  }


  # MCRM specific inputs ----------------------------------------------------
  if(fn == "mcrm"){

    # ------ Bird features ------------
    ## probability distribution parameters
    if(!is.null(wing_span_pars)) val_pars_df(wing_span_pars)
    if(!is.null(flt_speed_pars)) val_pars_df(flt_speed_pars)
    if(!is.null(body_lt_pars)) val_pars_df(body_lt_pars)
    if(!is.null(prop_crh_pars)) val_pars_df(prop_crh_pars)
    if(!is.null(avoid_bsc_pars)) val_pars_df(avoid_bsc_pars)
    if(!is.null(popn_estim_pars)) val_pars_df(popn_estim_pars)
    if(!is.null(flight_type)) val_option(flight_type, valid_opts = c("gliding", "flapping"))

    # ---- Turbine features --------
    if(!is.null(rtr_radius_pars)) val_pars_df(rtr_radius_pars)
    if(!is.null(bld_width_pars)) val_pars_df(bld_width_pars)
    if(!is.null(rtn_speed_pars)) val_pars_df(rtn_speed_pars)
    if(!is.null(bld_pitch_pars)) val_pars_df(bld_pitch_pars)


    if(!is.null(n_turbines)) val_constant(n_turbines,min=1,check_whole=TRUE)

    if(!is.null(trb_wind_avbl)){
      val_pars_df(trb_wind_avbl,
                  dt_type = "dstn_pars",
                  exp_colnames = c("month", "pctg"),
                  single_row = FALSE)
    }

    if(!is.null(trb_downtime_pars)){
      val_pars_df(trb_downtime_pars,
                  dt_type = "dstn_pars",
                  exp_colnames = c("month", "mean", "sd"),
                  single_row = FALSE)
    }
    if(!is.null(verbose)) val_logical(verbose)
    if(!is.null(seed)) val_constant(seed, min = 1, check_whole = TRUE)

  }
}




# ------------------------------------------------------------------------------
check_fhd_vs_maxtip <- function(fhd, tid_off, air_gap = NULL, hub_hght = NULL,
                                rtr_rad, fn){
  fhd_name <- deparse(substitute(fhd))


  if(fn == "scrm"){
    tip_max_height <- tid_off + air_gap + (2*rtr_rad)

    if(tip_max_height > max(fhd$height)){
      rlang::abort(
        message = c("Flight height distribution must cover the maximum blade tip height:",
                    x = paste0("Maximum value of column `height` in supplied `", fhd_name,
                               "`is lower than possible maximum tip height."),
                    i = "Max tip height based on supplied `air_gap_pars`, `rtr_radius_pars` and `tidal_offset` parameter values."))
    }
  }
  if(fn == "crm"){
    tip_max_height <- tid_off + hub_hght + rtr_rad

    if(tip_max_height > max(fhd$height)){
      rlang::abort(
        message = c("Flight height distribution must cover the maximum blade tip height:",
                    x = paste0("Maximum value of column `height` in supplied `", fhd_name,
                               "`is lower than possible maximum tip height."),
                    i = "Max tip height based on supplied `hub_height`, `rotor_radius` and `tidal_offset` values"))
    }
  }
}


# ------------------------------------------------------------------------------
val_logical <- function(x){
  obj_name <- deparse(substitute(x))

  if(!is.logical(x)){
    rlang::abort(paste0("`", obj_name,"` must be logical value."))
  }
}


# ------------------------------------------------------------------------------
val_constant <- function(x, min = -Inf, max = Inf, check_whole = FALSE){

  obj_name <- deparse(substitute(x))

  if(!is.numeric(x)){
    rlang::abort(paste0("`", obj_name,"` must be a numeric value."))
  }else if (length(x) != 1) {
    rlang::abort(paste0("`", obj_name, "` must have length of 1."))
  }else{
    if(x < min | x > max){
      if(min == -Inf){
        rlang::abort(paste0("`", obj_name, "` must be <= ", max, "."))
      }else if(max == Inf){
        rlang::abort(paste0("`", obj_name, "` must be >= ", min, "."))
      }else{
        rlang::abort(paste0("`", obj_name, "` must be bounded between ",
                            min, " and ", max, "."))
      }
    }
    if(check_whole){
      if(!is.wholenumber(x)){
        rlang::abort(paste0("`", obj_name,"` must be a whole number."))
      }
    }
  }

}



# ------------------------------------------------------------------------------
val_model_opts <- function(model_options){

  valid_opts <- c('1', '2', '3', '4')

  if(is.data.frame(model_options)){
    rlang::abort(
      message = c("`model_options` should be a character vector.",
                  x = "You provided a data frame.")
    )
  } else if(length(model_options) == 0){
    rlang::abort(
      message = c("`model_options` must have length 1 or greater.",
                  x = "You have supplied an object of length 0.")
    )
  } else if(all(model_options %nin% valid_opts)) {

    err_msg <- c("`model_options` must contain at least one of the following: '1', '2', '3' and/or '4'.",
                 x = paste0("You've supplied the value(s) '",
                            glue::glue_collapse(model_options, sep = "', '", last = "' and '"),
                            "'"))
    rlang::abort(err_msg)

  } else if(any(model_options %nin% valid_opts)){

    non_valid_vals <- model_options[model_options %nin% valid_opts]
    info_msg <- paste0("Value(s) '", glue::glue_collapse(non_valid_vals, sep = "', '", last = "' and '"),
                       "' supplied to `model_options` will be ignored.\n")
    rlang::inform(
      rlang::format_error_bullets(c(i = info_msg))
    )
  }
}







# ------------------------------------------------------------------------------
val_pars_df <- function(df,
                        dt_type = "dstn_pars",
                        exp_colnames = c("mean", "sd"),
                        single_row = TRUE){

  df_name <- deparse(substitute(df))

  if(!is.data.frame(df)){ # is df?

    rlang::abort(paste0("`", df_name, "` must be a data frame."))

  } else {

    if (dt_type == "dstn_pars") {

      val_df_columns(df, df_name, exp_colnames)

      if (single_row) {
        numrows <- nrow(df)
        if(numrows > 1){
          rlang::abort(
            message = c(paste0("`", df_name, "` must contain a single row:"),
                        x = paste0("You've supplied a data frame with ",
                                   numrows, " rows.")))
        }
      }
    }

    if(dt_type %in% c("samples", "qtls", "fhd", "chord_prof")){ # check all columns numeric
      if(any(!apply(df, 2, is.numeric))){
        rlang::abort(
          message = c(paste0("All columns in `", df_name, "` must be numeric:"),
                      i = "You have supplied at least one non-numeric column."))
      }
    }

    if(dt_type == "samples"){
      val_months(names(df),
                 err_msg_header = paste0("Non-valid month-named columns in ",
                                         df_name,":"))
    }

    if(dt_type == "qtls"){
      val_df_columns(df, df_name, exp_colnames = "p")
      qtls_months <- colnames(df)[colnames(df) != "p"]
      val_months(qtls_months,
                 err_msg_header = paste0("Non-valid month-named columns in ",
                                         df_name,":"))
    }

    if(dt_type == "fhd"){
      val_df_columns(df, df_name, exp_colnames)
      if(df$height[1] != 0){
        rlang::abort(
          message = c(paste0("First value of column `height` in `", df_name,
                             "` must take the value 0."),
                      i = "Flight height distribution data must start at band 0-1 metres.",
                      i = "Height bands expected to be referenced by their lower bound, e.g. 0 for '0-1m' height band."))
      }

      if(names(df)[1] != "height"){
        rlang::abort(paste0("First column of `", df_name, "` must be named `height`."))
      }

      if(!pracma::is.sorted(df$height)){
        rlang::abort(message = paste0("Column `height` of `", df_name, "` must be sorted."))
      }

      if(any(diff(df$height) != 1)){
        rlang::abort(
          message = c("Height bands in flight height distributions must be of size 1-metre:",
                      x = paste0("Values in column `height` of `",
                                 df_name, "` don't increase by increments of 1."))
        )
      }
    }

    if(dt_type == "chord_prof"){
      val_df_columns(df, df_name, exp_colnames)
      rad_incs <- round(diff(df$pp_radius), 3)
      if(length(unique(rad_incs)) > 1){
        rlang::abort(
          message = c(paste0("Values in column `pp_radius` in `", df_name,
                             "` must be equidistant:"),
                      i = "Please supply pp_radius values with a constant increment.",
                      i =  "Try '?get_avg_prob_collision' for further help."))
      }
      df_nrow <- nrow(df)
      if(df$pp_radius[1] != 0 | df$pp_radius[df_nrow] != 1){
        rlang::abort(
          message = c(paste0("Values in column `pp_radius` in `", df_name,
                             "` must start at 0 AND end at 1:"),
                      i =  "Try '?get_avg_prob_collision' for further help."))
      }
    }
  }
}



# ------------------------------------------------------------------------------
# Checks presence and validity of expected columns in data frame
val_df_columns <- function(df,
                           df_name,
                           exp_colnames = c("mean", "sd")){

  if(!is.data.frame(df)){
    rlang::abort(paste0("`", df_name, "` must be a data frame."))
  }else{
    for(colname in exp_colnames){

      if(colname %nin% names(df)){ # presence check
        rlang::abort(paste0("Can't find column `", colname, "` in `", df_name, "`."))
      }else{
        if(any(is.na(df[[colname]]))){
          rlang::abort(
            message = c("Parameter inputs can't hold missing values.",
                        x = paste0("Column `", colname, "` in `", df_name,
                                   "` contains NAs.")))
        }
        if(colname %nin% c( "month", "season_id", "start_month", "end_month")){ # for non-character columns
          if(!is.numeric(df[[colname]])){   # check if not numeric
            rlang::abort(paste0("Column `", colname, "` in `", df_name,
                                "` must be numeric."))
          }else{  # further specific checks on numeric columns
            if(colname == "sd"){
              if(any(df[[colname]] < 0)){
                rlang::abort(message = c(paste0("Column `", colname, "` in `", df_name,
                                                "` can't have negative values:"),
                                         i = "Standard deviations must be positive!"))
              }
            }
            if(colname %in% c("p", "prob", "pp_radius", "chord")){
              if(any(df[[colname]] < 0 | df[[colname]] > 1)){
                rlang::abort(
                  message = c(paste0("Values in column `", colname, "` in `",
                                     df_name, "` must be bounded between 0 and 1:"),
                              i = "Please supply appropriate probabilities (or proportions)."))
              }
            }
          }
        }else{ # for character columns, further specific checks

          invalid_month_header <- paste0("column `", colname, "` in `", df_name,
                                         "` must contain valid month names:")
          if(colname == "month"){
            val_months(df[[colname]], err_msg_header = invalid_month_header)
          }

          if(colname %in% c("start_month", "end_month")){
            val_months(df[[colname]], err_msg_header = invalid_month_header,
                       check_duplicated = FALSE)
          }

          if(colname == "season_id"){
            if(any(duplicated(df[[colname]]))){
              rlang::abort(
                message = c(paste0("Values contained in column `", colname, "` in `", df_name,
                                   "` must be unique."),
                            x = "You have supllied duplicated ids.")
              )
            }
          }
        }
      }
    }
  }
}



# ------------------------------------------------------------------------------
val_months <- function(m, err_msg_header, check_duplicated = TRUE){

  if(!is.character(m)){
    rlang::abort(
      message = c(err_msg_header,
                  x = paste0("You have supplied a ", class(m), " vector."),
                  i = "Months must be specified as character strings.")
    )
  }else{
    m <- format_months(m)

    if(any(m %nin% month.abb)){
      rlang::abort(
        message = c(err_msg_header,
                    i = "Please specify months by their English names (or 3-letter abbreviation).")
        )
    }

    if(check_duplicated){
      if(any(duplicated(m))){
        rlang::abort(
          message = c(err_msg_header,
                      x = "You have supllied duplicated months.",
                      i = "Specified months must be unique.")
        )
      }
    }
  }
}



# ------------------------------------------------------------------------------
val_option <- function(opt, valid_opts){

  obj_name <- deparse(substitute(opt))

  if(length(opt) != 1){
    rlang::abort(paste0("`", obj_name, "` must be of length 1."))
  }else if(any(opt %nin% valid_opts)){
    rlang::abort(
      message = c(paste0("'", opt, "' is an invalid choice for `", obj_name, "`:"),
                  i = paste0("Valid options are: '",
                             glue::glue_collapse(valid_opts, sep = "', '",
                                                 last = "' or '"),
                             "'.")))
  }
}




# ------------------------------------------------------------------------------
val_prop_oper <- function(turb_oper_month){


  if (any(c("month", "prop_oper") %nin% names(turb_oper_month))) {
    stop("Invalid argument: 'turb_oper_month' missing column(s) named 'month'
         and/or 'prop_oper'")
  }

  if(any(duplicated(turb_oper_month$month))){
    stop("Invalid argument: column 'month' in 'turb_oper_month' contains
         duplicated entries. Only one entry per month expected")
  }

}



# # ------------------------------------------------------------------------------
# val_bird_dens <- function(dens_month){
#   if(any(c("month", "dens") %nin% names(dens_month))){
#     stop("Invalid argument: 'dens_month' missing column(s) named 'month'
#          and/or 'dens'")
#   }
#
#   if(any(duplicated(dens_month$month))){
#     stop("Invalid argument: column 'month' in 'dens_month' contains
#          duplicated entries. Only one entry per month expected")
#   }
# }
#
# # ------------------------------------------------------------------------------
# val_chord_prof <- function(chord_prof){
#
#   if(any(c("pp_radius", "chord") %nin% names(chord_prof))){
#     stop("Invalid 'chord_prof' argument: missing column(s) named 'pp_radius'
#          and/or 'chord' Try e.g. '?get_avg_prob_collision' for further help.")
#   }
#
#   rad_incs <- round(diff(chord_prof$pp_radius), 3)
#   if(length(unique(rad_incs)) > 1){
#     stop("Invalid 'chord_prof' argument: all values in 'pp_radius' column must be
#          equidistant. Please provide pp_radius values with a constant increment.
#          Try e.g. '?get_avg_prob_collision' for further help.")
#   }
#
#   if(min(chord_prof$pp_radius) != 0 | max(chord_prof$pp_radius) != 1){
#     stop("Invalid 'chord_prof' argument: values in 'pp_radius' column must be
#          within the range [0, 1]. Please include a relative chord width at 0 AND
#          1 (relative) rotor radius. Try '?get_avg_prob_collision' for
#          further help")
#   }
# }
#
# # ------------------------------------------------------------------------------
# val_fhd <- function(fhd){
#
#   obj_name <- deparse(substitute(fhd))
#
#   if (any(c("height", "prop") %nin% names(fhd))) {
#     stop(paste0("Invalid argument: '", obj_name, "' missing column(s) named 'height'
#          and/or 'prop'"))
#   }
#
#   if(fhd$height[1] != 0){
#     stop(paste0("Invalid argument: flight distribution data in '", obj_name, "' must
#                 start at height == 0"))
#   }
#
# }
# # ------------------------------------------------------------------------------
# val_n_iter <- function(n_iter){
#   if (length(n_iter) != 1) {
#     rlang::abort("`n_iter` must have length 1.")
#   } else {
#     if (!is.numeric(n_iter)){
#       rlang::abort("`n_iter` must be numeric.")
#     } else {
#       if(!is.wholenumber(n_iter)|n_iter < 0){
#         rlang::abort("`n_iter` must be a positive integer.")
#       }
#     }
#   }
# }

