#' Sampling function for a single turbine in the mCRM
#'
#' Samples and aggregates appropriate data for a single wind turbine
#'
#'
#' @param bld_pitch_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the blade pitch angle,
#'   i.e. the angle between the blade surface and the rotor plane,
#'   in degrees. Assumed to follow a *tnorm-lw0* distribution.
#' @param rtr_radius_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the radius of the rotor, in metres.
#'   Assumed to follow a *tnorm-lw0* distribution.
#' @param bld_width_pars A single row data frame with columns `mean` and `sd`,
#'   the mean and standard deviation of the maximum blade width, in metres.
#'   Assumed to be *tnorm-lw0* distribution.
#'
#' @param season_specs A data frame
#'   defining the seasons for aggregating over collision estimates. It must
#'   comprise the following columns:
#'   * `season_id`, (unique) season identifier,
#'   * `start_month`, name of the season's first month,
#'   * `end_month`, name of the season's last month.
#' @param trb_wind_avbl A data frame with the monthly estimates of operational
#'   wind availability. It must contain the columns:
#'   * `month`, (unique) month names,
#'   * `pctg`, the percentage of time wind conditions allow for turbine operation
#'   per month.
#' @param trb_downtime_pars A data frame with monthly estimates of maintenance
#'   downtime, assumed to follow a *tnorm-lw0* distribution. It
#'   must contain the following columns:
#'   * `month`, (unique) month names,
#'   * `mean`, numeric, the mean percentage of time in each month when turbines
#'   are not operating due to maintenance,
#'   * `sd`, the standard deviation of monthly maintenance downtime.
#' @param n_iter An integer value. The number of samples to generate
#'
#' @return A data frame of all the information sampled for the turbine with nrow = n_iter
#' @export

sample_turbine_mCRM <- function(rtn_speed_pars,
                                bld_pitch_pars,
                                rtr_radius_pars,
                                bld_width_pars,
                                season_specs,
                                n_iter = 10,
                                trb_wind_avbl,
                                trb_downtime_pars){

  ## Create an empty dataframe to store information
  sampledTurbine = data.frame(matrix(data = 0, ncol = 4, nrow = n_iter))
  names(sampledTurbine) = c("RotorRadius", "BladeWidth", "RotorSpeed", "Pitch")

  rotorSpeed <- numeric()
  rotorPitch <- numeric()

  sampledTurbine$RotorSpeed<- rtnorm(n_iter, rtn_speed_pars$mean, rtn_speed_pars$sd, lower = 0)
  sampledTurbine$Pitch<- rtnorm(n_iter, bld_pitch_pars$mean, bld_pitch_pars$sd, lower = 0)
  #### Transform Pitch from degrees to radians, needed for Collision Risk Sheet
  sampledTurbine$Pitch = sampledTurbine$Pitch*pi / 180

  # Radius  -----------------------------------------------------------------
  sampledTurbine$RotorRadius <- rep(rtr_radius_pars$mean,n_iter)

  # Blade width -------------------------------------------------------------
  sampledTurbine$BladeWidth <- rep(bld_width_pars$mean,n_iter)


  for(bp in 1:nrow(season_specs)){
    if(!is.na(season_specs$start_month[bp])){
      ## Get wind availability for all months in the season
      windavsamp <- trb_wind_avbl[which(month.abb == season_specs$start_month[bp]):which(month.abb == season_specs$end_month[bp]),]
      ## Get downtime for all months in the season
      dwntmsamp <- trb_downtime_pars[which(month.abb == season_specs$start_month[bp]):which(month.abb == season_specs$end_month[bp]),]
      ## Calculate pooled standard deviation across the months
      pooled_sd <- sqrt(mean(sapply(1:nrow(dwntmsamp),function(x){
        va <- dwntmsamp$sd[x]^2
      })))
      ## Calculate mean downtime
      mnDT <- mean(dwntmsamp$mean)
      ## Calculate mean operational time
      mnOpTi <- mean(windavsamp$pctg)
      downtimenm <- paste0(season_specs$season_id[bp],"_DT")
      Opttimenm <- paste0(season_specs$season_id[bp],"_OT")
      sampledTurbine[downtimenm] <- rtnorm(n_iter, mnDT, pooled_sd, lower = 0)/100
      sampledTurbine[Opttimenm] <- rep(mnOpTi,n_iter)/100
    }
  }

  return(sampledTurbine)
}
