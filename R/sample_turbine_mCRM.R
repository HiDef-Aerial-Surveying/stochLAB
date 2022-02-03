#' Sampling function for a single turbine in the mCRM
#'
#' Samples and aggregates appropriate data for a single wind turbine
#'
#' @param TurbineData A data frame. The Turbine data formatted as per the "TurbineData" data object
#' @param BirdData A data frame. The bird data for getting migratory seasons
#' @param iter An integer value. The number of samples to generate
#' @return A data frame of all the information sampled for the turbine with nrow = iter
#' @import foreach
#' @importFrom msm rtnorm
#' @importFrom dplyr select
#' @export

sample_turbine_mCRM <- function(rtn_speed_pars,
                                bld_pitch_pars,
                                rtr_radius_pars,
                                bld_width_pars,
                                season_specs,
                                n_iter = 10,
                                windavb,
                                dwntm){

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
      windavsamp <- windavb[which(month.abb == season_specs$start_month[bp]):which(month.abb == season_specs$end_month[bp]),]
      ## Get downtime for all months in the season
      dwntmsamp <- dwntm[which(month.abb == season_specs$start_month[bp]):which(month.abb == season_specs$end_month[bp]),]
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
