#' Sampling function for a single turbine in the mCRM
#'
#' Samples and aggregates appropriate data for a single wind turbine
#'
#' @param TurbineData A data frame. The Turbine data formatted as per the "TurbineData" data object
#' @param BirdData A data frame. The bird data for getting migratory seasons
#' @param iter An integer value. The number of samples to generate
#' @return A data frame of all the information sampled for the turbine with nrow = iter
#' @importFrom dplyr select
#' @export

sample_turbine_mCRM <- function(TurbineData,BirdData,iter){

  ### CREATE TURBINE DATA FRAME###
  sampledTurbine = data.frame(matrix(data = 0, ncol = 4, nrow = iter))
  names(sampledTurbine) = c("RotorRadius", "BladeWidth", "RotorSpeed", "Pitch")

  rotorSpeed <- numeric()
  rotorPitch <- numeric()

  sampledTurbine$RotorSpeed<- rtnorm(iter, TurbineData$RotationSpeed, TurbineData$RotationSpeedSD, lower = 0)
  sampledTurbine$Pitch<- rtnorm(iter, TurbineData$BladePitch, TurbineData$BladePitchSD, lower = 0)
  #### Transform Pitch from degrees to radians, needed for Collision Risk Sheet
  sampledTurbine$Pitch = sampledTurbine$Pitch*pi / 180


  # Radius  -----------------------------------------------------------------
  sampledTurbine$RotorRadius <- rep(TurbineData$Rotorradius,iter)

  # Blade width -------------------------------------------------------------
  sampledTurbine$BladeWidth <- rep(TurbineData$Bladewidth,iter)


  for(bp in c('PrBMigration','PoBMigration','Omigration')){
    if(BirdData[bp][[1]] != "NA"){
      mnths <- get_months(c(BirdData[bp])[[1]],month.abb)

      WTopTime <- foreach(currentMonth=mnths,.combine='rbind')%do%{
        DTMean <- data.frame(TurbineData %>% dplyr::select(contains(currentMonth)) %>% dplyr::select(contains('mean')))[1,1]
        DTSD <- data.frame(TurbineData %>% dplyr::select(contains(currentMonth)) %>% dplyr::select(contains('SD')))[1,1]
        OpTi <- data.frame(TurbineData %>% dplyr::select(contains(currentMonth)) %>% dplyr::select(-contains('SD'), -contains('Mean')))[1,1]
        return(data.frame(DTMean,DTSD,OpTi))
      }

      ### Calculate means
      pooled_sd <- sqrt(mean(sapply(1:nrow(WTopTime),function(x){
        va <- WTopTime$DTSD[x]^2
      })))
      mnDT <- mean(WTopTime$DTMean)
      mnOpTi <- mean(WTopTime$OpTi)
      downtimenm <- paste0(bp,"_DT")
      Opttimenm <- paste0(bp,"_OT")

      sampledTurbine[downtimenm] <- rtnorm(iter, TurbineData$RotationSpeed, TurbineData$RotationSpeedSD, lower = 0)/100
      sampledTurbine[Opttimenm] <- rep(mnOpTi,iter)/100
    }

  }
  return(sampledTurbine)
}
