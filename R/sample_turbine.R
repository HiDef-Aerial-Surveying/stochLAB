#' Sampling function for a single turbine
#'
#' Samples and aggregates appropriate data for a single wind turbine
#'
#' @param TurbineData A data frame. The Turbine data formatted as per the "TurbineData" data object
#' @param windSpeedMean A numeric value. The mean wind speed in km/hr
#' @param windSpeedSD A numeric value. The SD of the wind speed in km/hr
#' @param windData A data frame.  The wind speed x rotation speed x blade pitch data table
#' @param windThreshold A numeric value. The minimum wind speed required for the blades to turn
#' @param iter An integer value. The number of samples to generate
#' @return A data frame of all the information sampled for the turbine with nrow = iter
#' @importFrom dplyr select
#' @export
#'

sample_turbine <- function(TurbineData = TurbineData,
                           windSpeedMean = windSpeedMean,
                           windSpeedSD = windSpeedSD,
                           windData=windData,
                           windThreshold=windThreshold,
                           iter=iter){
    ### GH Created function to take in a single row from the Turbine data (1 turbine at a time)
    ### CREATE TURBINE DATA FRAME###
    sampledTurbine = data.frame(matrix(data = 0, ncol = 18, nrow = iter))
    names(sampledTurbine) = c("RotorRadius", "HubHeight", "BladeWidth", "WindSpeed", "RotorSpeed", "Pitch", ### BC CHANGE ### -- windSpeed added
                              as.vector(sapply(month.abb,function(x)paste0(x,"Op"))))  ### GH change -- streamlined to an sapply instead of a hard-coded vector

    rotorSpeed <- numeric()
    rotorPitch <- numeric()

    # sample turbine pars based on their sampling dists
    # samples from wind pars, then uses pitch/speed curves
    # iter*20 is used for some reason ### GH change from S to iter*20
    windSpeed<-rtnorm((iter*20), windSpeedMean,windSpeedSD,0)
    wind.speed.m.s<-sample(windSpeed, iter+(iter/5), replace=T)
    wind.speed.m.s <- wind.speed.m.s[wind.speed.m.s>windThreshold]

    ### Streamlined by GH July 19 2021.
    ### Samples the generated wind speeds to find those that fall within the wind speed bands
    ### in the data sheet to match the sampled wind speeds to rotor pitch and rotation speed
    ### Basically acts as a lookup to find what the rotation speed of a blade is at different wind speeds

    for (y in 1:length(wind.speed.m.s)){
      for (z in 1:nrow(windData)){
        if(z<nrow(windData)){
          if(dplyr::between(wind.speed.m.s[y],windData$windSpeed[z],windData$windSpeed[z+1])){
            rotorSpeed[y]<-windData$rotationSpeed[z]
            rotorPitch[y]<-windData$bladePitch[z]
          }}else if(wind.speed.m.s[y]>=max(windData$windSpeed)){
            rotorSpeed[y]<-windData$rotationSpeed[z]
            rotorPitch[y]<-windData$bladePitch[z]}
      }
    }


    if(TurbineData$RotorSpeedAndPitch_SimOption == "windSpeedReltn"){
      randomSample <- sample(1:length(rotorSpeed), iter, replace=T)
      # wind speed
      sampledTurbine$WindSpeed <- wind.speed.m.s[randomSample]
      # rotor speed
      sampledTurbine$RotorSpeed <- rotorSpeed[randomSample]
      # Pitch
      sampledTurbine$Pitch <- rotorPitch[randomSample]
      sampledTurbine$Pitch = sampledTurbine$Pitch*pi / 180 #### Transform Pitch from degrees to radians, needed for Collision Risk Sheet

    }else{
      if(TurbineData$RotorSpeedAndPitch_SimOption == "probDist"){
        # Rotor speed ### GH - Removed these custom functions as they were all wrappers for the same function
        sampledTurbine$RotorSpeed<- rtnorm(iter, TurbineData$RotationSpeed, TurbineData$RotationSpeedSD, lower = 0)
        # Pitch ### GH - Removed these custom functions as they were all wrappers for the same function
        sampledTurbine$Pitch<- rtnorm(iter, TurbineData$Pitch, TurbineData$PitchSD, lower = 0)
        sampledTurbine$Pitch = sampledTurbine$Pitch*pi / 180 #### Transform Pitch from degrees to radians, needed for Collision Risk Sheet
      }
    }

    # Radius  -----------------------------------------------------------------

    sampledTurbine$RotorRadius <- sampler_hd(dat = TurbineData$RotorRadiusSD,
                                             mode ='rnorm',
                                             n = iter,
                                             mean = TurbineData$RotorRadius,
                                             sd = TurbineData$RotorRadiusSD)


    # Hub height --------------------------------------------------------------

    sampledTurbine$HubHeight <- sampler_hd(dat = TurbineData$HubHeightAddSD,
                                             mode ='rnorm',
                                             n = iter,
                                             mean = TurbineData$HubHeightAdd,
                                             sd = TurbineData$HubHeightAddSD)


    sampledTurbine$HubHeight <- sampledTurbine$RotorRadius + sampledTurbine$HubHeight


    # Blade width -------------------------------------------------------------


    sampledTurbine$BladeWidth <- sampler_hd(dat = TurbineData$BladeWidthSD,
                                           mode ='rnorm',
                                           n = iter,
                                           mean = TurbineData$BladeWidth,
                                           sd = TurbineData$BladeWidthSD)


    # Monthly estimates below here --------------------------------------------
    for(currentMonth in month.abb){
      # separate out the current month mean and SD. Species.count is already filtered for current species
      ### Functions having trouble with tibbles, so convert to data.frame to extract as a vector - GH Change July 19 2021
      workingMean <- data.frame(TurbineData %>% dplyr::select(contains(currentMonth)) %>% dplyr::select(contains('Mean')))[1,1]
      workingSD <- data.frame(TurbineData %>% dplyr::select(contains(currentMonth)) %>% dplyr::select(contains('SD')))[1,1]
      workingOp <- data.frame(TurbineData %>% dplyr::select(contains(currentMonth)) %>% dplyr::select(-contains('SD'), -contains('Mean')))[1,1]
      # if we have an SD, then we sample, otherwise just the mean
      if(!is.na(workingSD)){
        workingVect <- rnorm(iter, workingMean, workingSD)   ## GH - removed the function wrapper..
        ### === BC BUG FIXING === ### workingOp needs to be converted from a data.frame otherwise it doesn't return the vector of differences (only the 1st difference)
        ### GH - Above bug fix no longer an issue with vector change
        sampledTurbine[,grep(currentMonth, names(sampledTurbine))] <- workingOp - workingVect
        # will explicitly rep mean, although not needed as filling into the DF
        ### === BC BUG FIXING === ### workingOp needs to be converted from a data.frame (see above)
        ### GH - Above bug fix no longer an issue with vector change
      } else {
        sampledTurbine[,grep(currentMonth, names(sampledTurbine))] <- workingOp - rep(workingMean, iter)
        }
  }
  return(sampledTurbine)
}



