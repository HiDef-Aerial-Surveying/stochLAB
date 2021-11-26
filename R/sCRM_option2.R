#' Option 2 of the stochastic Band model
#'
#' This runs option 2 of the sCRM as per Masden(2015) - collisions as returned from a smooth distribution (Johnston et al default)
#'
#' @param FlightHeightSpec A dataframe. The bootstrapped flight height distribution of the species by Johnston et al. 2014
#' @param MonthlyOperational An integer value. The number of turbines in the site.
#' @param hours A data frame. The total number of hours per day at the latitude calculated by DayLength()
#' @param sampledTurbine A data frame. The row of the sampled Turbine dataframe that contains turbine information
#' @param TideOff A numeric value. The tidal offset
#' @param P_Collision A numeric value. The proportion of birds that could collide the turbine assuming no avoidance
#' @param sampledBirdParams A data frame. The row of the sampled bird parameters dataframe that contains PCH and Avoidance
#' @param LargeArrayCorrection A boolean. If true, the large array correction will be applied
#' @param L_ArrayCF A numeric value. The large array correction factor
#' @return A data frame. The collision rates for each time period.
#' @export


sCRM_option2 <- function(FlightHeightSpec=FlightHeightSpec,
                         MonthlyOperational=MonthlyOperational,
                         hours = hours,
                         P_Collision=P_Collision,
                         sampledTurbine=sampledTurbine,
                         TideOff=TideOff,
                         sampledBirdParams=sampledBirdParams,
                         LargeArrayCorrection=FALSE,
                         L_ArrayCF=NULL){


  # Option 2 calculation ----------------------------------------------------
  # Masden comment: Do model using option 2 - modelled flight height distribution
  flight.boot <- 2:dim(FlightHeightSpec)[2]     ##### BC CHANGE  -- need to skip first column with heights ######
  flight.boot.sample <- sample(flight.boot, 1, replace=T)
  FH.dat = FlightHeightSpec[,flight.boot.sample] ## using bootstraps

  HD.y <- round(seq(-1,1,0.05),2)
  height <- sampledTurbine$HubHeight + (HD.y * sampledTurbine$RotorRadius) + TideOff
  HD.d.y <- (FH.dat[floor(height)+1] + ((FH.dat[floor(height)+1] - FH.dat[ceiling(height)+1]) * (floor(height)-height))) * sampledTurbine$RotorRadius

  #risk.up <- vector()
  #risk.down <- vector()
  #for (q in 1:length(HD.y)) {
  #risk.up[q] <- xrisksum2(HD.y[q], 0.05, 1) # changed up to numeric 1
  #risk.down[q] <- xrisksum2(HD.y[q], 0.05, -1) # changed down to numeric -1
  #}
  #d.risk.up <- HD.d.y * risk.up
  #d.risk.down <- HD.d.y * risk.down

  Q2r <- 0.05 * (HD.d.y[1]/2 + HD.d.y[41]/2 + sum(HD.d.y[c(2:40)]))


  Operational <-  unlist(MonthlyOperational) / 100

  Transits <- hours$Flux * Operational

  collisions_No_Avoid <- Transits * (P_Collision/100)

  Option2_collisions_No_Avoid <- collisions_No_Avoid * Q2r

  Option2_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 1))
  names(Option2_CollisionRate) <- "Month"

  Option2_CollisionRate$Month = month.abb

  Option2_CollisionRate[,2]<- if(LargeArrayCorrection == TRUE){

    Option2_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceBasic) * L_ArrayCF

  } else {

    Option2_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceBasic)
  }

  names(Option2_CollisionRate)[2] <- "Collisions"
  return(Option2_CollisionRate)


}







