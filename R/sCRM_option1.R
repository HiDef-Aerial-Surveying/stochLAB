#' Option 1 of the stochastic Band model
#'
#' This runs option 1 of the sCRM as per Masden(2015)
#'
#' @param MonthlyOperational An integer value. The number of turbines in the site.
#' @param hours A data frame. The total number of hours per day at the latitude calculated by DayLength()
#' @param PCH A numeric value. The percentage of birds at collision risk height
#' @param P_Collision A numeric value. The proportion of birds that could collide the turbine assuming no avoidance
#' @param AvoidanceBasic A numeric value. The basic avoidance rate as defined by Band (2012) and Masden (2015)
#' @param LargeArrayCorrection A boolean. If true, the large array correction will be applied
#' @param L_ArrayCF A numeric value. The large array correction factor
#' @return A data frame. The collision rates for each time period.
#' @export

sCRM_option1 <- function(MonthlyOperational=MonthlyOperational,
                         hours=hours,
                         PCH=PCH,
                         P_Collision=P_Collision,
                         AvoidanceBasic=AvoidanceBasic,
                         LargeArrayCorrection=FALSE,
                         L_ArrayCF=NULL){

  Operational <-  unlist(MonthlyOperational) / 100

  Option1_Transits <- unlist(hours$Flux) * PCH * Operational

  Option1_collisions_No_Avoid <- Option1_Transits * (P_Collision/100)

  Option1_CollisionRate <- data.frame(Month = month.abb, Collisions = NA)

  if(LargeArrayCorrection==TRUE){
    Option1_CollisionRate$Collisions <- Option1_collisions_No_Avoid * (1-AvoidanceBasic) * L_ArrayCF
  } else {
    Option1_CollisionRate$Collisions<- Option1_collisions_No_Avoid * (1-AvoidanceBasic)
  }

  return(Option1_CollisionRate)

}


