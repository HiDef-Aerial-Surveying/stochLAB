#' Large array correction
#'
#' A correction factor in the case that there is a very large wind farm array which may deter birds
#' @param NTurbines An integer value. The number of turbines in the site.
#' @param sampledTurbine A data frame. The data frame with all the sampled parameters from the turbine
#' @param sampledBirdParams A data frame. The data frame with all the sampled parameters of the bird's features
#' @param P_Collision A numeric value. The proportion of birds that could collide the turbine assuming no avoidance
#' @param MeanOperational A numeric value. The mean operational time of the wind farm (in hours)
#' @param WFWidth A numeric value. The approximate longitudinal width (in km) of the wind farm
#' @return A numeric value. The correction factor to be applied
#' @export

large_array_correction <- function(NTurbines=NTurbines,
                                   sampledTurbine=sampledTurbine,
                                   sampledBirdParams=sampledBirdParams,
                                   P_Collision=P_Collision,
                                   MeanOperational=MeanOperational,
                                   WFWidth=WFWidth){

  NTurbRows <- NTurbines ^ 0.5

  CollRiskSinglePassage <- NTurbines * (pi * sampledTurbine$RotorRadius^2)/(2 * sampledTurbine$RotorRadius * WFWidth * 1000) *
    (P_Collision/100) * (MeanOperational/100) * (1-sampledBirdParams$AvoidanceBasic)

  L_ArrayCF <- 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage +
    (NTurbRows - 1) * (2*NTurbRows)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)

  return(L_ArrayCF)
}

