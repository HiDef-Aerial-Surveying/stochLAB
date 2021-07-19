#' The probability of collision assuming no avoidance
#'
#' The probability of a bird colliding a blade as modified from Masden (2015)
#' @param sampledBirdParams A data frame. The data frame with all the sampled parameters of the bird's features
#' @param sampledTurbine A data frame. The data frame with all the sampled parameters from the turbine
#' @param TurbineData A data frame. The Turbine data formatted as per the "TurbineData" data object
#' @param Prop_Upwind A decimal value. A value between 0-1 bounded as proportion of flights upwind - default of 0.5.
#' @param Flap_Glide A decimal value. The value representing the correction for flapping or gliding birds
#' @return A numeric value. The proportion of birds that could collide a blade
#' @export

probability_collision_no_avoid <- function(sampledBirdParams=sampledBirdParams,
                                           sampledTurbine=sampledTurbine,
                                           TurbineData=turbineData,
                                           Prop_Upwind=Prop_Upwind,
                                           Flap_Glide=Flap_Glide){

  CollisionRiskTab = data.frame(matrix(data = 0, nrow = 21, ncol = 7))
  names(CollisionRiskTab) = c("radius", "chord", "alpha", "Up_length", "Up_P", "Down_length", "Down_P")
  CollisionRiskTab$radius = seq(0, 1, 0.05)
  CollisionRiskTab$chord = c(NA, 0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85, 0.80, 0.75, 0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30,0.24,0.00)

  #### can be revised to match actual turbine blades

  CollisionRiskTab$alpha[1] = NA
  CollisionRiskTab$Up_length[1] = NA
  CollisionRiskTab$Up_P[1] = 1
  CollisionRiskTab$Down_length[1] = NA
  CollisionRiskTab$Down_P[1] = 1


  ### populate collision risk table

  for (u in 1:20) {

    #### First calculate alphas

    CollisionRiskTab$alpha[u + 1] = sampledBirdParams$FlightSpeed * (60/sampledTurbine$RotorSpeed) /
      (CollisionRiskTab$radius[u + 1] * sampledTurbine$RotorRadius * 2 * pi)

    # collision risk calculations ---------------------------------------------



    #### Now calculate upwind length


    CollisionRiskTab$Up_length[u+1] <- ifelse (CollisionRiskTab$alpha[u + 1] < (sampledBirdParams$BodyLength /sampledBirdParams$WingSpan),

                                               sampledBirdParams$BodyLength +
                                                 abs(sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch)+
                                                       (CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch))),


                                               (sampledBirdParams$WingSpan * Flap_Glide * CollisionRiskTab$alpha[u + 1]) +
                                                 abs(sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch)+
                                                       (CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch))))

    #### Now calculate upwind probability of collision


    CollisionRiskTab$Up_P[u+1] = min (1, (TurbineData$Blades/(60/sampledTurbine$RotorSpeed)) *
                                        CollisionRiskTab$Up_length[u+1]/sampledBirdParams$FlightSpeed)

    #### Now calculate downwind length

    ifelse (CollisionRiskTab$alpha[u + 1] < (sampledBirdParams$BodyLength /sampledBirdParams$WingSpan),

            sampledBirdParams$BodyLength +
              abs(-sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch)+
                    (CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch)))
            -> CollisionRiskTab$Down_length[u+1],


            (sampledBirdParams$WingSpan * Flap_Glide * CollisionRiskTab$alpha[u + 1]) +
              abs(-sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*sin(sampledTurbine$Pitch)+
                    (CollisionRiskTab$alpha[u + 1] * sampledTurbine$BladeWidth*CollisionRiskTab$chord[u + 1]*cos(sampledTurbine$Pitch)))
            -> CollisionRiskTab$Down_length[u+1])


    #### Now calculate Down wind probability of collision


    CollisionRiskTab$Down_P[u+1] = min (1, (TurbineData$Blades/(60/sampledTurbine$RotorSpeed)) *
                                          CollisionRiskTab$Down_length[u+1]/sampledBirdParams$FlightSpeed)


  }


  Total_Up_Wind_P = 2 * (sum(CollisionRiskTab$radius[2:20] * CollisionRiskTab$Up_P[2:20]) + CollisionRiskTab$Up_P[21]/2) * 0.05

  Total_Down_Wind_P = 2 * (sum(CollisionRiskTab$radius[2:20] * CollisionRiskTab$Down_P[2:20]) + CollisionRiskTab$Down_P[21]/2) * 0.05


  P_Collision = (Prop_Upwind * Total_Up_Wind_P) + ((1-Prop_Upwind) * Total_Down_Wind_P)
  P_Collision = 100 * P_Collision


}


