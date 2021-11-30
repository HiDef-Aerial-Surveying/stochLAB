#' Option 3 of the stochastic Band model
#'
#' This runs option 3 of the sCRM as per Masden(2015) - collisions as returned from a smooth distribution (Johnston et al default)
#' taking into account risk of collision at various points over the turbine blade
#'
#' @param FlightHeightSpec A dataframe. The bootstrapped flight height distribution of the species by Johnston et al. 2014
#' @param MonthlyOperational An integer value. The number of turbines in the site.
#' @param hours A data frame. The total number of hours per day at the latitude calculated by DayLength()
#' @param sampledTurbine A data frame. The row of the sampled Turbine dataframe that contains turbine information
#' @param TideOff A numeric value. The tidal offset
#' @param P_Collision A numeric value. The proportion of birds that could collide the turbine assuming no avoidance
#' @param sampledBirdParams A data frame. The row of the sampled bird parameters dataframe that contains PCH and Avoidance
#' @param speciesdat A data frame. The data extracted for the species
#' @param LargeArrayCorrection A boolean. If true, the large array correction will be applied
#' @param L_ArrayCF A numeric value. The large array correction factor
#' @return A data frame. The collision rates for each time period.
#' @export

sCRM_option3 <- function(FlightHeightSpec=FlightHeightSpec,
                         MonthlyOperational=MonthlyOperational,
                         hours = hours,
                         P_Collision=P_Collision,
                         sampledTurbine=sampledTurbine,
                         TideOff=TideOff,
                         Prop_Upwind = NULL,
                         sampledBirdParams=sampledBirdParams,
                         speciesdat=species.dat,
                         LargeArrayCorrection=FALSE,
                         L_ArrayCF=NULL){



  currentFlightNumeric <- speciesdat$FlightNumeric

  inputRad = round(seq(0,1,0.05),2)
  inputCirc = c(0.69,0.73,0.79,0.88,0.96,1,0.98,0.92,0.85,0.8,0.75,0.7,0.64,0.58,0.52,0.47,0.41,0.37,0.3,0.24,0)


#### Work out Integral Flux
flight.boot <- 2:dim(FlightHeightSpec)[2]     ##### BC CHANGE  -- need to skip first column with heights ######
flight.boot.sample <- sample(flight.boot, 1, replace=T)
FH.dat = FlightHeightSpec[,flight.boot.sample] ## using bootstraps

HD.y <- round(seq(-1,1,0.05),2)
height <- sampledTurbine$HubHeight + (HD.y * sampledTurbine$RotorRadius) + TideOff
HD.d.y <- (FH.dat[floor(height)+1] + ((FH.dat[floor(height)+1] - FH.dat[ceiling(height)+1]) * (floor(height)-height))) * sampledTurbine$RotorRadius

  FluxMin <- HD.d.y[1] * (2 * ((1 - HD.y[1] * HD.y[1])^0.5)) / 2 ### risk at lowest point on rotor blade
  FluxInt <- FluxMin + HD.d.y[41] * (2 * ((1 - HD.y[41] * HD.y[41])^0.5)) / 2 ### risk at highest point on rotor blade

  for (r in 2:40) {

	    FluxInt <- FluxInt + HD.d.y[r] * (2 * ((1 - HD.y[r] * HD.y[r])^0.5))  #### Fill in intermediate heights

		}


  FluxInt <- FluxInt * 0.05 * (2/pi)


##### Work out Collision Flux

## Up wind

CollMinUP <- HD.d.y[1] * xrisksum2(HD.y[1], 0.05, 1,
                                   inputRad = inputRad,
                                   inputCirc = inputCirc,
                                   sampledTurbine = sampledTurbine,
                                   inputFlight = currentFlightNumeric,
                                   sampledBirdParams = sampledBirdParams) / 2 ### risk at lowest point on rotor blade


CollIntUP <- CollMinUP + HD.d.y[41] * xrisksum2(HD.y[41], 0.05, 1,
                                                inputRad = inputRad,
                                                inputCirc = inputCirc,
                                                sampledTurbine = sampledTurbine,
                                                inputFlight = currentFlightNumeric,
                                                sampledBirdParams = sampledBirdParams) / 2 ### risk at highest point on rotor blade

  for (v in 2:40) {

	  CollIntUP <- CollIntUP + HD.d.y[v] * xrisksum2(HD.y[v], 0.05, 1,
	                                                 inputRad = inputRad,
	                                                 inputCirc = inputCirc,
	                                                 sampledTurbine = sampledTurbine,
	                                                 inputFlight = currentFlightNumeric,
	                                                 sampledBirdParams = sampledBirdParams)  #### Fill in intermediate heights

		}


  CollIntUP = CollIntUP * 0.05 * (2/pi)

## Down wind

CollMinDown <- HD.d.y[1] * xrisksum2(HD.y[1], 0.05, -1,
                                     inputRad = inputRad,
                                     inputCirc = inputCirc,
                                     sampledTurbine = sampledTurbine,
                                     inputFlight = currentFlightNumeric,
                                     sampledBirdParams = sampledBirdParams) / 2 ### risk at lowest point on rotor blade

CollIntDown <- CollMinDown + HD.d.y[41] * xrisksum2(HD.y[41], 0.05, -1,
                                                    inputRad = inputRad,
                                                    inputCirc = inputCirc,
                                                    sampledTurbine = sampledTurbine,
                                                    inputFlight = currentFlightNumeric,
                                                    sampledBirdParams = sampledBirdParams) / 2 ### risk at highest point on rotor blade

for (w in 2:40) {

	CollIntDown = CollIntDown + HD.d.y[w] * xrisksum2(HD.y[w], 0.05, -1,
	                                                  inputRad = inputRad,
	                                                  inputCirc = inputCirc,
	                                                  sampledTurbine = sampledTurbine,
	                                                  inputFlight = currentFlightNumeric,
	                                                  sampledBirdParams = sampledBirdParams)  #### Fill in intermediate heights

		}


CollIntDown <- CollIntDown * 0.05 * (2/pi)

## Average Collision Integral

CollInt <- (Prop_Upwind * CollIntUP) + ((1-Prop_Upwind) * CollIntDown)
CollRiskDist <- CollInt/FluxInt ##Average collision risk for single rotor transit


## Calculate Collisions

# Operational = c(sampledJanOp[i],sampledFebOp[i],sampledMarOp[i],sampledAprOp[i],sampledMayOp[i],sampledJunOp[i],sampledJulOp[i],sampledAugOp[i],sampledSepOp[i],sampledOctOp[i],sampledNovOp[i],sampledDecOp[i])

  Operational <- sampledTurbine %>% select(contains('Op', ignore.case = F))

  Operational <- unlist(Operational)/100

  Option3_collisions_No_Avoid <- hours$Flux * CollInt * Operational

  Option3_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 1))
  names(Option3_CollisionRate) <- "Month"
  Option3_CollisionRate$Month <- month.abb



  if(LargeArrayCorrection == "yes"){

              Option3_CollisionRate[,2] <- Option3_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceExtended) * L_ArrayCF

            } else {

              Option3_CollisionRate[,2] <- Option3_collisions_No_Avoid * (1-sampledBirdParams$AvoidanceExtended)

            }

  names(Option3_CollisionRate)[2] = "Collisions"

  return(Option3_CollisionRate)

}
