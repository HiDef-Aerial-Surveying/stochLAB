library(foreach)
library(tidyverse)
library(stochLAB)

BirdDat <- readxl::read_xlsx("C:/Temp/Scenarios_for_Nina.xlsx",sheet="BirdData")
names(BirdDat) <- str_replace_all(names(BirdDat)," ","")
TurbineDat <- readxl::read_xlsx("C:/Temp/Scenarios_for_Nina.xlsx",sheet="TurbineData")
names(TurbineDat) <- str_replace_all(names(TurbineDat)," ","")
CountDat <- readxl::read_xlsx("C:/Temp/Scenarios_for_Nina.xlsx",sheet="CountData")
names(CountDat) <- str_replace_all(names(CountDat)," ","")



outputs <- matrix(nrow=nrow(CountDat),ncol=5)

for(i in 1:nrow(CountDat)){

  spp_name <- CountDat$Species[i]
  wf_name <- CountDat$Windfarm[i]

  BirdData <- BirdDat %>% dplyr::filter(Species == spp_name)
  TurbineData <- TurbineDat %>% dplyr::filter(Windfarm == wf_name)
  CountData <- CountDat[i,]

  outs <- mig_stoch_crm(BirdData,TurbineData,CountData,iter=1000,spp_name,LargeArrayCorrection = T)


  ## Send outputs to matrix
  outputs[i,1] <- spp_name
  outputs[i,2] <- wf_name
  outputs[i,3] <- paste(round(mean(outs[,1],na.rm=T),3), "\u00B1", round(sd(outs[,1],na.rm=T),3))
  outputs[i,4] <- paste(round(mean(outs[,2],na.rm=T),3), "\u00B1", round(sd(outs[,2],na.rm=T),3))
  outputs[i,5] <- paste(round(mean(outs[,3],na.rm=T),3), "\u00B1", round(sd(outs[,3],na.rm=T),3))

}
outputs <- data.frame(outputs)
names(outputs) <- c('species',"windfarm","PrBMigration","PoBMigration","Omigration")



PreBreedout <- reshape2::dcast(outputs[,c(1:3)],formula = species ~windfarm)
PostBreedout <- reshape2::dcast(outputs[,c(1,2,4)],formula = species ~windfarm)
Otherout <- reshape2::dcast(outputs[,c(1,2,5)],formula = species ~windfarm)


outputs[,c(1:3)]








i <- 1




PrBsummary <- data.frame(species = CountData$Species[i])

PoBsummary <-

Osummary <-











# Global variables   ---------------------------------------------------------
model_months <- month.abb
n_months <- length(model_months)

## get daylight hours and night hours per month based on the latitude
## This is only for future proofing, 2021 model does not assume day hours have
## impact on birds
daynight_hrs_month <- DayLength(TurbineData$Latitude)

# Chord taper profile based on the blade of a typical 5 MW turbine used for
# offshore generation. Required for `p_single_collision` function
chord_profile <- data.frame(
  # radius at bird passage point, as a proportion of rotor radius (R)
  pp_radius = seq(0.05, 1, 0.05),
  # chord width at pp_radius, as a proportion of the maximum chord width
  chord = c(0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85, 0.80, 0.75,
            0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30,0.24,0.00)
)


# Initiate objects to harvest results ----------------------------------------
sampledBirdParams <- list()

mcrm_outputs <- data.matrix(
  matrix(data = NA, ncol = 3, nrow = iter,
         dimnames = list(NULL, c('PrBMigration','PoBMigration','Omigration')))
)

# Prepare inputs  ------------------------------------------------------------

## For the migration app we make the assumption to be precautionary, that the animals are
## passing through the windfarm area during the day time.
## Thus, flux is simply a count of the number of birds passing through the area
## as per the simulation outputs


## join rotation and pitch data into a single table
#windData <- dplyr::left_join(windData_rotation,windData_pitch,by='windSpeed')
#windThreshold <- windData$windSpeed[min(which(windData$rotationSpeed != 0))]

## bird inputs
species.dat = BirdData

species.dat$FlightNumeric <- ifelse(species.dat$Flight == 'Flapping', 1, 0)
Flap_Glide = ifelse (species.dat$Flight == "Flapping", 1, 2/pi)

# Generate random draws of parameters  ---------------------------------------
## sample bird attributes

sampledBirdParams$WingSpan <- sampler_hd(dat = species.dat$WingspanSD,
                                         mode = 'rtnorm',
                                         n = iter,
                                         mean=species.dat$Wingspan,
                                         sd = species.dat$WingspanSD,
                                         lower = 0)

sampledBirdParams$BodyLength <- sampler_hd(dat = species.dat$BodyLengthSD,
                                           mode = 'rtnorm',
                                           n = iter,
                                           mean=species.dat$BodyLength,
                                           sd = species.dat$BodyLengthSD,
                                           lower = 0)


sampledBirdParams$FlightSpeed <- sampler_hd(dat = species.dat$FlightSpeedSD,
                                            mode = 'rtnorm',
                                            n = iter,
                                            mean=species.dat$FlightSpeed,
                                            sd = species.dat$FlightSpeedSD,
                                            lower = 0)

### Sampler deactivated Nov 2021 as PCH is a point estimate
#sampledBirdParams$PCH <- #sampler_hd(dat = species.dat$Prop_CRH_ObsSD,
#mode = 'rbeta',
#n = iter,
#mean=species.dat$Prop_CRH_Obs,
#sd = species.dat$Prop_CRH_ObsSD)

sampledBirdParams$PCH <- rep(species.dat$PCH,iter)


### Nocturnal activity set to 0 for future proofing
sampledBirdParams$NocturnalActivity <- rep(0,iter) #sampler_hd(dat = species.dat$Nocturnal_ActivitySD,
#          mode = 'rbeta',
#          n = iter,
#          mean=species.dat$Nocturnal_Activity,
#          sd = species.dat$Nocturnal_ActivitySD)


sampledBirdParams$Avoidance <- sampler_hd(dat = species.dat$AvoidanceSD,
                                          mode = 'rbeta',
                                          n = iter,
                                          mean=species.dat$Avoidance,
                                          sd = species.dat$AvoidanceSD)



## turbine parameters
## function where the row gets passed in for sampling
sampledTurbine <- sample_turbine_mCRM(TurbineData,BirdData,iter)



# Sample the counts -------------------------------------------------------

SampledCounts <- sampler_hd(dat = CountData$`Populationestimate(SD)`,
                            mode = 'rtnorm',
                            n = iter,
                            mean=CountData$Populationestimate,
                            sd = CountData$`Populationestimate(SD)`,
                            lower = 0)


### Iterate over seasons, then over sampled parameters


for(bp in c('PrBMigration','PoBMigration','Omigration')){
  sampTurb <- sampledTurbine %>% dplyr::select(RotorRadius,BladeWidth,RotorSpeed,Pitch,contains(bp))
  if(ncol(sampTurb)>4){

    for(i in 1:iter){
      p_single_collision <-
        get_prob_collision(
          chord_prof = chord_profile,
          flight_speed = sampledBirdParams$FlightSpeed[i],
          body_lt = sampledBirdParams$BodyLength[i],
          wing_span = sampledBirdParams$WingSpan[i],
          prop_upwind = TurbineData$Proportionupwindflight/100,
          flap_glide = Flap_Glide,
          rotor_speed = sampTurb$RotorSpeed[i],
          rotor_radius = sampTurb$RotorRadius[i],
          blade_width = sampTurb$BladeWidth[i],
          blade_pitch = sampTurb$Pitch[i],
          n_blades = TurbineData$Numberofblades
        )




      # STEP 2 - Set up Large Array Correction Factor -----
      if (LargeArrayCorrection == TRUE) {
        L_ArrayCF <-
          get_lac_factor(
            n_turbines = TurbineData$Numberofturbines,
            rotor_radius = sampTurb$RotorRadius[i],
            avoidance_rate = sampledBirdParams$Avoidance[i],
            prob_single_collision = p_single_collision,
            mean_prop_operational = sampTurb[i,paste0(bp,"_OT")],
            wf_width = TurbineData$Width
          )
      } else{
        # set multiplier to 1 to dismiss large array correction
        L_ArrayCF <- 1
      }

      flux_fct <- SampledCounts[i]


      # Step 3 - Apply option 1 of the sCRM --------------------------------------

      mcrm_outputs[i,bp] <- crm_opt1(
        flux_factor = flux_fct,
        prop_crh_surv = sampledBirdParams$PCH[i],
        prob_single_collision = p_single_collision,
        prop_operational = sampTurb[i,paste0(bp,"_OT")],
        avoidance_rate = sampledBirdParams$Avoidance[i],
        lac_factor = L_ArrayCF)
    }


  }
}

return(mcrm_outputs)


