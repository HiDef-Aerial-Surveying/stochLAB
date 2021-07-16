
# fill bird parameters with sampled values --------------------------------
  #= [NB Masden note: if no measure of variance provided, only the mean value is used
  #= here replace all ifelse with if. Only a constant needs evaluating for the if - so ifelse inefficient



sampledBirdParams$WingSpan <- sampler_hd(dat = species.dat$WingspanSD,
                                         mode = 'rtnorm',
                                         n = iter,
                                         mean=species.dat$Wingspan,
                                         sd = species.dat$WingspanSD,
                                         lower = 0)

sampledBirdParams$BodyLength <- sampler_hd(dat = species.dat$Body_LengthSD,
                                           mode = 'rtnorm',
                                           n = iter,
                                           mean=species.dat$Body_Length,
                                           sd = species.dat$Body_LengthSD,
                                           lower = 0)


sampledBirdParams$FlightSpeed <- sampler_hd(dat = species.dat$Flight_SpeedSD,
                                            mode = 'rtnorm',
                                            n = iter,
                                            mean=species.dat$Flight_Speed,
                                            sd = species.dat$Flight_SpeedSD,
                                            lower = 0)

sampledBirdParams$PCH <- sampler_hd(dat = species.dat$Prop_CRH_ObsSD,
                                    mode = 'rbeta',
                                    n = iter,
                                    mean=species.dat$Prop_CRH_Obs,
                                    sd = species.dat$Prop_CRH_ObsSD)


sampledBirdParams$NocturnalActivity <- sampler_hd(dat = species.dat$Nocturnal_ActivitySD,
                                                  mode = 'rbeta',
                                                  n = iter,
                                                  mean=species.dat$Nocturnal_Activity,
                                                  sd = species.dat$Nocturnal_ActivitySD)


sampledBirdParams$AvoidanceBasic <- sampler_hd(dat = species.dat$AvoidanceBasicSD,
                                               mode = 'rbeta',
                                               n = iter,
                                               mean=species.dat$AvoidanceBasic,
                                               sd = species.dat$AvoidanceBasicSD)


# Avoidance ---------------------------------------------------------------


if(!is.na(species.dat$AvoidanceBasicSD)){

  sampledBirdParams$AvoidanceBasic <- sampleAvoidance(iter, species.dat$AvoidanceBasic, species.dat$AvoidanceBasicSD)

} else {

  sampledBirdParams$AvoidanceBasic <- rep(species.dat$AvoidanceBasic, iter)

}


# extended variant

if(!is.na(species.dat$AvoidanceExtendedSD)){

  sampledBirdParams$AvoidanceExtended <- sampleAvoidance(iter, species.dat$AvoidanceExtended, species.dat$AvoidanceExtendedSD)

} else {

  sampledBirdParams$AvoidanceExtended <- rep(species.dat$AvoidanceExtended, iter)

}


# Monthly density estimates below here --------------------------------------------

if(c_densOpt == "truncNorm"){

  for(currentMonth in monthLabels){

      # separate out the current month mean and SD. Species.count is already filtered for current species
      workingMean <- species.count %>% select(contains(currentMonth)) %>% select(-contains('SD'))

      workingSD <- species.count %>% select(contains(currentMonth)) %>% select(contains('SD'))

      # if we have an SD, then we sample, other wise just the mean
      if(!is.na(workingSD[1,1])){

        workingVect <- sampleCount_tnorm(iter, workingMean[1,1], workingSD[1,1])                  # <<<<< BC <<<<<

        sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect

        # will explicitly rep mean, although not needed as filling into the DF
      } else {
        sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- rep(workingMean[1,1], iter)}
    }
}



if(c_densOpt == "reSamp"){
  for(currentMonth in monthLabels){

    #browser()

    workingVect <- sampleCount_resample(n = iter, countsSample = species.count %>% select(contains(currentMonth)))

    sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect

  }
}



if(c_densOpt == "pcntiles"){
  for(currentMonth in monthLabels){

    cPcntls <- species.count %>% select(referenceProbs, contains(currentMonth))

    workingVect <- sampleCount_pctiles(iter, probs = cPcntls[, 1], countsPctls = cPcntls[, 2])

    sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect

  }
}


