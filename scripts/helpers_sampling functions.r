
# The reading of sampling functions ---------------------------------------
  # The following were all previously individual source calls in masden


#### --- (Extensive) Further Modifications by BC  ---- #####
#
# Including:
#  - truncated normal for wingspan, bird length & flight speed
#  - beta for Basic and Extended Avoidance, CHR & Nocturnal avoidance
#  - 3 options for generating monthly densities: (i) truncated normal; (ii) sample of the distribution ; (iii) distribution percentiles


# Sampling function --------------------------------------------










#= wingspan
  sampleWingSpan <- function(n, meanspan, sdspan){

    #rnorm(n, meanspan, sdspan)
    rtnorm_dmp(n, mean = meanspan, sd = sdspan, lower = 0)               # <<<<< BC <<<<<

  }

#= bird length

  sampleBirdLength <- function(n, meanlength, sdlength){

    #rnorm(n, meanlength, sdlength)
    rtnorm_dmp(n, mean = meanlength, sd = sdlength, lower = 0)          # <<<<< BC <<<<<

  }

#= CHR?

  sampleCRH <- function(n, meanCRH, sdCRH){

    #rnorm(n, meanCRH, sdCRH)
    rbeta_dmp(n, p = meanCRH, sd = sdCRH)   ##### BC #####

  }



#= Flight Speed                                                         # <<<<< BC <<<<<

  sampleFlightSpeed <- function(n, meanflspeed, sdflspeed){

    rtnorm_dmp(n, mean = meanflspeed, sd = sdflspeed, lower = 0)
  }



#= Nocturnal (?)

  sampleNocturnal <- function(n, meannoc, sdnoc){

    #rnorm(n, meannoc, sdnoc)
    rbeta_dmp(n, p = meannoc, sd = sdnoc)                              # <<<<< BC <<<<<

  }


#= Avoidance (basic and extended)

  sampleAvoidance <- function(n, meanavoid, sdavoid){

    #rnorm(n, meanavoid, sdavoid)
    rbeta_dmp(n, p = meanavoid, sd = sdavoid)                         # <<<<< BC <<<<<

  }




#= Bird counts                                                        # <<<<< BC <<<<<

  # sampleCount <- function(n, meancount, sdcount){
  #
  #   if(sdcount > 0 & meancount > 0){
  #     return(rtnorm(n, meancount, sdcount, 0))
  #   }else{
  #     if(sdcount==0 & meancount >=0){
  #       return(rep(meancount, n))
  #     }
  #     if(sdcount==0 & meancount < 0){
  #       return(rep(0,n))
  #     }
  #   }
  # }

  # sample from a truncated normal bounded at 0
  sampleCount_tnorm <- function(n, meancount, sdcount){

    rtnorm_dmp(n, mean = meancount, sd = sdcount, lower = 0)

  }


  # resampling with replacement from a vector of random realizations of counts from an unspecified distribution
  sampleCount_resample <- function(n, countsSample){

    dplyr::sample_n(tbl = countsSample, size = n, replace = TRUE)

    #countsSample[sample(1:nrow(countsSample), size = n, replace = TRUE),]

  }



  # resample from empirical cdf based on set of quantiles
  sampleCount_pctiles <- function(n, probs, countsPctls){

    #' based on the Inverse Transform Sampling tecnhique, by sampling random probabilities from an uniform distribution
    #' and interpolate (cubic) the count samples from the percentiles provided by the user (taken as the empirical cdf)
    x_intPoints <- runif(n, min(probs), max(probs))
    y_intPoints <- interp1(probs, countsPctls, xi = x_intPoints, method = "cubic")

    return(y_intPoints)
  }




# Sampling functions for turbine pars -------------------------------------

#= Blade width

  sampleBladeWidth <- function(n, meanwidth, sdwidth){

    rnorm(n, meanwidth, sdwidth)

  }

#= rotor radius

  sampleRotorRadius <- function(n, meanrotor, sdrotor){

    rnorm(n, meanrotor, sdrotor)

  }

#= Hub height

  sampleHubHeightAdd <- function(n, meanadd, sdadd){

    rnorm(n, meanadd, sdadd)

  }

#= operation times?

  sampleOp <- function(n, meanop, sdop){

    rnorm(n, meanop, sdop)

  }



#### BC ##### -- Aditional sampling functions for turbine params for integration of stocasticity based on a prob distn alone  =====================
#= rotation speed

  sampleRotnSpeed <-function(n, meanRotSpeed, sdRotSpeed){

    rtnorm(n, meanRotSpeed, sdRotSpeed, lower = 0)

  }


#= Blade pitch

  samplePitch <-function(n, meanPitch, sdPitch){

    rtnorm(n, meanPitch, sdPitch, lower = 0)

  }




# Misc --------------------------------------------------------------------

#= basic CV in 100%

  CV <- function(mean, sd){

    (sd/mean)*100

  }







  # generate random samples from a truncated normal, returning NAs if conditions are not met




