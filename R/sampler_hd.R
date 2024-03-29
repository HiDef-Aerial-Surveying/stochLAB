#' Customised sampling function wrapper
#'
#' Samples a dataset based on inputs for either the rtnorm, rbeta or 'rnorm' distributions
#'
#' @param dat = A decimal value. The SD value to test (from the UI) - if not available in the UI, then do not create a distribution
#' @param mode = A string.  Either 'rtnorm', 'rbeta' or 'rnorm' to determine which distribution to generate
#' @param n An integer value. The number of samples to generate
#' @param mean A decimal value. The mean for the truncated normal distribution
#' @param sd A decimal value. The standard deviation of the distribution to simulate
#' @param lower A decimal value. The lower limit for the distribution
#' @param upper A decimal value. The upper limit for the distribution
#' @return a vector of samples values from the distribution
#'
#' @examples
#'   sampler_hd(dat=0.1,
#'        mode='rtnorm',
#'        n=100,
#'        mean=9,
#'        sd=0.1)
#'
#' @export
#'

sampler_hd <- function(dat,mode="rtnorm",n=NULL,mean=NULL,sd=NULL,lower=0,upper=NULL){
  if(mean == 0){      ## GH addition, 2/2/22, if mean is 0, then return 0
    output <- rep(0,n)
  }else{
    if(!is.na(dat)){
      if(mode == "rtnorm"){
        output <- rtnorm_dmp(n = n,
                             mean = mean,
                             sd = sd, lower = lower)

      }else if(mode == "rbeta"){
        output <- rbeta_dmp(n = n,
                            p = mean,
                            sd = sd)

      }else if(mode == "rnorm"){
        output <- rnorm(n = n,
                        mean = mean,
                        sd = sd)
      }

    }else{
      output <- rep(dat,n)
    }
  }



  return(output)

}
