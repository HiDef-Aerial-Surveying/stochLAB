#' Customised sampling function wrapper for rtnorm_dmp and rbeta_dmp
#'
#' Samples a dataset based on inputs for either the rtnorm or rbeta distributions
#'
#' @param dat = A decimal value. The SD value to test (from the UI) - if not available in the UI, then do not create a distribution
#' @param mode = A string.  Either 'rtnorm' or 'rbeta' to determine which distribution to generate
#' @param n An integer value. The number of samples to generate
#' @param mean A decimal value. The mean for the truncated normal distribution
#' @param sd A decimal value. The standard deviation of the distribution to simulate
#' @param lower A decimal value. The lower limit for the distribution
#' @param upper A decimal value. The upper limit for the distribution
#' @return a vector of samples values from the distribution
#' @export
#'

sampler_hd <- function(dat,mode="rtnorm",n=NULL,mean=NULL,sd=NULL,lower=NULL,upper=NULL){
  if(!is.na(dat)){
    if(mode == "rtnorm"){
      output <- rtnorm_dmp(n = n,
                           mean = mean,
                           sd = sd, lower = 0)

    }else if(mode == "rbeta"){
      output <- rbeta_dmp(n = n,
                          p = mean,
                          sd = sd)
    }
  }else{
    output <- rep(dat,n)
  }

  return(output)

}
