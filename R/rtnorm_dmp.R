#' Customised sampling functions for the Truncated Normal distribution
#'
#' generate random samples from a truncated normal distribution, parameterized as mean and sd, and returning NAs if conditions are not met
#'
#' @param n An integer value. The number of samples to generate
#' @param mean A decimal value. The mean for the truncated normal distribution
#' @param sd A decimal value. The standard deviation of the distribution to simulate
#' @param lower A decimal value. The lower limit for the distribution
#' @param upper A decimal value. The upper limit for the distribution
#' @return a vector of samples values from the truncated normal distribution
#' @export
#'


rtnorm_dmp <- function(n, mean=0, sd=1, lower=-Inf, upper=Inf){

  if(is.na(mean)|is.na(sd)){
    out <- rep(NA, n)
    warning("NA values for mu and/or stdev - NAs produced")
  }else{
    if(sd >= 0){
      if(sd == 0 & mean == lower){
        out <- rep(lower, n)
      }
      if(sd == 0 & mean < lower){
        out <- rep(NA, n)
        warning("mu < lower & SD = 0 - NAs produced")
      }
      if(sd == 0 & mean > lower){
        #out <- rtnorm(n, mean = mean, sd = sd, lower = lower, upper = upper)
        out <- rep(mean, n)
      }
      if(sd > 0){
        out <- msm::rtnorm(n, mean = mean, sd = sd, lower = lower, upper = upper)
      }
    }else{
      warning("SD < 0 - NAs produced")
      out <- rep(NA, n)
    }
  }

  return(out)
}
