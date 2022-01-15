#' Customised sampling of Truncated Normal distribution
#'
#' Wrapper of the msm::rtnorm() function, improving on outputs management and
#' user feedback on edge cases
#'
#'
#' @examples
#' \donttest{
#' ## Example case one:  mean < lower while sd = 0
#' ##
#' ## original function freezes the R session
#' msm::rtnorm(n = 1, mean = 1, sd = 0, lower = 0)
#' ## this version returns NA and issues a warning
#' rtnorm_dmp(n = 1, mean = 1, sd = 0, lower = 0)
#'
#'
#' ## Example case two: mean = lower while sd = 0
#' ##
#' ## original function returns NA with no warning
#' msm::rtnorm(n = 1, mean = 1, sd = 0, lower = 1)
#' ## this version returns NA and issues a warning
#' rtnorm_dmp(n = 1, mean = 1, sd = 0, lower = 1)
#' }
#'
#'
#' @param n An integer value. The number of samples to generate
#' @param mean A decimal value. The mean for the truncated normal distribution
#' @param sd A decimal value. The standard deviation of the distribution to simulate
#' @param lower A decimal value. The lower limit for the distribution
#' @param upper A decimal value. The upper limit for the distribution
#' @return a vector of samples values from the truncated normal distribution
#' @export

rtnorm_dmp <- function(n, mean=0, sd=1, lower=-Inf, upper=Inf){

  if(is.na(mean)|is.na(sd)){
    out <- rep(NA, n)
    warning("NA values for mu and/or stdev - NAs produced")
  }else{
    if(sd >= 0){
      if(sd == 0 & mean == lower){
        #out <- rep(lower, n)
        # this is consistent with behavior in msm::rtnorm() and truncnorm::rtruncnorm()
        out <- rep(NA, n)
        warning("mean is equal to lower bound - NAs produced")
      }
      if(sd == 0 & mean < lower){
        out <- rep(NA, n)
        warning("mu < lower bound  while SD = 0 - NAs produced")
      }
      if(sd == 0 & mean > lower){
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
