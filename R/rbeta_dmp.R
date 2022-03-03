#' Customised sampling functions for the Beta distributions
#'
#' generate random samples from a beta distribution, parameterized as mean and sd, and returning NAs if conditions are not met
#'
#' @param n An integer value. The number of samples to generate
#' @param p A decimal value. The value used to calculate parameters for the beta distribution
#' @param sd A decimal value. The standard deviation of the beta distribution to simulate
#' @return a vector of samples values from the beta distribution
#'
#' @examples
#'     rbeta_dmp(n=100,p=0.14,sd=9.8)
#' @export
#'


rbeta_dmp <- function(n, p, sd){

  eta <- p*(1-p)/sd^2 - 1
  alpha <- eta*p
  beta <- eta*(1-p)

  betaMeanVarCond <- sd^2 < p*(1-p)

  if(is.na(p) | is.na(sd)){

    out <- rep(NA, n)
    warning("NA values for p and/or sd - NAs produced")

  }else{

    if(p >= 0 & p <= 1){

      if(sd < 0){
        out <- rep(NA, n)
        warning("stdev < 0 - NAs produced")
      }

      if(sd == 0){
        out <- rep(p, n)
      }

      if(sd > 0){
        if(betaMeanVarCond){
          out <- stats::rbeta(n, shape1 = alpha, shape2 = beta)
        }else{
          out <- rep(NA, n)
          warning("condition var < p*(1 - p) not met - NAs produced")
        }
      }
    }else{
      out <- rep(NA, n)
      warning("p < 0 | p > 1 - NAs produced")
    }
  }
  return(out)
}
