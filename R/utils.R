#' Sample count percentiles
#'
#' resample from empirical cdf based on set of quantiles
#' @return interpolated random probabilities as a vector
#' @import pracma
#'
#' @export
sampleCount_pctiles <- function(n, probs, countsPctls){
  # based on the Inverse Transform Sampling tecnhique, by sampling random probabilities from an uniform distribution
  # and interpolate (cubic) the count samples from the percentiles provided by the user (taken as the empirical cdf)
  x_intPoints <- runif(n, min(probs), max(probs))
  y_intPoints <- pracma::interp1(probs, countsPctls, xi = x_intPoints, method = "cubic")
  return(y_intPoints)
}

#' Coefficient of Variation
#'
#' Calculate the coefficient of variation of a single value
#' @return a numeric value
#'
#' @export
#'
CV <- function(mean, sd){
  (sd/mean)*100
}


#' The inverse of \%in\%
#'
#' The inverse of \%in\% for identifying elements absent from a vector
"%notin%" <- Negate("%in%")


