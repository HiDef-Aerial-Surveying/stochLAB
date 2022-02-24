#' Generate random draws based on empirical c.d.f.
#'
#' Generate random draws from a set of quantiles, based on the empirical
#' cumulative density function
#'
#' @param n integer, the number of draws to generate
#' @param probs numeric vector, the probabilities
#' @param qtls numeric vector, the quantiles for the probabilities
#'   specified in `probs`
#'
#' @details Based on the Inverse Transform Sampling technique, by sampling
#' random probabilities from a uniform distribution and interpolate (cubic)
#' the count samples from the percentiles provided by the user (taken as the
#' empirical cumulative density function)
#'
#' @return a numeric vector, with random draws of the approximated distribution
#' underpinning the provided quantiles
#' @export
#' @examples
#'  sample_qtls(10,c(0.1,0.2,0.3),qtls=c(0.05,0.1,0.95))
sample_qtls <- function(n, probs, qtls){
  x_intPoints <- runif(n, min(probs), max(probs))
  y_intPoints <- pracma::interp1(probs, qtls, xi = x_intPoints, method = "cubic")
  return(y_intPoints)
}



#' Coefficient of Variation
#'
#' Calculate the coefficient of variation of a single value
#' @return a numeric value
#'
#' @param mean numeric, mean of the random variable
#' @param sd numeric, standard deviation of the random variable
#' @export
#' @examples
#'  CV(mean=10,sd=3)
CV <- function(mean, sd){
  (sd/mean)*100
}


# The inverse of %in%, returning elements absent from a vector
"%nin%" <- Negate("%in%")


#' Format month names
#'
#' Formats a character vector with month names to their 3-letter abbreviation
#'
#' @param months a character vector with month names
#' @examples
#'    format_months(c("Jan","Feb"))
format_months <- function(months){
  substr(months, 1, 1) <- toupper(substr(months, 1, 1))
  substr(months, 1, 3)
}



#' Generate sequence of months
#'
#' @param start_month character string, the three-letter name of the starting month.
#' @param end_month character string, the three-letter name of the finishing month.
#' @examples
#'    seq_months("Jan", "Apr")
seq_months <- function(start_month, end_month){
  Mst <- which(month.abb == start_month)
  Men <- which(month.abb == end_month)
  if(Mst > Men){
    Mnths <- c(month.abb[Mst:12],month.abb[1:Men])
  }else{
    Mnths <- month.abb[Mst:Men]
  }
  return(Mnths)
}



#' Name of parent function
#'
#' Helper to get the name of the parent function.
#'
#' @return
#' The name of the parent function relative to current evaluation. Returns "GlobalEnv"
#' if parent frame is the Global environment.
#' @examples
#'     parent_fn_name()
parent_fn_name <- function(){
  grandparent_call <- sys.call(-2)
  if(is.null(grandparent_call)){
    "GlobalEnv"
  }else{
    sub("\\(.+", "", deparse(grandparent_call, nlines = 1))
  }
}


### Upper-case conversion of the first character of a string
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


#' check if a number is whole
#' @param x a numeric value
#' @param tol a numeric value
#' @examples
#'     is.wholenumber(2)
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

