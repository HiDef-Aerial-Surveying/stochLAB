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


# The inverse of %in%, returning elements absent from a vector
"%nin%" <- Negate("%in%")


#' Format any month name to three letter code
#' @param months character string or vector. The name of the month
#'
#' @return a character string. The three letter name of the month
#' @examples
#'   format_months("January")
#' @export
format_months <- function(months){
  substr(months, 1, 1) <- toupper(substr(months, 1, 1))
  return(substr(months, 1, 3))
}


#' Generate sequence of months
#'
#' @param start_month character string, the three-letter name of the starting month.
#' @param end_month character string, the three-letter name of the finishing month.
#'
#' @return character vector. The list of months that falls in between two months
#' @examples
#'    seq_months("Jan", "Apr")
#' @export
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


# Name of parent function
#
# Returns the name of the parent function relative to current evaluation. Returns "GlobalEnv"
# if parent frame is the Global environment.
parent_fn_name <- function(){
  grandparent_call <- sys.call(-2)
  if(is.null(grandparent_call)){
    "GlobalEnv"
  }else{
    sub("\\(.+", "", deparse(grandparent_call, nlines = 1))
  }
}


# check if a number is whole
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

