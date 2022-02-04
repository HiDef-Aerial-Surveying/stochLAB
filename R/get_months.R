#' Gets a list of months from a month range
#'
#' Outputs a vector of months from the input month range
#'
#' @param Mrange A character string. The month range in the format "XXX - XXX"
#' @return A vector of the months within the given range
#' @export
#' @examples
#' get_months("Jan - Apr")
#'
get_months <- function(Mrange){
  Mspl <- strsplit(Mrange," - ")
  Mst <- which(month.abb == Mspl[[1]][1])
  Men <- which(month.abb == Mspl[[1]][2])

  if(Mst > Men){
    Mnths <- month.abb[c(1:Men,Mst:12)]
  }else{
    Mnths <- month.abb[Mst:Men]
  }
  return(Mnths)
}
