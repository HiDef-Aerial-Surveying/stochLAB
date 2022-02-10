#' Total day and night hours per month
#'
#' Taken from Forsythe et al.(1995) A model comparison for daylength as a
#' function of latitude and day of year.  Ecological Modelling. 80: 87 - 95
#'
#' @param wf_latitude A decimal value. The latitude of the centroid of the
#'   windfarm, in degrees.
#' @return data frame with total number of daylight hours and night hours in each
#' month at the specified latitude.
#'
#' @export
DayLength <- function(wf_latitude){

  DaylengthThruYr <- data.frame (seq(1,365,1))
  names(DaylengthThruYr) <- c("YrDay")

  DaylengthThruYr$P <- asin(0.39795 * cos (0.2163108 + 2 * atan (0.9671396 * tan(0.0086 * (DaylengthThruYr$YrDay - 186)))))

  DaylengthThruYr$DayLength <- 24 - (24/pi) * acos((sin(0.8333*pi/180) + sin(wf_latitude * pi / 180) * sin (DaylengthThruYr$P)) / (cos(wf_latitude *pi /180 ) * cos(DaylengthThruYr$P)))

  hours <- data.frame(month.abb)
  names(hours) <- c("Month")
  monthdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  hours$Total <- sapply(monthdays,function(x) x*24)

  hours$Day[1] <- sum(DaylengthThruYr$DayLength[c(1:31)])
  hours$Day[2] <- sum(DaylengthThruYr$DayLength[c(32:59)])
  hours$Day[3] <- sum(DaylengthThruYr$DayLength[c(60:90)])
  hours$Day[4] <- sum(DaylengthThruYr$DayLength[c(91:120)])
  hours$Day[5] <- sum(DaylengthThruYr$DayLength[c(121:151)])
  hours$Day[6] <- sum(DaylengthThruYr$DayLength[c(152:181)])
  hours$Day[7] <- sum(DaylengthThruYr$DayLength[c(182:212)])
  hours$Day[8] <- sum(DaylengthThruYr$DayLength[c(213:243)])
  hours$Day[9] <- sum(DaylengthThruYr$DayLength[c(244:273)])
  hours$Day[10] <- sum(DaylengthThruYr$DayLength[c(274:304)])
  hours$Day[11] <- sum(DaylengthThruYr$DayLength[c(305:334)])
  hours$Day[12] <- sum(DaylengthThruYr$DayLength[c(335:365)])

  hours$Night <- hours$Total - hours$Day

  return(hours)
}




