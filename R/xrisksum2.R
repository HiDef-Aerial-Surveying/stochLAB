#' The xrisksum2 function for calculating risk along a turbine blade
#'
#' This function will calculate the risk associated with each point along the surface of a turbine blade
#'
#' @export


xrisksum2 = function (y, xinc, direction,
                      inputRad = currentRad,
                      inputCirc = currentCirc,
                      inputRotorRadius = currentRotorRadius,
                      inputBladeWidth = currentBladeWidth,
                      inputRotorSpeed = currentRotorSpeed,
                      inputPitch = currentPitch,
                      inputFlight = currentFlightNumeric,
                      inputWingSpan = currentWingSpan,
                      inputFlightSpeed = currentFlightSpeed,
                      inputBirdLength = currentBirdLength,
                      inputBlades = currentBlades) {

  xmax = (1- y * y) ^ 0.5

  imax = as.integer (xmax/xinc)

  # avoid repeating calcs - once here for use later
  pcollxy_imax <- pcollxy (imax * xinc, y, direction)

  pcollxy_xmax <- pcollxy(xmax, y, direction)
  pcollxy_0 <- pcollxy(0, y, direction)

  #
  risk = (pcollxy_imax/2 + pcollxy_xmax/2) * (xmax - imax * xinc)
  risk2 = risk + (pcollxy_0/2 + pcollxy_imax/2) * xinc

  # loop over k
  for (k in 1: (imax - 1)) {


    risk2 = risk2 + pcollxy(k*xinc, y, direction) * xinc


  } # end of k loop


  ifelse(imax > 0, 2 * risk2, 2*risk)


}
