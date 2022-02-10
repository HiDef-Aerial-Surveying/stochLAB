#' The xrisksum2 function for calculating risk along a turbine blade
#'
#' Single transit collision risk/probability at rotor height bands
#'
#' This function will calculate the risk associated with each point along the surface of a turbine blade
#'
#' @note Modified version of the original code. Changes focused on streamlining
#'    the code and optimizing the performance of the functions involved.
#'    `xrisksum2` and nested functions are called hundreds of times in
#'    each iteration, so performance under option 3 depends heavily on
#'    their execution speed
#'
#' @inheritParams pcollxy
#' @param xinc numeric
#'
#' @export
xrisksum2 <- function (y, xinc, direction, pars) {

  xmax <- (1- y * y) ^ 0.5

  imax <- as.integer (xmax/xinc)

  # avoid repeating calcs - once here for use later
  pcollxy_imax <- pcollxy (imax * xinc, y, direction, pars)

  pcollxy_xmax <- pcollxy(xmax, y, direction, pars)
  pcollxy_0 <- pcollxy(0, y, direction, pars)

  #
  risk <- (pcollxy_imax/2 + pcollxy_xmax/2) * (xmax - imax * xinc)
  risk2 <- risk + (pcollxy_0/2 + pcollxy_imax/2) * xinc

  # loop over k
  for (k in 1: (imax - 1)) {


    risk2 <- risk2 + pcollxy(k*xinc, y, direction, pars) * xinc


  } # end of k loop


  ifelse(imax > 0, 2*risk2, 2*risk)
}






# pcollxy - called within xrisksum2 -----------------------------------------
#' In Masden construction, relies on various objects from outside function
#' These will be passed explicitly:
#' sampledTurbine `[ith row]`
#' sampledBirdParams `[ith row]`
#' @param x,y,direction,pars numeric

pcollxy <- function(x, y, direction, pars) {

  r <- (x * x + y * y) ^ 0.5

  #if(r > 1) browser()

  phi <- ifelse ( y == 0,

                  ifelse( x >= 0, pi /2, -pi/2),

                  atan (x/y))

  phi2 <- ifelse ( y < 0, phi + pi , phi)

  phi2 = phi2 * 180 / pi

  pcoll(r, phi2, direction,
        rotor_radius = pars$rotor_radius,
        blade_width = pars$blade_width,
        rotor_speed = pars$rotor_speed,
        blade_pitch = pars$blade_pitch,
        flight_type_num = pars$flight_type_num,
        wing_span = pars$wing_span,
        flight_speed = pars$flight_speed,
        body_lt = pars$body_lt,
        n_blades = pars$n_blades)
}



#' prob of collision, called within xrisksum2 and pcollxy
#'
#'
#' @param r,phi,direction,rotor_radius numeric TODO
#' @param blade_width,rotor_speed numeric TODO
#' @param blade_pitch,flight_type_num numeric TODO
#' @param wing_span,flight_speed,body_lt,n_blades numeric TODO
pcoll <- function (r, phi, direction, rotor_radius, blade_width, rotor_speed,
                  blade_pitch, flight_type_num, wing_span, flight_speed,
                  body_lt, n_blades) {

  inputRad <- round(seq(0,1,0.05),2)
  inputCirc <- c(0.69,0.73,0.79,0.88,0.96,1,0.98,0.92,0.85,0.8,0.75,0.7,0.64,0.58,0.52,0.47,0.41,0.37,0.3,0.24,0)

  #cell_val <- ifelse (r > 1, 21, which(inputRad == (round(ceiling(r*100)/5) * 5)/100) ) # ERROR!
  cell_val <- ifelse (r > 1, 21, which(inputRad == round(ceiling(r/0.05)*0.05, 2)))
  upper <- inputRad[cell_val]
  lower <- inputRad[cell_val - 1]

  p <- (r - lower) / (upper-lower)

  c <- inputCirc[cell_val-1] + p * (inputCirc[cell_val] - inputCirc[cell_val-1])

  radius <- rotor_radius * r
  chord <- blade_width * c
  omega <- rotor_speed * 2 * pi /60   # conversion to angular velocity, i.e. radians/sec
  #pitch <- blade_pitch * pi /180     # ERROR!!! Pitch is already in radians, from sample_turbine()
  pitch <- blade_pitch
  phi <- phi * pi /180


  if(flight_type_num == 0) {
    wing_span <- wing_span * abs(cos(phi))
  }


  multiplier <- n_blades * omega /(2 * pi * flight_speed)

  alpha <- flight_speed/(radius * omega)

  CollideLength_1 <- abs(direction * chord * sin(pitch) + alpha * chord * cos(pitch))

  CollideLength2 <- ifelse(body_lt > wing_span * alpha,

                           body_lt, wing_span * alpha)


  ifelse(radius == 0, 1,  (multiplier * (CollideLength_1 + CollideLength2)))

}
