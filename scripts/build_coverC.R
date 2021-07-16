##### create dataframe giving the width of the chord relative to its maximum width at given points along the radius of the rotor

rad = round(seq(0,1,0.05),2)

circ = c(0.69,0.73,0.79,0.88,0.96,1,0.98,0.92,0.85,0.8,0.75,0.7,0.64,0.58,0.52,0.47,0.41,0.37,0.3,0.24,0)

coverC = data.frame(cbind(rad, circ))

usethis::use_data(coverC)
