# This script generates the chord taper profile based on the blade of a typical
# 5 MW turbine used for offshore generation, as explained in the Band documentation.
# Data extracted from the Band worksheet

chord_prof_5MW <- data.frame(
  # radius at bird passage point, as a proportion of rotor radius (R)
  pp_radius = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50,
                0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00),
  # chord width at pp_radius, as a proportion of the maximum chord width
  chord = c(0.69, 0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85, 0.80, 0.75,
            0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30,0.24,0.00)
)

usethis::use_data(chord_prof_5MW, overwrite = TRUE, compress = "xz")
