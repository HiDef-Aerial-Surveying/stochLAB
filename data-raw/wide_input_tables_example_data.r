# Script for generating rda files with examples of multiple scenario input data
# stored in wide tables. This is to provide internal data for example code to
# reshape these datasets in conformity with stoch_crm() interface

# Fake data for 3 species
bird_pars_wide_example <- read.csv("data-raw/bird_pars_wide_example.csv")
dens_tnorm_wide_example <- read.csv("data-raw/bird_dens_tnorm_wide_example.csv")

# Fake data for 3 windfarm scenarios
turb_pars_wide_example <- read.csv("data-raw/turbine_pars_wide_example.csv")



wndspd_rtn_ptch_example <- read.csv("data-raw/windspeed_rtn_pitch_data_example.csv")


usethis::use_data(bird_pars_wide_example, overwrite = TRUE)
usethis::use_data(dens_tnorm_wide_example, overwrite = TRUE)
usethis::use_data(turb_pars_wide_example, overwrite = TRUE)
usethis::use_data(wndspd_rtn_ptch_example, overwrite = TRUE)
