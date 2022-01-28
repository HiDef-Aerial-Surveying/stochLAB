
# parameter values --------------------------
# test

FlapGlide <- "Flapping"
tpower <-  1760
nturbs <- round (tpower/Turbine_Data$TurbineModel)

latitude  <- 53.7
pcollision <- 8.059731
meanOp <- 87.49717
wf_width <- 4

birdPars <- tibble::tibble(
  AvoidanceBasic = 0.9899236,
  AvoidanceExtended = 0.9639835,
  WingSpan = 1.72,
  BodyLength = 0.94,
  PCH = 0.05109559,
  FlightSpeed = 14.9,
  NocturnalActivity = 0.03322674
)

turbPars <- tibble::tibble(
  RotorRadius = 57.5,
  HubHeight = 106.5,
  BladeWidth = 4.21,
  WindSpeed = 0,
  RotorSpeed = 9.9,
  Pitch = 15 * pi/180,
  JanOp = 86.10582,
  FebOp = 92.75517,
  MarOp = 88.65776,
  AprOp = 86.63098,
  MayOp = 84.99313,
  JunOp = 88.84497,
  JulOp = 80.58533,
  AugOp = 84.94592,
  SepOp = 88.0998,
  OctOp = 84.36208,
  NovOp = 89.03661,
  DecOp = 92.02299
)


bird_counts <- tibble::tibble(
  Jan = 0.97,
  Feb = 1.04,
  Mar = 1.15,
  Apr = 0.48,
  May = 0.56,
  Jun = 0.63,
  Jul = 0.68,
  Aug = 0.64,
  Sep = 0.53,
  Oct = 1.2,
  Nov = 1.02,
  Dec = 0.99
  )

ch_prf <- data.frame(
  # radius at bird passage point, as a proportion of rotor radius (R)
  pp_radius = seq(0.05, 1, 0.05),
  # chord width at pp_radius, as a proportion of the maximum chord width
  chord = c(0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85, 0.80, 0.75,
            0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30,0.24,0.00)
)




# function for probability of single rotor transit collision --------------------------

prob_new <- get_prob_collision(
  chord_prof = ch_prf,
  flight_speed = birdPars$FlightSpeed,
  rotor_speed = turbPars$RotorSpeed,
  rotor_radius = turbPars$RotorRadius,
  body_lt = birdPars$BodyLength,
  wing_span = birdPars$WingSpan,
  blade_width = turbPars$BladeWidth,
  blade_pitch = turbPars$Pitch,
  n_blades = Turbine_Data$Blades,
  prop_upwind= 0.5,
  flap_glide = ifelse(FlapGlide == "Flapping", 1, 2/pi)) * 100


prob_old <- print(probability_collision_no_avoid(sampledBirdParams=birdPars,
                                     sampledTurbine=turbPars,
                                     TurbineData=Turbine_Data,
                                     Prop_Upwind=0.5,
                                     Flap_Glide = ifelse(FlapGlide == "Flapping", 1, 2/pi)))

all.equal(prob_new, prob_old)


# Test computation speed
bench::mark(
  coll_prob_GH = probability_collision_no_avoid(
    sampledBirdParams=birdPars,
    sampledTurbine=turbPars,
    TurbineData=Turbine_Data,
    Prop_Upwind=0.5,
    Flap_Glide = ifelse(FlapGlide == "Flapping", 1, 2/pi)),
  coll_prob_BC = get_prob_collision(
    chord_prof = ch_prf,
    flight_speed = birdPars$FlightSpeed,
    rotor_speed = turbPars$RotorSpeed,
    rotor_radius = turbPars$RotorRadius,
    body_lt = birdPars$BodyLength,
    wing_span = birdPars$WingSpan,
    blade_width = turbPars$BladeWidth,
    blade_pitch = turbPars$Pitch,
    n_blades = Turbine_Data$Blades,
    prop_upwind= 0.5,
    flap_glide = ifelse(FlapGlide == "Flapping", 1, 2/pi)) * 100
)




# flux/transits functions ------------------------------------------------------

t_hours <- DayLength(latitude)

flux_GH <- initial_flux(
  NTurbines=nturbs,
  sampledTurbine=turbPars,
  sampledBirdParams=birdPars,
  sampledSpeciesCount = bird_counts,
  TPower=tpower,
  hours=t_hours)

flux_BC <- get_flux_factor(
  n_turbines = nturbs,
  rotor_radius = turbPars$RotorRadius,
  flight_speed = birdPars$FlightSpeed,
  bird_dens = bird_counts,
  daynight_hrs = t_hours,
  noct_activity = birdPars$NocturnalActivity)


all.equal(unlist(flux_GH$Flux), unlist(flux_BC, use.names = FALSE))

# test performance
bench::mark(
  check = FALSE,
  flux_GH = initial_flux(
    NTurbines=nturbs,
    sampledTurbine=turbPars,
    sampledBirdParams=birdPars,
    sampledSpeciesCount = bird_counts,
    TPower=tpower,
    hours=t_hours),
  flux_BC = get_flux_factor(
    n_turbines = nturbs,
    rotor_radius = turbPars$RotorRadius,
    flight_speed = birdPars$FlightSpeed,
    bird_dens = bird_counts,
    daynight_hrs = t_hours,
    noct_activity = birdPars$NocturnalActivity)
)


# LAC functions ----------------------------------------------------------------

lacf_GH <- large_array_correction(
  NTurbines = nturbs,
  sampledTurbine = turbPars,
  sampledBirdParams = birdPars,
  P_Collision = pcollision,
  MeanOperational = meanOp,
  WFWidth = wf_width)


lacf_BC <- get_lac_factor(
  n_turbines = nturbs,
  rotor_radius = turbPars$RotorRadius,
  avoidance_rate = birdPars$AvoidanceBasic,
  prob_single_collision = pcollision/100,
  mean_prop_operational = meanOp/100,
  wf_width = wf_width
)

all.equal(lacf_GH, lacf_BC)
# not equal due to correction of error in original code
# (see get_lac_factor source code)

bench::mark(
  check = FALSE,
  lacf_GH = large_array_correction(
    NTurbines = nturbs,
    sampledTurbine = turbPars,
    sampledBirdParams = birdPars,
    P_Collision = pcollision,
    MeanOperational = meanOp,
    WFWidth = wf_width),
  lacf_BC = get_lac_factor(
    n_turbines = nturbs,
    rotor_radius = turbPars$RotorRadius,
    avoidance_rate = birdPars$AvoidanceBasic,
    prob_single_collision = pcollision/100,
    mean_prop_operational = meanOp/100,
    wf_width = wf_width),
  )



# Option 1 collisions functions ----------------------------------------------------------------

opt1_GH <- sCRM_option1(
  MonthlyOperational = turbPars %>% select(JanOp:DecOp),
  hours = flux_fact,
  PCH = birdPars$PCH,
  P_Collision = pcollision,
  AvoidanceBasic = birdPars$AvoidanceBasic,
  LargeArrayCorrection = TRUE,
  L_ArrayCF = lacf_BC)$Collisions


opt1_BC <- crm_opt1(
  flux_factor = flux_BC,
  prop_crh = birdPars$PCH,
  prob_single_collision = pcollision/100,
  prop_operational = turbPars %>% select(JanOp:DecOp)/100,
  avoidance_rate = birdPars$AvoidanceBasic,
  lac_factor = lacf_BC)


all.equal(opt1_GH, unlist(opt1_BC, use.names = FALSE))


# not comparable, really, as sCRM_option1 uses pre-calculated flux, while crm_opt1
# calculates flux on the fly
bench::mark(
  opt1_GH = sCRM_option1(
    MonthlyOperational = turbPars %>% select(JanOp:DecOp),
    hours = flux_fact,
    PCH = birdPars$PCH,
    P_Collision = pcollision,
    AvoidanceBasic = birdPars$AvoidanceBasic,
    LargeArrayCorrection = TRUE,
    L_ArrayCF = lacf_BC)$Collisions,

  opt1_BC = unlist(crm_opt1(
    flux_factor = flux_BC,
    prop_crh = birdPars$PCH,
    prob_single_collision = pcollision/100,
    prop_operational = turbPars %>% select(JanOp:DecOp)/100,
    avoidance_rate = birdPars$AvoidanceBasic,
    lac_factor = lacf_BC), use.names = FALSE)

)




# Main function for Option 1  ----------------------------------------------------------------------------------------------

set.seed(10)
system.time(
coll_opt1_GH <- stochasticBand(
  results_folder = "c:/test",
  BirdData = Bird_Data,
  TurbineData = Turbine_Data,
  CountData = Count_Data,
  FlightData = Flight_Data,
  iter = 1000,
  CRSpecies = c("Black_legged_Kittiwake"),
  TPower = tpower,
  LargeArrayCorrection = TRUE,
  WFWidth = wf_width,
  Prop_Upwind = 0.5,
  Latitude = latitude,
  TideOff = 2.5,
  windSpeedMean = 30,
  windSpeedSD = 5.1,
  windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
  windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
  #updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
  #updateProgress_Iter,
  c_densOpt = "truncNorm"
  #DensityOpt = list(userOption = "truncNorm")
)$monthCollsnReps_opt1$Black_legged_Kittiwake$turbModel6
)



set.seed(10)
system.time(
coll_opt1_BC <- stoch_crm(model_options = c(1),
          BirdData = Bird_Data[3, ],
          TurbineData = Turbine_Data,
          CountData = Count_Data[3, ],
          iter = 1000,
          spp_name = c("Black_legged_Kittiwake"),
          LargeArrayCorrection = TRUE,
          n_turbines = nturbs,
          WFWidth = wf_width,
          Prop_Upwind = 0.5,
          Latitude = latitude,
          TideOff = 2.5,
          windSpeedMean = 30,
          windSpeedSD = 5.1,
          windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
          windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
          dens_opt = "truncNorm")$opt1
)


# not equal due to correction of errors in original LAC and pcoll calculations
# (see get_lac_factor source code)
all.equal(coll_opt1_GH, as.data.frame(coll_opt1_BC))

bench::mark(
  check = FALSE,
  coll_opt1_GH = {
    stochasticBand(
      results_folder = "c:/test",
      BirdData = Bird_Data,
      TurbineData = Turbine_Data,
      CountData = Count_Data,
      FlightData = Flight_Data,
      iter = 1000,
      CRSpecies = c("Black_legged_Kittiwake"),
      TPower = tpower,
      LargeArrayCorrection = TRUE,
      WFWidth = wf_width,
      Prop_Upwind = 0.5,
      Latitude = latitude,
      TideOff = 2.5,
      windSpeedMean = 30,
      windSpeedSD = 5.1,
      windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
      windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
      #updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
      #updateProgress_Iter,
      c_densOpt = "truncNorm"
      #DensityOpt = list(userOption = "truncNorm")
    )$monthCollsnReps_opt1$Black_legged_Kittiwake$turbModel6
  },

  coll_opt1_BC = {
    stoch_crm(model_options = c(1),
              BirdData = Bird_Data[3, ],
              TurbineData = Turbine_Data,
              CountData = Count_Data[3, ],
              iter = 1000,
              spp_name = c("Black_legged_Kittiwake"),
              LargeArrayCorrection = TRUE,
              n_turbines = nturbs,
              WFWidth = wf_width,
              Prop_Upwind = 0.5,
              Latitude = latitude,
              TideOff = 2.5,
              windSpeedMean = 30,
              windSpeedSD = 5.1,
              windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
              windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
              dens_opt = "truncNorm")$opt1
  }
)





# Test performance of Option 1 vs Option 2 Vs Option 3 -------------------------
bench::mark(check = FALSE,
  opt1 = stoch_crm(model_options = c(1),
            BirdData = Bird_Data[3, ],
            TurbineData = Turbine_Data,
            CountData = Count_Data[3, ],
            iter = 10,
            spp_name = c("Black_legged_Kittiwake"),
            LargeArrayCorrection = TRUE,
            n_turbines = nturbs,
            WFWidth = wf_width,
            Prop_Upwind = 0.5,
            Latitude = latitude,
            TideOff = 2.5,
            windSpeedMean = 30,
            windSpeedSD = 5.1,
            windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
            windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
            dens_opt = "truncNorm",
            fhd_bootstraps = generic_fhd_bootstraps$Black_headed_Gull),

  opt2 = stoch_crm(model_options = c(2),
                   BirdData = Bird_Data[3, ],
                   TurbineData = Turbine_Data,
                   CountData = Count_Data[3, ],
                   iter = 10,
                   spp_name = c("Black_legged_Kittiwake"),
                   LargeArrayCorrection = TRUE,
                   n_turbines = nturbs,
                   WFWidth = wf_width,
                   Prop_Upwind = 0.5,
                   Latitude = latitude,
                   TideOff = 2.5,
                   windSpeedMean = 30,
                   windSpeedSD = 5.1,
                   windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
                   windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
                   dens_opt = "truncNorm",
                   fhd_bootstraps = generic_fhd_bootstraps$Black_headed_Gull),

  opt3 = stoch_crm(model_options = c(3),
            BirdData = Bird_Data[3, ],
            TurbineData = Turbine_Data,
            CountData = Count_Data[3, ],
            iter = 10,
            spp_name = c("Black_legged_Kittiwake"),
            LargeArrayCorrection = TRUE,
            n_turbines = nturbs,
            WFWidth = wf_width,
            Prop_Upwind = 0.5,
            Latitude = latitude,
            TideOff = 2.5,
            windSpeedMean = 30,
            windSpeedSD = 5.1,
            windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
            windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
            dens_opt = "truncNorm",
            fhd_bootstraps = generic_fhd_bootstraps$Black_headed_Gull)

)







# --------------------------------------


n_transits <- get_rotor_transits(
  n_turbines = 150,
  rotor_rad = 63,
  flight_speed = 10.5,
  bird_dens = c(0.1128, 0.47, 1.49, 1.40, 0.88, 1.54, 0.57, 1.80, 2.79, 0.83,
                0.50, 0.12),
  daynight_hrs = dn_h,
  noct_activity = 0.3,
  prop_crh =0.281)

get_collisions_basic(
  n_transits = n_transits,
  prob_single_collision = 0.5,
  prop_time_operating = rbeta_dmp(12, 0.8, 0.3),
  avoidance_rate = 0.99,
  lac_factor = 0.3)







#--------------------------------------------------------

gannet_fhd_band_spreadsheet <- readr::read_csv("data-raw/gannet_fhd_Band_spreadsheet.csv")
kitti_fhd_band_spreadsheet <- readr::read_csv("data-raw/kittiwake_fhd_Band_comparison_spreadsheet.csv")

d_y_gannet <- get_fhd_rotor(
  hub_height = 75,
  fhd = generic_fhd_bootstraps$Black_headed_Gull$bootId_3,
  #fhd = kitti_fhd_band_spreadsheet$prop,
  #fhd = gannet_fhd_band_spreadsheet$prop,
  rotor_radius = 50,
  tide_off = 2.5)

get_prop_crh_fhd(d_y_gannet)



get_lac_factor(n_turbines = 144,
               rotor_radius = 50,
               avoidance_rate = 0.975,
               prob_single_collision = 0.15,
               mean_prop_operational = 0.9,
               wf_width = 6)


crm()

crm_opt3(
  gen_d_y = band_spreadsheet_dt$d_y,
  rotor_radius = band_spreadsheet_dt$rotor_radius,
  blade_width = band_spreadsheet_dt$blade_width,
  rotor_speed = band_spreadsheet_dt$rotor_speed,
  blade_pitch = band_spreadsheet_dt$blade_pitch,
  flight_type_num = ifelse(band_spreadsheet_dt$flight_type == "Flapping", 1, 0),
  wing_span = band_spreadsheet_dt$wing_span,
  flight_speed = band_spreadsheet_dt$flight_speed,
  body_lt = band_spreadsheet_dt$body_length,
  n_blades = band_spreadsheet_dt$n_blades,
  prop_upwind = band_spreadsheet_dt$prop_upwd,
  avoidance_rate_ext = band_spreadsheet_dt$avoid_rate_ext,
  flux_factor = band_spreadsheet_dt$flux_fct,
  prop_operational = band_spreadsheet_dt$prop_oper,
  lac_factor = band_spreadsheet_dt$lac_factor
)






pars_test <- list(rotor_radius = band_spreadsheet_dt$rotor_radius,
                  blade_width = band_spreadsheet_dt$blade_width,
                  rotor_speed = band_spreadsheet_dt$rotor_speed,
                  blade_pitch = band_spreadsheet_dt$blade_pitch,
                  flight_type_num = ifelse(band_spreadsheet_dt$flight_type == "Flapping", 1, 0),
                  wing_span = band_spreadsheet_dt$wing_span,
                  flight_speed = band_spreadsheet_dt$flight_speed,
                  body_lt = band_spreadsheet_dt$body_length,
                  n_blades = band_spreadsheet_dt$n_blades)






