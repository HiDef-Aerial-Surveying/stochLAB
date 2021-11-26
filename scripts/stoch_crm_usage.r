tpower <-  1760
nturbs <- round (tpower/Turbine_Data$TurbineModel)

set.seed(188)
stoch_crm(model_options = c(1, 2, 3),
          BirdData = Bird_Data[3, ],
          TurbineData = Turbine_Data,
          CountData = Count_Data[3, ],
          iter = 10,
          spp_name = c("Black_legged_Kittiwake"),
          LargeArrayCorrection = TRUE,
          n_turbines = nturbs,
          WFWidth = wf_width,
          Prop_Upwind = 0.5,
          Latitude = 56,
          TideOff = 2.5,
          windSpeedMean = 30,
          windSpeedSD = 5.1,
          windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
          windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
          dens_opt = "truncNorm",
          fhd_bootstraps = generic_fhd_bootstraps$Black_headed_Gull)




# BC: Note, there will be divergences between stoch_crm() and stochasticBand()
# outputs, as there has been a couple of corrections (in LACF and option 3's pcoll)
# applied to concur with Band original calculations


set.seed(188)
stochasticBand(
  results_folder = "c:/test",
  BirdData = Bird_Data,
  TurbineData = Turbine_Data,
  CountData = Count_Data,
  FlightData = Flight_Data,
  iter = 10,
  CRSpecies = c("Black_legged_Kittiwake"),
  TPower = tpower,
  LargeArrayCorrection = TRUE,
  WFWidth = wf_width,
  Prop_Upwind = 0.5,
  Latitude = 56,
  TideOff = 2.5,
  windSpeedMean = 30,
  windSpeedSD = 5.1,
  windData_rotation = startUpValues$turbinePars$rotationVsWind_df,
  windData_pitch = startUpValues$turbinePars$pitchVsWind_df,
  c_densOpt = "truncNorm")$monthCollsnReps_opt1$Black_legged_Kittiwake$turbModel6


