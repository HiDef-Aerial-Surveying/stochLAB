# Running the CRM in batch mode

library(readxl)
library(foreach)
library(tidyverse)
library(doParallel)
library(stochLAB)

Turbines <- readxl::read_xlsx("D:/Collision_Risk_Modelling/data/Spreadsheet.xlsx",sheet = "TurbineParameters")
Birds <- readxl::read_xlsx("D:/Collision_Risk_Modelling/data/Spreadsheet.xlsx",sheet = "BirdParameters")
Ws_relation <- readxl::read_xlsx("D:/Collision_Risk_Modelling/data/Spreadsheet.xlsx",sheet = "WindSpeedRelation")


Trow <- Turbines[1,]
Brow <- Birds[1,]

Scenario_id <- paste("Turbine_Scenario",Trow$Scenario,"Species_Scenario",Brow$Species_Scenario,sep="_")


BirdDat <- Brow %>% select(Species:AvoidanceExtendedSD)
CountDat <- Brow %>% select(Jan:DecSD)
TurbineDat <- Trow
Ws_relation_rotation <- Ws_relation %>% select(windSpeed,rotationSpeed)
Ws_relation_pitch <- Ws_relation %>% select(windSpeed,bladePitch)


test <- stochasticBand(results_folder = "D:/TEMP/CRM_TEST/",scenario_id = Scenario_id,
               BirdData = BirdDat,TurbineData = TurbineDat,
               CountData = CountDat,CRSpecies = BirdDat$Species,TPower = TurbineDat$Tpower,
               NTurbines = TurbineDat$NTurbines,LargeArrayCorrection = TRUE,
               WFWidth = TurbineDat$Width,
               Prop_Upwind = 0.5,
               Latitude = TurbineDat$Latitude,
               TideOff = TurbineDat$TidalOffset,
               windSpeedMean = TurbineDat$windSpeedMean,
               windSpeedSD = TurbineDat$windSpeedSD,
               windData_rotation = Ws_relation_rotation,
               windData_pitch = Ws_relation_pitch,
               c_densOpt = "truncNorm"
               )




