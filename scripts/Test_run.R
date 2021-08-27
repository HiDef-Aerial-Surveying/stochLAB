###### Simulation
library(dplyr)

site_area <- 5000

num_sitting <- 10000
num_flying <- 600

pch_all <- 0.011
pch_flying <- 0.20


Bird_Data <- Bird_Data %>% dplyr::filter(Species == "Black_legged_Kittiwake")
Bird_Data_all <- Bird_Data_flying <- Bird_Data

Bird_Data_all$Prop_CRH_Obs <- pch_all
Bird_Data_all$Prop_CRH_ObsSD <- 0.001

Bird_Data_flying$Prop_CRH_Obs <- pch_flying
Bird_Data_flying$Prop_CRH_ObsSD <- 0.05


Count_Data <- Count_Data %>% dplyr::filter(Species == "Black_legged_Kittiwake")
Count_Data_all <- Count_Data_flying <- Count_Data

Count_Data_all$Jan <- 10600/site_area
Count_Data_all$JanSD <- 1.5

Count_Data_flying$Jan <- 600/site_area
Count_Data_flying$JanSD <- 0.08



all_analysis <- stochasticBand(results_folder="D:/TEMP",BirdData = Bird_Data_all,CountData = Count_Data_all)
flying_analysis <- stochasticBand(results_folder="D:/TEMP",BirdData = Bird_Data_flying,CountData = Count_Data_flying)


mean(all_analysis$monthCollsnReps_opt1$Black_legged_Kittiwake$turbModel6$Jan)

mean(flying_analysis$monthCollsnReps_opt1$Black_legged_Kittiwake$turbModel6$Jan)
