require(chillR)
require(ggplot2)
require(reshape2)
require(kableExtra)
#Temperature_scenarios<-read_tab("data/Temperature_scenarios.csv")
Temperature_scenarios[,"Date"]<-as.Date(ISOdate(2000, Temperature_scenarios$Month, Temperature_scenarios$Day))
Bonn_temps<-read_tab("data/Bonn_temps.csv")
#chill_hist_scenario_list<-load_temperature_scenarios("data","chill_hist_scenario_list_")






getClimateWizardData(
  coordinates = c(longitude = 10.61, latitude = 34.93),
  scenario = "rcp45",
  start_year = 2020,
  end_year = 2050,
  metric = c("CD18", "R02"),
  GCMs = c("bcc-csm1-1", "BNU-ESM")
)


RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)



# 
# for(RCP in RCPs)
#   for(Time in Times)
#   {start_year <- Time-15
#   end_year <- Time+15
#   clim_scen <-getClimateWizardData(
#     c(longitude = 7.143,latitude = 50.866),
#     RCP,
#     start_year,
#     end_year,
#     temperature_generation_scenarios = TRUE,
#     baseline =c(1975, 2005),
#     metric = "monthly_min_max_temps",
#     GCMs = "all")
#   save_temperature_scenarios(clim_scen,
#                              "weather_data/ClimateWizard",
#                              paste0("Bonn_futures_",Time,"_",RCP))}


Bonn_temps = read.csv("weather_data/Bonn_temps.csv", header = TRUE)

scenario_1990<-temperature_scenario_from_records(Bonn_temps[,3:8],1990)
scenario_1996<-temperature_scenario_from_records(Bonn_temps,1996)
adjustment_scenario<-temperature_scenario_baseline_adjustment(scenario_1996,scenario_1990)






for(RCP in RCPs)
  for(Time in Times)
  {
    clim_scen<-load_ClimateWizard_scenarios(
      "weather_data/ClimateWizard",
      paste0("Bonn_futures_",Time,"_",RCP))
    clim_scen_adjusted<-
      temperature_scenario_baseline_adjustment(
        baseline_temperature_scenario=adjustment_scenario,
        temperature_scenario=clim_scen)
    Temps<-temperature_generation(
      weather=Bonn_temps, 
      years=c(1973,2019),
      sim_years=c(2001,2101),
      temperature_scenario = clim_scen_adjusted)
    
    save_temperature_scenarios(
      Temps,
      "weather_data/Weather",
      paste0("Bonn_",Time,"_",RCP))
  }

