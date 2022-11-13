library(chillR)
library(reshape2)
library(kableExtra)
library(tidyverse)



#Task 1

# Use chillR functions to find out how many gaps you have in this dataset
# (even if you have none, please still follow all further steps)



Leszno = read.csv("weather_data/Poland_leszno_chillR_weather.csv")


# detected many gaps 

Leszno_QC = fix_weather(Leszno)$QC

kable(Leszno_QC, caption="Quality control summary produced by *fix_weather()*") %>%
  kable_styling("striped", position = "left", font_size = 10)



# Task 2 
# Create a list of the 25 closest weather stations using the handle_gsod function


station_list_close_to_leszno<-handle_gsod(action="list_stations",location=c(16.57,51.85),time_interval=c(1990,2020))

kable(head(station_list_close_to_leszno), caption="List of GSOD weather stations close to Leszno") %>%
  kable_styling("striped", position = "left", font_size = 10)


#Task 4
#Download weather data for promising stations, convert them 
#to chillR format and compile them in a list

positions_in_station_list<-c(4,6,7)

patch_weather<-list()

for(i in 1:length(positions_in_station_list))
{
  patch_weather[[i]] <-
    handle_gsod(
      handle_gsod(
        action = "download_weather",
        location = station_list_close_to_leszno$chillR_code[positions_in_station_list[i]],
        time_interval = c(1990, 2020)
      )
    )[[1]]$weather
  names(patch_weather)[i] <-
    station_list_close_to_leszno$STATION.NAME[positions_in_station_list[i]]
}


#save_temperature_scenarios(patch_weather,"weather_data/", "patch_weather")
patch_weather<-load_temperature_scenarios("weather_data/gepatchtes_Wetter_PL", "patch_weather_pl")


patched<-patch_daily_temperatures(weather = Leszno,
                                  patch_weather = patch_weather)



#Have a look


kable(patched$statistics[[1]],
      caption=paste("Patch statistics for",
                    names(patched$statistics)[1])) %>%
  kable_styling("striped", position = "left", font_size = 10)




kable(patched$statistics[[2]],
      caption=paste("Patch statistics for",
                    names(patched$statistics)[2])) %>%
  kable_styling("striped", position = "left", font_size = 10)



kable(patched$statistics[[3]],
      caption=paste("Patch statistics for",
                    names(patched$statistics)[3])) %>%
  kable_styling("striped", position = "left", font_size = 10)




#Task 5
#Use the patch_daily_temperatures function to fill gaps

patched<-patch_daily_temperatures(weather = Leszno,
                                  patch_weather = patch_weather,
                                  max_mean_bias = 1,
                                  max_stdev_bias = 2)

post_patch_stats<-fix_weather(patched)$QC
Leszno_weather<-fix_weather(patched)