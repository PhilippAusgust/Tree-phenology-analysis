require(chillR)
require(tidyverse)
require(kableExtra)
require(reshape2)



# Task 1
station_list_poland = handle_gsod(
  action = "list_stations",
  location = c(16.5, 51.39),
  time_interval = c(1990, 2020)
)
kable(station_list_poland[1:10, ]) %>% kable_styling("striped", position = "left", font_size = 10)


# Task 2 




weather_poland_leszno <- handle_gsod(
  action = "download_weather",
  location = station_list_poland$chillR_code[7],
  time_interval = c(1990, 2020)
)


kable(weather_poland_leszno[[1]][[2]][1:10, ]) %>%
  kable_styling("striped", position = "left", font_size = 10)



# Task 3

weather_pl <- weather_poland_leszno$LESZNO[[2]]
cleaned_weather_pl <- handle_gsod(weather_pl)


kable(cleaned_weather_pl[1:20,]) %>%
  kable_styling("striped", position = "left", font_size = 10)



write.csv(station_list_poland,"weather_data/Poland_station_list.csv",row.names=FALSE)
write.csv(weather_poland_leszno[[1]][[2]],"weather_data/Poland_leszno_weather.csv",row.names=FALSE)
write.csv(cleaned_weather_pl,"weather_data/Poland_leszno_chillR_weather.csv",row.names=FALSE)




#______________________________________________________________________________

#Additional Material

### handle DWD

station_list_dwd<-handle_dwd(action="list_stations",
                             location=c(7.10,50.73),
                             time_interval=c(19900101,20201231))
head(station_list_dwd)
weather_dwd<-handle_dwd(action="download_weather",
                        location=station_list_dwd$Station_ID[2],
                        time_interval=c(19900101,20201231))
head(weather_dwd$`Königswinter-Heiderhof`$weather)
cleaned_weather_dwd<-handle_dwd(weather_dwd)

cleaned_weather_dwd$`Königswinter-Heiderhof`

