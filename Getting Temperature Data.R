require(chillR)
require(tidyverse)
require(kableExtra)
require(reshape2)





station_list_ex = handle_gsod(action="list_stations",
                           location=c(7.10,50.73),
                           time_interval=c(1990,2020))


station_list_ex


weather<-handle_gsod(action="download_weather",
                     location=station_list_ex$chillR_code[4],
                     time_interval=c(1990,2020))

weather

cleaned_weather<-handle_gsod(weather)



station_list_poland = handle_gsod(action="list_stations",
                                  location=c(16.5,51.39),
                                  time_interval=c(1990,2020))

station_list_poland

# write.csv(station_list_poland, "weather_data/station.csv")
station_list_poland[1]

#global summuraise of day function handle_gsod

weather_poland<-handle_gsod(action="download_weather",
                            location=station_list_poland$chillR_code[7],
                            time_interval=c(1990,2020))
?handle_gsod
# Get Weather from LESZNO
str(weather_poland)


setwd("")

str(weather_poland)

weather_poland$LESZNO[[2]][1:20,]

weather_pl <- weather_poland$LESZNO[[2]]

cleaned_weather_pl<-handle_gsod(weather_pl)
cleaned_weather_pl[1:20,]

weather_poland[[1]][[2]][1:20,]
cleaned_weather_pl[1:365,]
# dir.create("weather_data")
# write_csv(cleaned_weather_pl, "weather_data/weather_pl.csv")

kable(cleaned_weather_pl[1:20,]) %>%
  kable_styling("striped", position = "left", font_size = 10)


# German weather stations 
station_list_dwd <- handle_dwd(action = "list_stations",
                               location = c(7.10,50.73),
                               time_interval = c(19900101,20201231))

station_list_dwd[2]

station_list_dwd_ <- handle_dwd(action = "download_weather",
                               location = station_list_dwd$Station_ID[2],
                               time_interval = c(19900101,20211231))

cean_dwd <- handle_dwd(station_list_dwd_)

