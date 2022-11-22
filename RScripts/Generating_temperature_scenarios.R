# Generating temperature scenarios

library(kableExtra)
library(reshape2)
library(tidyverse)
library(chillR)
library(lubridate)



# preprocessing data from zuelpich fuessenich 


tmax = read.table("weather_data/data_weather_station_fuessenich/TmaxFuessnich19-22.csv", header = TRUE, sep = ",")
tmin = read.table("weather_data/data_weather_station_fuessenich/TminFuessnich19-22.csv", header = TRUE, sep = ",")

fuessenich = data.frame(
  Year = as.numeric(substr(tmax[, 5], 1, 4)),
  Month = as.numeric(substr(tmax[, 5], 6,7 )),
  Day = as.numeric(substr(tmax[, 5], 9, 10 )),
  Tmax = tmax[,2],
  Tmin = tmin[,2]
)

head(fuessenich)
fuessenich = fuessenich[-nrow(fuessenich),]
fuessenich[nrow(fuessenich),]

#write.csv(fuessenich,"weather_data/data_weather_station_fuessenich/fuessenich_clean.csv")



FUE_weather = fuessenich
tail(KA_weather)




df_lz=read.csv("weather_data/Leszno_weather.csv", header = TRUE)

LZ_weather = data.frame(Year = df_lz$Year,
                        Month = df_lz$Month,
                        Day = df_lz$Day,
                        Tmin = df_lz$Tmin,
                        Tmax = df_lz$Tmax)



Temp<-temperature_generation(LZ_weather,
                             years=c(1998,2005),
                             sim_years = c(2001,2100))




