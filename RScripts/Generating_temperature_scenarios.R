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
test_ = handle_gsod(df_lz)

# LZ_weather = data.frame(Year = df_lz$Year,
#                         Month = df_lz$Month,
#                         Day = df_lz$Day,
#                         Tmin = df_lz$Tmin,
#                         Tmax = df_lz$Tmax)



Ltest<-temperature_generation(test_,
                             years=c(1998,2005),
                             sim_years = c(2001,2100))

Temp<-temperature_generation(LZ_weather,
                             years=c(1998,2005),
                             sim_years = c(2001,2100))

Temp<-temperature_generation(KA_weather,
                             years=c(1998,2005),
                             sim_years = c(2001,2100))

Temp<-temperature_generation(KA_weather,
                             years=c(1998,2005),
                             sim_years = c(2001,2100))




Temperatures<-cbind(LZ_weather[
  which(LZ_weather$Year %in% 1998:2005),] ,Data_source="observed")
Temperatures<-rbind(Temperatures,
                    cbind(Ltest[[1]][,c("Year","Month","Day","Tmin","Tmax")],
                          Data_source="simulated"))
Temperatures[,"Date"]<-as.Date(ISOdate(2000,
                                       Temperatures$Month,
                                       Temperatures$Day))


ggplot(data=Temperatures, aes(Date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")



ggplot(data=Temperatures, aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")











chill_observed<-chilling(
  stack_hourly_temps(
    Temperatures[which(Temperatures$Data_source=="observed"),],
    latitude = 50.4),
  Start_JDay = 305,
  End_JDay = 59)
chill_simulated<-chilling(
  stack_hourly_temps(
    Temperatures[which(Temperatures$Data_source=="simulated"),],
    latitude = 50.4),
  Start_JDay = 305,
  End_JDay = 59)

chill_comparison<-cbind(chill_observed ,Data_source="observed")
chill_comparison<-rbind(chill_comparison,
                        cbind(chill_simulated ,Data_source="simulated"))

chill_comparison_full_seasons<-chill_comparison[
  which(chill_comparison$Perc_complete==100),]





ggplot(chill_comparison_full_seasons, aes(x=Chill_portions)) + 
  geom_histogram(binwidth=1,aes(fill = factor(Data_source))) +
  theme_bw(base_size = 20) +
  labs(fill = "Data source") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency")

