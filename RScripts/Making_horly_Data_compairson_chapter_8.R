# Making hourly Temperature Data compairson 

library(tidyverse)
library(chillR)
library(lubridate)
library(kableExtra)
library(reshape2)

Zuelpich_hourly = read.table("weather_data/Weather_Zuelpich_2019_hourly.csv", header = TRUE, sep = ",")
Zuelpich_min_max= read.table("weather_data/Weather_Zuelpich_2019.csv", header = TRUE, sep = ",")


# Zuelpich hourly 1 to 5 April 2019

zuelpich_april = Zuelpich_hourly %>% filter("2019-04-01 00:00:00" < date) %>%
  filter("2019-04-05 00:00:00" > date)

zuelpich_april$date_new <- as.POSIXct(zuelpich_april[, 3])
zuelpich_april$date_newnew = as.Date(zuelpich_april[, 3])

# get subset with day temp max and temp min from hour avarage over day 
final <- zuelpich_april %>%
  group_by(Tag = day(date_newnew)) %>%
  summarise(
    Mittel =  round(mean(temperature, na.rm = TRUE), digits = 1),
    Tmax = max(temperature),
    Tmin = min(temperature)
  )
final


#NAs generieren

zuelpich_april$Tmax_Tmin = NA




# Indizies generieren 
# T min zuordnen 

for (i in seq(1, nrow(zuelpich_april))) {
  for (j in seq(1, nrow(final))) {
    if (zuelpich_april$temperature[i] == final$Tmax[j]) {
      zuelpich_april$Tmax_Tmin[i] <- final$Tmax[j]
    }
  }
}

# Tmax zuordnen 

for(i in seq(1, nrow(zuelpich_april))) {
  for (j in seq(1, nrow(final))) {
    if (zuelpich_april$temperature[i] == final$Tmin[j]) {
      zuelpich_april$Tmax_Tmin[i] <- final$Tmin[j]
    }
  }
}

# plot original measured data 
ggplot(data = zuelpich_april, aes(x = zuelpich_april[, 4], y = zuelpich_april[, 2])) +
  geom_line(size = 1.0, colour = "darkgreen") +
  geom_point(aes(y = zuelpich_april$Tmax_Tmin),
             colour = "red",
             size = 3.0) +
  labs(x = "Date", y = "Temperature (C°)") +
  ggtitle("1 to 5 April 2019 weather station Zuelpich") +
  theme_bw(base_size = 20)

# create dataframe for linear interpolation 

ZU_weather = data.frame(
  DATE = zuelpich_april[, 4],
  Year = as.numeric(substr(zuelpich_april[, 4], 1, 4)),
  Month = as.numeric(substr(zuelpich_april[, 4], 6, 7)),
  Day = as.numeric(substr(zuelpich_april[, 4], 9, 10)),
  Tcontinue = zuelpich_april[, 2],
  Temp_inter = zuelpich_april[, 6]
)

# lineare Interpolation 
ZU_weather$Temp_inter<-interpolate_gaps(ZU_weather$Temp_inter)[[1]]
dummy = interpolate_gaps(ZU_weather$Temp_inter)





# nicht lineare Interpolation preprocessing

Zuelpich_min_max = Zuelpich_min_max%>% filter("2019-03-31 00:00:00"<date)%>%
  filter("2019-04-05 00:00:00"> date)


# datafram erstellen für nicht lineare interpolation 
ZU_weather_min_max = data.frame(
  Year = as.numeric(substr(Zuelpich_min_max[, 2], 1, 4)),
  Month = as.numeric(substr(Zuelpich_min_max[, 2], 6, 7)),
  Day = as.numeric(substr(Zuelpich_min_max[, 2], 9, 10)),
  Tmax = final[, 3],
  Tmin = final[, 4]
)




#Interpolation

ZU_hourly<-stack_hourly_temps(ZU_weather_min_max, latitude=50.4)

# Add DATE as readable Date 
ZU_hourly$hourtemps[,"DATE"]<-ISOdate(ZU_hourly$hourtemps$Year,ZU_hourly$hourtemps$Month, ZU_hourly$hourtemps$Day, ZU_hourly$hourtemps$Hour)


# Show Result non linear Interpolation 
ggplot(ZU_hourly$hourtemps,aes(DATE,Temp))+geom_line(lwd=1.5)+xlab("Date")+ylab("Temperature (°C)") +
  theme_bw(base_size = 20)


# Längen der Interpolierten Temperaturen messen und herausfinden, warum die nicht lineare 
# Interpolation etwas anders läuft 

ZU_hourly_mod = ZU_hourly[[1]][-1,]



library(writexl)

final_df = data.frame(
  DATE = zuelpich_april[, 4], 
  Measured_Temp = zuelpich_april[, 2],
  Linear_Interp  = ZU_weather[, 6],
  Non_Linear_Interp = ZU_hourly_mod[, 8]
)




final_df_mod  = pivot_longer(final_df, -"DATE", names_to = "Method", values_to = "Temperature")

# final plot 

ggplot(data = final_df_mod, aes(x = DATE, y = Temperature, colour = Method)) +
  geom_line(lwd = 1.3) +
  labs(x = "Date", y = "Temperature (C°)") +
  ggtitle("1 to 5 April 2019  Zuelpich") +
  scale_color_manual(values = c("red", "darkgreen", "darkblue")) +
  facet_wrap(vars(Method))+
  theme_bw(base_size = 15)




