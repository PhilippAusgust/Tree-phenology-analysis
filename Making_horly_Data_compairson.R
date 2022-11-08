# Making hourly Temperature Data compairson 

library(tidyverse)
library(chillR)
library(lubridate)
library(kableExtra)
library(reshape2)

Zuelpich_hourly = read.table("weather_data/Weather_Zuelpich_2019_hourly.csv", header = TRUE, sep = ",")
Zuelpich_min_max= read.table("weather_data/Weather_Zuelpich_2019.csv", header = TRUE, sep = ",")


# Zuelpich hourly 1 to 5 April 2019

zuelpich_april = Zuelpich_hourly%>% filter("2019-04-01 00:00:00"<date)%>%
  filter("2019-04-21 00:00:00"> date)

zuelpich_april$date_new <- as.POSIXct(zuelpich_april[,3])
zuelpich_april$date_newnew = as.Date(zuelpich_april[,3])


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


ggplot(data = zuelpich_april, aes(x = zuelpich_april[, 4], y = zuelpich_april[, 2])) +
  geom_line(size = 1.0, colour = "darkgreen") +
  geom_point(aes(y = zuelpich_april$Tmax_Tmin),
             colour = "red",
             size = 3.0) +
  labs(x = "Date", y = "Temperature (C°)") +
  ggtitle("1 to 20 April 2019 weather station Zuelpich") +
  theme_bw(base_size = 20)












