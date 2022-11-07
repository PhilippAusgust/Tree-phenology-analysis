# 07.11.2022

# Filling gaps in temperature Data


library(chillR)
library(tidyverse)
library(reshape2)
library(kableExtra)
library(dplyr)


weather <- make_all_day_table(KA_weather)
Tmin_int <- interpolate_gaps(KA_weather[, "Tmin"])
weather[, "Tmin"] <- Tmin_int$interp
weather[, "Tmin_interpolated"] <- Tmin_int$missing

Tmax_int <- interpolate_gaps(KA_weather[, "Tmax"])
weather[, "Tmax"] <- Tmax_int$interp
weather[, "Tmax_interpolated"] <- Tmax_int$missing


KA_weather_gap <-
  rbind(KA_weather, c(
    Year = 2011,
    Month = 3,
    Day = 3,
    Tmax = 26,
    Tmin = 14
  ))
fixed_winter_days <-
  fix_weather(
    KA_weather_gap,
    start_year = 2000,
    end_year = 2011,
    start_date = 300,
    end_date = 100
  )
# look on effect of fix_weather
df_test = fixed_winter_days[[1]][3780:4080, ]
plot(x = df_test[, 1], y = df_test[, 6])



fixed_all_days <- fix_weather(KA_weather_gap)


# look on effect of fix_weather
df_test2 = fixed_all_days[[1]][4510:4810, ]
plot(x = df_test2[, 1], y = df_test2[, 6])


gap_weather <- KA_weather[200:305, ]
gap_weather[, "Tmin_observed"] <- gap_weather$Tmin
gap_weather$Tmin[c(2,
                   4:5,
                   7:9,
                   11:14,
                   16:20,
                   22:27,
                   29:35,
                   37:44,
                   46:54,
                   56:65,
                   67:77,
                   79:90,
                   92:104)] <- NA

fixed_gaps<-fix_weather(gap_weather)$weather


ggplot(data = fixed_gaps, aes(DATE, Tmin_observed))+ 
  geom_line(lwd = 1.3) + xlab("Date")+ 
  ylab("Daily minimum temperature (°C)") +
  geom_line(data=fixed_gaps,aes(DATE,Tmin),col="red",lwd=1.3)


fixed_gaps[, "error"] <- abs(fixed_gaps$Tmin - fixed_gaps$Tmin_observed)

# größe der Fehler durch Interpolation
ggplot(data=fixed_gaps,aes(DATE,error))+ 
  geom_line(lwd=1.3) + xlab("Date") + 
  ylab("Error introduced by interpolation (°C)") +
  geom_point(data=fixed_gaps[which(!fixed_gaps$no_Tmin),],aes(DATE,error),col="red",cex=3)

# Get Data again

# station_list <- handle_gsod(
#   action = "list_stations",
#   location = c(7.10, 50.73),
#   time_interval = c(1990, 2020)
# )

# weather <- handle_gsod(
#   action = "download_weather",
#   location = station_list$chillR_code[4],
#   time_interval = c(1990, 2020)
# )

# weather[[1]]$weather[1:20, ]
 
# cleaned_weather <- handle_gsod(weather)

# cleaned_weather[[1]][[2]]
# cleaned_weather[[1]]$weather[1:20, ]
# cleaned_weather[[1]][2]
# write.csv(cleaned_weather, "weather_data/station_list.csv", row.names = FALSE)
# write.csv(weather,"weather_data/Bonn_weather.csv",row.names=FALSE)
# write.csv(cleaned_weather[[1]][[2]],"weather_data/Bonn_chillR_weather.csv",row.names=FALSE)

          
Bonn<-read.csv("weather_data/Bonn_chillR_weather.csv")
Bonn_QC<-fix_weather(Bonn)$QC

kable(Bonn_QC, caption="Quality control summary produced by *fix_weather()*") %>%
  kable_styling("striped", position = "left", font_size = 10)


str(Bonn)












