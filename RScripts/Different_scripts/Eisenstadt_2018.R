require(chillR)
require(tidyverse)
require(kableExtra)
require(reshape2)


station_list_burgenland = handle_gsod(action="list_stations",
                                  location=c(17.02,47.77),
                                  time_interval=c(2000,2020))


station_list_burgenland_data <- handle_gsod(
  action = "download_weather",
  location = station_list_burgenland$chillR_code[7],
  time_interval = c(2000, 2020)
)


station_list_burgenland_data[[1]][[2]][1:30,]


clean_Eisenstadt = handle_gsod(station_list_burgenland_data)
year_2000 = clean_Eisenstadt[[1]][[2]][6570:6935,]
year_2018 = clean_Eisenstadt[[1]][[2]][6576:6936,]

tail(year_2000)


month_2018 = year_2018

month_2018$date_new = as.Date(month_2018[,1])

plot(x = year_2018[, 1], y = year_2018[, 7])
plot(x = year_2000[, 1], y = year_2000[, 7])

str(month_2018)
library(lubridate)

final <- month_2018 %>%
  group_by(Tag = month(date_new)) %>%
  summarise(Mittel =  round(mean(Tmean, na.rm = TRUE), digits = 1))
final[,2]

final_max <- month_2018 %>%
  group_by(Tag = month(date_new)) %>%
  summarise(Mittel =  round(mean(Tmax, na.rm = TRUE), digits = 1))


final_min <- month_2018 %>%
  group_by(Tag = month(date_new)) %>%
  summarise(Mittel =  round(mean(Tmin, na.rm = TRUE), digits = 1))


df_Eisenstadt = data.frame(
  Datum = final$Tag,
  Mean = final$Mittel,
  Min = final_min$Mittel,
  Max = final_max$Mittel
)

df_long =
  pivot_longer(df_Eisenstadt,
               -"Datum",
               names_to = "Treatment",
               values_to = "Temperatur")



Eisenstadt = ggplot(df_long, aes(x = Datum, y = Temperatur, groupe = Treatment)) +
  geom_line(aes(colour = Treatment), size = 1.3) +
  geom_point(aes(colour = Treatment)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  geom_text(aes(label = Temperatur, vjust = -0.3), size = 4.5) +
  labs(x = "Monat", x = "Tempratur") +
  ggtitle("Eisenstadt 2018") +
  theme_minimal(base_size = 20)
 


Eisenstadt + 
  ggsave(plot = Eisenstadt, filename = "Eisenstadt.png")
  #theme_gray(base_size = 20)







