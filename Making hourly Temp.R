# Making hourly temperatures


require(chillR)
require(ggplot2)
require(reshape2)
require(kableExtra)
require(tidyverse)

KA_weather

KA_weather[nrow(KA_weather),]

KA_hours<-KA_weather[10:20,]
KA_hours
KA_hours[,"Hour"]<-0
KA_hours
KA_hours$Hour[nrow(KA_hours)]<-23
KA_hours
KA_hours[,"Temp"]<-0
KA_hours<-make_all_day_table(KA_hours,timestep="hour")

# make_all_day_table  Erzeugt aus einem täglichen Datenset Platzhalter für stündliche Werte 





KA_hours

#for(i in 2:nrow(KA_hours)){print(i)}


for(i in 2:nrow(KA_hours))
{if(is.na(KA_hours$Tmin[i])) KA_hours$Tmin[i]<-KA_hours$Tmin[i-1]
if(is.na(KA_hours$Tmax[i])) KA_hours$Tmax[i]<-KA_hours$Tmax[i-1]
}
KA_hours$Temp<-NA

KA_hours$Temp[which(KA_hours$Hour==6)]<-KA_hours$Tmin[which(KA_hours$Hour==6)] 
KA_hours$Temp[which(KA_hours$Hour==18)]<-KA_hours$Tmax[which(KA_hours$Hour==18)] 
KA_hours$Temp<-interpolate_gaps(KA_hours$Temp)$interp

ggplot(KA_hours[20:100,],aes(DATE,Temp))+geom_line(lwd=1.5)+xlab("Date")+ylab("Temperature (°C)") +
  theme_bw(base_size = 20)





require(ggplot2)
require(reshape2) # functiom melt benutzen 
Days<-daylength(latitude=50.4,JDay=1:365)
Days_df<-data.frame(JDay=1:365,Sunrise=Days$Sunrise,Sunset=Days$Sunset,Daylength=Days$Daylength)
Days_df<-melt(Days_df, id=c("JDay")) 



ggplot(Days_df, aes(JDay, value)) + geom_line(lwd=1.5) + facet_grid(cols=vars(variable)) +
  ylab("Time of Day / Daylength (Hours)") + theme_bw(base_size = 20)


KA_weather

KA_hourly<-stack_hourly_temps(KA_weather, latitude=50.4, keep_sunrise_sunset = FALSE)
KA_hourly

kable(KA_hourly$hourtemps[100:120,],row.names = FALSE)  %>%
  kable_styling("striped", position = "left",font_size = 10)




# add a plot of hourly temperatures



# Datum zum ursprünglichen Datafame
KA_hourly$hourtemps[,"DATE"]<-ISOdate(KA_hourly$hourtemps$Year,
                                      KA_hourly$hourtemps$Month, 
                                      KA_hourly$hourtemps$Day, 
                                      KA_hourly$hourtemps$Hour)

#look at KA_hourly
KA_hourly
ggplot(KA_hourly$hourtemps[20:100,],aes(DATE,Temp))+
  geom_line(lwd=1.5)+
  xlab("Date")+
  ylab("Temperature (°C)") +
  theme_bw(base_size = 20)









empi_curve<-Empirical_daily_temperature_curve(Winters_hours_gaps)


kable(empi_curve[1:48,])  %>%
  kable_styling("striped", position = "left",font_size = 10)

ggplot(data=empi_curve[1:96,], aes(Hour,Prediction_coefficient))+
  geom_line(lwd=1.3, col="red")+
  facet_grid(rows=vars(Month))+
  xlab("Hour of the day")+
  ylab("Prediction coefficient") + 
  theme_bw(base_size=20)



coeffs<-Empirical_daily_temperature_curve(Winters_hours_gaps)
Winters_daily<-make_all_day_table(Winters_hours_gaps, input_timestep="hour")
Winters_hours<-Empirical_hourly_temperatures(Winters_daily,coeffs)

require(reshape2)








