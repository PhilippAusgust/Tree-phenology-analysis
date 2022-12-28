library(chillR)
library(dplyr)
library(lubridate)


mon<-1 # Month
ndays<-31 # Number of days per month
tmin<-1
tmax<-8
latitude<-50


weather<-make_all_day_table(data.frame(Year=c(2001,2001),
                                      Month=c(mon,mon),
                                      Day=c(1,ndays),Tmin=c(0,0),Tmax=c(0,0)))

weather$Tmin<-tmin
weather$Tmax<-tmax

hourly_temps<-stack_hourly_temps(weather,latitude=latitude)

CPs<-Dynamic_Model(hourly_temps$hourtemps$Temp)
daily_CPs<-CPs[length(CPs)]/nrow(weather)

daily_CPs



latitude<-50.6

month_range<-c(10,11,12,1,2,3)

Tmins=c(-20:20)
Tmaxs=c(-15:30)

mins<-NA
maxs<-NA
CP<-NA
month<-NA
temp_model<-Dynamic_Model

for(mon in month_range)
{days_month<-as.numeric(difftime( ISOdate(2002,mon+1,1),
                                  ISOdate(2002,mon,1) ))
if(mon==12) days_month<-31
weather<-make_all_day_table(data.frame(Year=c(2001,2002),
                                       Month=c(mon,mon),
                                       Day=c(1,days_month),Tmin=c(0,0),Tmax=c(0,0)))
for(tmin in Tmins)
  for(tmax in Tmaxs)
    if(tmax>=tmin)
    {
      weather$Tmin<-tmin
      weather$Tmax<-tmax
      hourtemps<-stack_hourly_temps(weather,
                                    latitude=latitude)$hourtemps$Temp
      CP<-c(CP,do.call(Dynamic_Model,
                       list(hourtemps))[length(hourtemps)]/(length(hourtemps)/24))
      mins<-c(mins,tmin)
      maxs<-c(maxs,tmax)
      month<-c(month,mon)
    }
}
results<-data.frame(Month=month,Tmin=mins,Tmax=maxs,CP)
results<-results[!is.na(results$Month),]


write.csv(results,"weather_data/model_sensitivity_development.csv",row.names = FALSE)
