#install.packages("chillR")

library(chillR)
library(knitr)
#install.packages("pander")
library(pander)
#install.packages("kableExtra")
library(kableExtra)



kable(Winters_hours_gaps[1:20,])  %>%
  kable_styling("striped", position = "left",font_size = 10)
                


nrow(Winters_hours_gaps)
hourtemps<-Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]
hourtemps


library(lubridate)
library(tidyverse)
# hourtemps_sum = hourtemps %>% group_by(Monat=month(Month)) %>% summarise(Temp = mean(Temp))

hourtemps[3,]
hourtemps[c(1,2,4),]
hourtemps[1:5,]
hourtemps[1:5,"Temp"]



hourtemps[,"Chilling_Hour"]<-hourtemps$Temp>=0&hourtemps$Temp<=7.2
hourtemps
hourtemps[,6]
hourtemps[13:20,]

sum(hourtemps$Chilling_Hour[13:20])
sum(hourtemps$Chilling_Hour)

Start_Date<-which(hourtemps$Year==2008 & hourtemps$Month==10 &
                    hourtemps$Day==1 & hourtemps$Hour==12)
End_Date<-which(hourtemps$Year==2008 & hourtemps$Month==10 &
                  hourtemps$Day==31 & hourtemps$Hour==12)

hourtemps[Start_Date,]
hourtemps[End_Date,]

#28 Chill Hours in October 2008
sum(hourtemps$Chilling_Hour[Start_Date:End_Date])


CH<-function(hourtemps)
{
  hourtemps[,"Chilling_Hour"]<-hourtemps$Temp>=0&hourtemps$Temp<=7.2
  return(hourtemps)
}

CH(hourtemps = hourtemps)[13:20,]


help("trunc")



sum_CH<-function(hourtemps, Start_YEARMODA, End_YEARMODA)
{
  Start_Year<-trunc(Start_YEARMODA/10000) # 2008/10000 = 0.2008
  Start_Month<-trunc((Start_YEARMODA-Start_Year*10000)/100)
  Start_Day<-Start_YEARMODA-Start_Year*10000-Start_Month*100
  Start_Hour<-12 # This could also be flexible, but let's skip this for now
  End_Year<-trunc(End_YEARMODA/10000)
  End_Month<-trunc((End_YEARMODA-End_Year*10000)/100)
  End_Day<-End_YEARMODA-End_Year*10000-End_Month*100
  End_Hour<-12 # This could also be flexible, but let's skip this for now
  
  Start_Date<-which(hourtemps$Year==Start_Year & hourtemps$Month==Start_Month &
                      hourtemps$Day==Start_Day & hourtemps$Hour==Start_Hour)
  End_Date<-which(hourtemps$Year==End_Year & hourtemps$Month==End_Month &
                    hourtemps$Day==End_Day & hourtemps$Hour==End_Hour)
  
  Chill_hours<-CH(hourtemps)
  
  return(sum(Chill_hours$Chilling_Hour[Start_Date:End_Date]))
  
}


# trunc(20080401/10000)
# trunc((20080401-2008*10000)/100)
# Start_Day=20080401-2008*10000-4*100



m = Dates_[[2]]








