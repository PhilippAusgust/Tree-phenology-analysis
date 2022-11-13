library(chillR)
library(kableExtra)


Chilling_Hours


HourTemp = c(1.1,18.2,4,7.1,7.7,5.4)

Chilling_Hours <- function (HourTemp, summ= TRUE)
{
    CH_range <- which(HourTemp <= 7.2 & HourTemp >= 0) # Speichert in CH_range an welcher Stelle die Bedingung HourTemp <= 7.2 & HourTemp >= 0 erfüllt ist.
    CH_weights <- rep(0, length(HourTemp))  # Erzeugt einen leeren Vector der Länge con HourTemp mit Nullen gefüllt 
    CH_weights[CH_range] <- 1               # überschreibt die Nullen an der Stelle, an der die obere Bedingung erfüllt ist mit eins 
    if (summ == TRUE)
        return(cumsum(CH_weights))          # Gibt die kummulierte Summe zurück 
    else return(CH_weights)
  
}

# Example from "help"

# help("Chilling_Hours")
# 
# 
# 
# weather<-fix_weather(KA_weather[which(KA_weather$Year>2006),])
# 
# hourtemps<-stack_hourly_temps(weather,latitude=50.4)
# 
# Chilling_Hours(hourtemps$hourtemps$Temp, summ= TRUE)


Chilling_Hours(HourTemp= HourTemp, summ = FALSE)

Chilling_Hours(Winters_hours_gaps$Temp)[1:100] # Insgesamt gab es in diesem Zeitraum 25 Chill Hours
tail(Chilling_Hours(Winters_hours_gaps$Temp))


# Utah Model 

Utah_Model <- function (HourTemp, summ = TRUE) 
  return(step_model(HourTemp, df = data.frame(lower = c(-1000, 
     1.4, 2.4, 9.1, 12.4, 15.9, 18), upper = c(1.4, 2.4, 9.1, 
     12.4, 15.9, 18, 1000), weight = c(0, 0.5, 1, 0.5, 0, -0.5, 
     -1)), summ = summ))

step_model

# Function step model 

nutzlos <- function (HourTemp, df = data.frame(lower = c(-1000, 1.4, 2.4, 9.1, 12.4, 15.9, 18), 
                                    upper = c(1.4,   2.4, 9.1, 12.4, 15.9, 18, 1000), 
                                    weight = c(0,    0.5, 1,   0.5,  0,   -0.5, -1)), summ = TRUE) 
{
  lower <- df$lower
  upper <- df$upper
  weight <- df$weight
  if (summ == TRUE) 
    return(cumsum(sapply(HourTemp, function(x) weight[which(x > 
           lower & x <= upper)])))
  else return(sapply(HourTemp, function(x) weight[which(x > 
             lower & x <= upper)]))
}

help(sapply)

Utah_Model(Winters_hours_gaps$Temp, summ= TRUE)[1:6]

Utah_Model(Winters_hours_gaps$Temp, summ= FALSE)[1:6]


head(Winters_hours_gaps$Temp)




df<-data.frame(
  lower=c(-1000,1,2,3,4,5, 6),
  upper=c(   1, 2,3,4,5,6, 1000),
  weight=c(  0, 1,2,3,2,1, 0))
df


head(Winters_hours_gaps$Temp)

custom<-function(x) step_model(x,df)
custom(Winters_hours_gaps$Temp)[1:100] 


Dynamic_Model


chilling



output<-chilling(make_JDay(Winters_hours_gaps),Start_JDay = 90, End_JDay = 100)
output




# Dynamic Model

Dynamic_Model(Winters_hours_gaps$Temp)[1:100]
output<-chilling(make_JDay(Winters_hours_gaps),Start_JDay = 90, End_JDay = 100)
help(Dynamic_Model)

help("chilling")

help("make_JDay") # erstellt Tage als index (Juian Kaleder) 
winter_JDAy <- make_JDay(Winters_hours_gaps)
winter_JDAy[1:100,]


output<-chilling(make_JDay(Winters_hours_gaps),Start_JDay = 90, End_JDay = 100)
output
kable(output) %>%
  kable_styling("striped", position = "left", font_size = 10)




help("tempResponse")
output<-tempResponse(make_JDay(Winters_hours_gaps),
                     Start_JDay = 90, 
                     End_JDay = 100, 
                     models=list(Chill_Portions=Dynamic_Model, GDH=GDH))
output

kable(output) %>%
  kable_styling("striped", position = "left", font_size = 10)



Utah_Model()

# Create your own temperature-weighting chill model using the step_model() function


Utah_new <- function (HourTemp, summ = TRUE){ 
  return(step_model(HourTemp, df =o_df, summ = summ))}




Utah_new(Winters_hours_gaps$Temp[1:100], summ = TRUE)
Utah_Model(Winters_hours_gaps$Temp, summ = TRUE)[1:100]



function (HourTemp, df = data.frame(lower = c(-1000, 1.4, 2.4, 9.1, 12.4, 15.9, 18), 
                                    upper = c(1.4,   2.4, 9.1, 12.4, 15.9, 18, 1000), 
                                    weight = c(0,    0.5, 1,   0.5,  0,   -0.5, -1)), summ = TRUE) 
{
                                lower <- df$lower
                                upper <- df$upper
                                weight <- df$weight
                                if (summ == TRUE) 
                                  return(cumsum(sapply(HourTemp, function(x) weight[which(x > 
                                                       lower & x <= upper)])))
                                else return(sapply(HourTemp, function(x) weight[which(x > 
                                                       lower & x <= upper)]))
}

#2. create own dataframe with lower upper and weights 
o_df = data.frame (lower  = c(-100,0,  2, 4,  5, 6,   7    ),
                   upper  = c(  0, 2,  4, 5,  6, 7,   100  ),
                   weight = c(  0, 0.5,1, 1.5,1, 0.5, 0    ))

# apply 
custom<-function(x) step_model(x,df)
use_step_model <- function(x){step_model(x,o_df)}

# look at the result
use_step_model(Winters_hours_gaps$Temp)[1:100]



#3. Run this model on the Winters_hours_gaps dataset using the tempResponse() function.

output<-tempResponse(make_JDay(Winters_hours_gaps),Start_JDay = 30, End_JDay = 100, models=list(Chill_Portions=Dynamic_Model, GDH=GDH, Phil = use_step_model))
output
kable(output) %>%
  kable_styling("striped", position = "left", font_size = 10)

help(tempResponse)
# Spielerei 

# Days_Euskirchen<-daylength(latitude=50.4,JDay=1:365)
# Days_Glogow <- daylength(latitude = 51.4, JDay = 1:365)
# 
# df_1 = data.frame(base= seq(length(Days_Euskirchen$Sunrise)),
#                   Euskirchen = Days_Euskirchen$Daylength,
#                   Glogau = Days_Glogow$Daylength)
# library(tidyverse)
# df_1
# 
# df_long=pivot_longer(df_1, -"base", names_to = "Location", values_to = "day_length")
# 
# 
# ggplot(df_long, aes(x=base, y=day_length, groupe =Location ) )+
#   geom_line(aes(color = Location))+
#   labs(x= "Days", y= "Daylength")
#   



