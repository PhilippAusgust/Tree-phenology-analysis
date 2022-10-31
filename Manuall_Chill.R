#1. Write a basic function that calculates warm hours (>25°C)   

WH <- function(Degree_Hours)  
{Degree_Hours[,"warm hours"] <- Degree_Hours$Temp >25  
return(Degree_Hours)  
}   

data_subset = Winters_hours_gaps[,c(1,2,3,4,6)]
WH(Degree_Hours = data_subset)



WH(hourtemps)  

WH_sum <- function(Degree_Hours)  
{Degree_Hours[,"warm hours"] <- Degree_Hours$Temp >25  
WH<-sum(Degree_Hours[,"warm hours"])  
return(WH)  
}  


WH_sum(Degree_Hours =data_subset )

#2. Apply this function to the Winters_hours_gaps dataset  

Degree_Hours <- Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]

WH <- function(Degree_Hours)  
{Degree_Hours[,"warm hours"] <- Degree_Hours$Temp >25  
return(Degree_Hours)  
}   

WH(hourtemps)  

WH_sum <- function(Degree_Hours)  
{Degree_Hours[,"warm hours"] <- Degree_Hours$Temp >25  
WH<-sum(Degree_Hours[,"warm hours"])  
return(WH)  
}  

WH_sum(hourtemps)    
>1275
#3. Extend this function, so that it can take start and end dates as inputs and sums up warm hours between these dates  

WH_sum_dates <- function(Degree_Hours, Start_year, Start_month, Start_day, Start_hour,  
                         End_year, End_month, End_day, End_hour)  
{
  Start_Date <- which(hourtemps$Year==Start_year &    
                        hourtemps$Month==Start_month &    
                        hourtemps$Day==Start_day &    
                        hourtemps$Hour==Start_hour)    
  End_Date <- which(hourtemps$Year==End_year &  
                      hourtemps$Month==End_month &  
                      hourtemps$Day==End_day &  
                      hourtemps$Hour==End_hour)  
  Degree_Hours[,"Cwarm hours"] <- Degree_Hours$Temp > 25  
  WHs<- sum(Degree_Hours[Start_Date:End_Date,"Cwarm hours"])  
  return(WHs)  
}  
WH_sum_dates(data_subset, 2008, 4, 4, 12, 2008, 10, 10, 12)
