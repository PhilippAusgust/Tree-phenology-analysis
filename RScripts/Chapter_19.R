library(chillR)
#library(devtools)
#install_github('https://github.com/EduardoFernandezC/dormancyR')
library(dormancyR)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(patchwork)


hourly_models <- list(Chilling_units = chilling_units,
                      Low_chill = low_chill_model,
                      Modified_Utah = modified_utah_model,
                      North_Carolina = north_carolina_model,
                      Positive_Utah = positive_utah_model,
                      Chilling_Hours = Chilling_Hours,
                      Utah_Chill_Units = Utah_Model,
                      Chill_Portions = Dynamic_Model)
daily_models<-list(Rate_of_Chill = rate_of_chill, 
                   Exponential_Chill = exponential_chill,
                   Triangula_Chill_Haninnen = triangular_chill_1,
                   Triangular_Chill_Legave = triangular_chill_2)

metrics<-c(names(daily_models),names(hourly_models))

model_labels=c("Rate of Chill",
               "Exponential Chill",
               "Triangular Chill (Häninnen)",
               "Triangular Chill (Legave)",
               "Chilling Units",
               "Low-Chill Chill Units",
               "Modified Utah Chill Units",
               "North Carolina Chill Units",
               "Positive Utah Chill Units",
               "Chilling Hours",
               "Utah Chill Units",
               "Chill Portions")


#iterate over constant temperatures
for(T in -20:30) {
  #apply the temperature on the different models
  hourly<-sapply(hourly_models, function(x) x(rep(T,1000)))[1000,]
  #create input for daily models
  temp_frame<-data.frame(Tmin=rep(T,1000),
                         Tmax=rep(T,1000),
                         Tmean=rep(T,1000))
  #run daily models
  daily<-sapply(daily_models, function(x) x(temp_frame))[1000,]
  
  #in the first step of the iteration create the sensitivity vector, in other steps 
  #append row to matrix
  if(T==-20) sensitivity<-c(T=T,daily,hourly) else
    sensitivity<-rbind(sensitivity,c(T=T,daily,hourly))
}


#outputs are in completely different scales
#--> normalize to make comparable
sensitivity_normal<-
  as.data.frame(cbind(sensitivity[,1],
                      sapply(2:ncol(sensitivity),
                             function(x)
                               sensitivity[,x]/max(sensitivity[,x]))))
colnames(sensitivity_normal)<-colnames(sensitivity)
#bring to long format
sensitivity_gg<-melt(sensitivity_normal,id.vars="T")
#set relatively small output equal to missing value
sensitivity_gg$value[which(sensitivity_gg$value<=0.001)]<-NA


chill<-
  ggplot(sensitivity_gg,aes(x=T,y=factor(variable),size=value)) +
  geom_point(col="light blue") +
  scale_y_discrete(labels= model_labels) +
  ylab("Chill model") +
  xlab("Temperature (assumed constant, °C)") +
  xlim(c(-30,40)) +
  theme_bw(base_size=15) +
  labs(size = "Chill \nWeight")

chill
#make a similar plot with the observed temperature in 
# 1) CKA (pear case study)
# 2) Beijing (Chestnut case study)
# 3) Davis (walnut case study)

#load daily temperature
KA_temps_JD<-make_JDay(read_tab("weather_data/Alexander_Lucas/TMaxTMin1958-2019_patched.csv"))
#trasnform daily to hourly temperature
temps<-stack_hourly_temps(
  KA_temps_JD[which(KA_temps_JD$JDay>305|KA_temps_JD$JDay<90),],
  latitude=50.6)
#create histogram of observed temperature for chill season
hh<-hist(temps$hourtemps$Temp,breaks=c(-30:30), plot=FALSE)
#hist(temps$hourtemps$Temp,breaks=c(-30:30))
#normalize the histogram output
hh_df<-data.frame(
  T=hh$mids,
  variable="Klein-Altendorf, Germany",
  value=hh$counts/max(hh$counts))
hh_df$value[which(hh_df$value==0)]<-NA

#same procedure for Beijing
Beijing_temps_JD<-make_JDay(read_tab("weather_data/Chapter_19_Successes and limitations of PLS regression analysis/Beijing_weather.csv"))
temps<-stack_hourly_temps(
  Beijing_temps_JD[which(Beijing_temps_JD$JDay>305|Beijing_temps_JD$JDay<90),]
  ,latitude=39.9)
hh<-hist(temps$hourtemps$Temp,breaks=c(-30:30), plot=FALSE)
hh_df_2<-data.frame(T=hh$mids,
                    variable="Beijing, China",
                    value=hh$counts/max(hh$counts))
hh_df_2$value[which(hh_df_2$value==0)]<-NA

Davis_temps_JD<-make_JDay(read_tab("weather_data/Chapter_19_Successes and limitations of PLS regression analysis/Davis_weather.csv"))
temps<-stack_hourly_temps(
  Davis_temps_JD[which(Davis_temps_JD$JDay>305|Davis_temps_JD$JDay<90),],
  latitude=38.5)
hh<-hist(temps$hourtemps$Temp,breaks=c(-30:40), plot=FALSE)
hh_df_3<-data.frame(T=hh$mids,
                    variable="Davis, California",
                    value=hh$counts/max(hh$counts))
hh_df_3$value[which(hh_df_3$value==0)]<-NA


#combine the dataframes of CKA, Beijing and Davis
hh_df<-rbind(hh_df,hh_df_2,hh_df_3)


locations<-
  ggplot(data=hh_df,aes(x=T,y=variable,size=value)) +
  geom_point(col="coral2") +
  ylab("Location") +
  xlab("Temperature (between November and March, °C)") + 
  xlim(c(-30,40)) +
  theme_bw(base_size=15) +
  labs(size = "Relative \nfrequency")


locations

plot<- (chill +
          locations +
          plot_layout(guides = "collect",
                      heights = c(0.5,1))
) & theme(legend.position = "right",
          legend.text = element_text(size=10),
          legend.title = element_text(size=12))

plot
#--> in CKA most frequent weather is in the optimal temperature range of Dynamic Model
#little variation in chill because mostly optimal chill
#variation in Beijing and Davis was larger













