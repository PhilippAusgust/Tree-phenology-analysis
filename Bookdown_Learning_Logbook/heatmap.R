library(tidyverse)
library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)
library(colorRamps)
library(tibble)

# load data
weather_fue <- read_delim("temp_hum_pres08.08.22.csv")

# data preparation 

# sub_weather = weather_fue <-
#   weather_fue %>% filter("2021-01-01 00:00:00" < date) %>%
#   filter("2022-01-01 00:00:00" > date)%>%
#   select(temperature, period, type, date, sensor) %>%
#   filter(period == 1, type == 0, sensor == 0)
# 
# write.csv(sub_weather, "/Users/Phil/Documents/TFA-evaluation/Data/Data_Weatherstation/weather_2021_fuessenich_hourly.csv")


mod_weather_fue <-
  weather_fue %>% filter("2022-07-01 00:00:00" < date) %>%
  filter("2022-08-01 00:00:00" > date) %>%
  select(temperature, period, type, date, sensor) %>%
  filter(period == 1, type == 0, sensor == 0) #%>%
# group_by(Tag =day(date))%>%
# summarise(Mittel =  round(mean(temperature, na.rm = TRUE), digits = 1),
#           Tmax = max(temperature), Tmin = min(temperature))%>%
# pivot_longer(-"Tag",
#              names_to = "Treatment",
#              values_to = "Temperatur")
#write.csv(mod_weather_fue, "/Users/Phil/Documents/TFA-evaluation/example_weather_data.csv")

# classical line plot with groups Tmin, Tmax, Tmean

ggplt_f<-
  ggplot(data = mod_weather_fue, aes(x = Tag, y = Temperatur, groupe = Treatment)) +
  labs(x = "Tag", y = "Temperatur") +
  geom_tile(mapping = mod_weather_fue)
# geom_point(aes(colour=Treatment))+
# geom_line(aes(colour=Treatment))+
  geom_text(aes(label = Temperatur), vjust = -0.3, size = 2.0) +
  scale_x_continuous(breaks = seq(1, 31, 1)) +
  ggtitle("Füssenich Juli 2022") +
  theme_minimal()
ggplt_f




# preprocessing 

# create column with date
mod_weather_fue[,"date_new"] = 
  as.POSIXct(mod_weather_fue$date, format ="%m/%d/%Y %H:%M:%S" )

# separate hours (time)
mod_weather_fue[, "hours"] =
  format(mod_weather_fue$date_new, format = "%H:%M:%S")

# pick only hours
mod_weather_fue[, "hours_only"] = 
  as.numeric(substr(mod_weather_fue$hours, 1, 2))

# delete first row (individual fix)
mod_weather_fue = mod_weather_fue[-c(1), ]


# pick day 
mod_weather_fue[, "Tag"] =
  as.numeric(substr(mod_weather_fue$date, 9, 10))


# calculate range 
rng = c("Tmin = ", min(mod_weather_fue$temperature), "Tmax = ", max(mod_weather_fue$temperature),
      c("Diff Tmax - Tmin = ",max(mod_weather_fue$temperature) - min(mod_weather_fue$temperature)))


# set levels 
"8-10 = 1, 10-12 = 2, 12-14 = 3, 14-16 = 4, 16-18 = 5, 18-20 = 6, 20-22 = 7, 
22-24 = 10, 24-26 = 11, 26 -28 = 12, 28-30 =13, 30-32 =14, 32-34 = 15, 34-36 = 16, 36-38 =17"


# set new col Index with NA 

mod_weather_fue[,"Index"] <- NA


# 

min = min(mod_weather_fue[,1])
max = max(mod_weather_fue[,1])

sacle = 2 

end = round(((max-min)/sacle)) 


#end = 14

for( i in seq(1, nrow(mod_weather_fue[,1]))) {
  
  
  for (j in seq(0, (end-1))) {

    if(mod_weather_fue[,1][i,] > (min + end * sacle)) {
      mod_weather_fue[,"Index"][i,] =  paste((min + end * sacle), " >")
      break
    }
    
    if (mod_weather_fue[,1][i,] > (min + (j*sacle))  && mod_weather_fue[,1][i,] < ((min+sacle) + (j*sacle))) { 
      mod_weather_fue[,"Index"][i,] = paste((min + (j*sacle)), "-",  ((min+sacle) + (j*sacle)))
      break
    }
  }
}

#write.csv(mod_weather_fue, "test_loop.csv")

# write function 


dummy_data <- data.frame(
  x= c(-1,3,4,2,5,3,4,4,3,5,6,7,4,3,2,4,5,6,7,8,3),
  y = as.double(seq(0,20)))


#dummy_data<-as_tibble(dummy_data)



make_classes <- function(data, scale_) {
  data <- as_tibble(data)
  min = min(data) - 1
  max = max(data)
  sacle = scale_
  end = round(((max - min) / sacle))
  
  data[, "class"] <- NA
  
  for (i in seq(1, nrow(data[, 1]))) {
    for (j in seq(0, (end - 1))) {
      if (data[, 1][i,] > (min + end * sacle)) {
        data[, "class"][i,] =  paste((min + end * sacle), " >")
        break
      }
      
      if (data[, 1][i,] >= (min + (j * sacle))  &&
          data[, 1][i,] <= ((min + sacle) + (j * sacle))) {
        data[, "class"][i,] = paste((min + (j * sacle)), "-",
                                    ((min + sacle) + (j * sacle)))
        break
      }
    }
  }
  return(list(data, 
              unique(data[,"class"])))
}

make_classes(dummy_data$x, scale_ = 2)

make_classes(mod_weather_fue$temperature, scale_ = 4)[[1]][,"class"]




min = min(dummy_data[,1])-1
max = max(dummy_data[,1])
sacle = 3
end = round(((max-min)/sacle)) 
dummy_data[,"Index"] <- NA

for( i in seq(1, nrow(dummy_data[,1]))) {
  
  
  for (j in seq(0, (end-1))) {
    
    if(dummy_data[,1][i,] > (min + end * sacle)) {
      dummy_data[,"Index"][i,] =  paste((min + end * sacle), " >")
      break
    }
    
    if (dummy_data[,1][i,] >= (min + (j*sacle))  && dummy_data[,1][i,] <= ((min+sacle) + (j*sacle))) { 
      dummy_data[,"Index"][i,] = paste((min + (j*sacle)), "-",  ((min+sacle) + (j*sacle)))
      break
    }
  }
}

write.csv(make_classes(mod_weather_fue$temperature, scale_ = 4)[[1]], "dummytemps.csv")









#end = 14



for( i in seq(1, nrow(mod_weather_fue[,1]))) {
  
  
  for (j in seq(0, (end-1))) {
    
    if(mod_weather_fue[,1][i,] > (8 + end * 2)) {
      mod_weather_fue[,"Index"][i,] =  paste((8 + end * 2), " >")
      break
    }
    
    if (mod_weather_fue[,1][i,] > (8 + (j*2))  && mod_weather_fue[,1][i,] < (10 + (j*2))) { 
      mod_weather_fue[,"Index"][i,] = paste((8 + (j*2)), "-",  (10 + (j*2)))
      break
    }
  }
}




#möglichkeit 2

for( i in seq(1, nrow(mod_weather_fue[,1]))) {


if (mod_weather_fue[,1][i,] > 8 && mod_weather_fue[,1][i,] < 10){
  mod_weather_fue[,"Index"][i,] = paste0("8-10")
} else if (mod_weather_fue[,1][i,]>10 && mod_weather_fue[,1][i,] < 12) {
  mod_weather_fue[,"Index"][i,] = paste0("10-12")
} else if  (mod_weather_fue[,1][i,]> 12 && mod_weather_fue[,1][i,] < 14) {
  mod_weather_fue[,"Index"][i,] = paste0("12-14")
} else if  (mod_weather_fue[,1][i,]> 14 && mod_weather_fue[,1][i,] < 16) {
  mod_weather_fue[,"Index"][i,] = paste0("14-16")
} else if  (mod_weather_fue[,1][i,]> 16 && mod_weather_fue[,1][i,] < 18) {
  mod_weather_fue[,"Index"][i,] = paste0("16-18")
} else if  (mod_weather_fue[,1][i,]> 18 && mod_weather_fue[,1][i,] < 20) {
  mod_weather_fue[,"Index"][i,] = paste0("18-20")
} else if  (mod_weather_fue[,1][i,]> 20 && mod_weather_fue[,1][i,] < 22) {
  mod_weather_fue[,"Index"][i,] = paste0("20-22")
} else if  (mod_weather_fue[,1][i,]> 22 && mod_weather_fue[,1][i,] < 24) {
  mod_weather_fue[,"Index"][i,] = paste0("22-24")
} else if  (mod_weather_fue[,1][i,]> 24 && mod_weather_fue[,1][i,] < 26) {
  mod_weather_fue[,"Index"][i,] = paste0("24-26")
} else if  (mod_weather_fue[,1][i,]> 26 && mod_weather_fue[,1][i,] < 28) {
  mod_weather_fue[,"Index"][i,] = paste0("26-28")
} else if  (mod_weather_fue[,1][i,]> 28 && mod_weather_fue[,1][i,] < 30) {
  mod_weather_fue[,"Index"][i,] = paste0("28-30")
} else if  (mod_weather_fue[,1][i,]> 30 && mod_weather_fue[,1][i,] < 32) {
  mod_weather_fue[,"Index"][i,] = paste0("30-32")
} else if  (mod_weather_fue[,1][i,]> 32 && mod_weather_fue[,1][i,] < 34) {
  mod_weather_fue[,"Index"][i,] = paste0("32-34")
} else if  (mod_weather_fue[,1][i,]> 34 && mod_weather_fue[,1][i,] < 36) {
  mod_weather_fue[,"Index"][i,] = paste0("34-36")
}
  else {
  mod_weather_fue[,"Index"][i,] = paste0("36 >")
}}


#write.csv(mod_weather_fue, "Juli_2022_Wetterstation_Fuessenich_test.csv")

# break
#mod_weather_fue[,"indes_as_factor"] <- factor(mod_weather_fue$Index)






m3 <- mod_weather_fue

m3[,"countfactor"] <- NA


m1 <-m3 %>%
  # convert state to factor and reverse order of levels
  mutate(Index=factor(Index, levels=rev(sort(unique(Index)))))%>%
  mutate(countfactor=cut(temperature, breaks=c(8, 10, 12, 14, 16, 18, 20, 22,
                                         24,26,28,30,32,34,36, max(temperature, na.rm=TRUE)),
                         labels=c("8-10", "10-12", "12-14", "14-16", "16-18", "18-20",
                                  "20-22","22-24","24-26","26-28","28-30",
                                  "30-32","32-34","34-36","36 >")))%>%
mutate(countfactor=factor(as.character(countfactor), levels=rev(levels(countfactor))))

str(m1)

write.csv(m1, "Juli_2022_Wetterstation_Fuessenich_tt.csv")


colors <- c("#002565", "#00556E", "#007763",
            "#007F3A", "#00860D","#198C00","#519300","#8B9803","#9D7608","#9A520B","#98300D","#951010","#931231","#901551","#8B1A89")
textcol <- "grey40"

ggplot(m1, aes(x=hours_only , y=Tag, fill=countfactor ))+
  geom_tile(colour="white", size=0.3)+
  geom_text(aes(label=round(temperature, digits = 1)), size=2.3)+
  scale_fill_manual(values=rev(colors), na.value = "grey90")+
  guides(fill=guide_legend(title="Temperatur in [°C]"))+
  theme_grey(base_size=10)+
  labs(x = "Hour", y = "Day")+
  ggtitle("Hourly mean Temperatures in Juli 2022")+
  theme(legend.position="right", legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0, "cm")),
        legend.text=element_text(colour=textcol, size=7, face="bold"),
        legend.key.height=grid::unit(0.8, "cm"),
        legend.key.width=grid::unit(0.2, "cm"),
        axis.text.x=element_text(size=10, colour=textcol),
        axis.text.y=element_text(vjust=0.2, colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title=element_text(colour=textcol, hjust=0, size=14, face="bold")
  )+
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 6))+
  scale_y_continuous(breaks = seq(0,31,1))
  
  ggsave("July_2022.pdf", width = 30,  height = 20, units = "cm", dpi = 2000)
  
  




  
  
  
  





ggplot(mod_weather_fue, aes(x = hours_only, 
                            y = Tag, 
                            fill =temperature)) +
                            geom_tile(colour="white", size=0.25)+
  scale_fill_gradientn(colours=matlab.like(15))+
  labs(x = "Hour", y = "Day")+
  ggtitle("Hourly mean Temperatures in Juli 2022")+
  
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 6))+
  theme_grey(base_size=8)+
  
  theme(legend.text=element_text(face="bold"),
        axis.ticks=element_line(size=0.4),
        panel.background = element_blank(),
        plot.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()
  )



library(colorRamp)
install.packages("colorRamp")

# make great result 

# test 

v = c("zehn", "zwanzig", "dreißig", "vierzig", "fünfzig")

dt = data.frame( x = c(rep(v[1], 10), rep(v[2], 10),
                      rep(v[3], 10), rep(v[4], 10), rep(v[5], 10)),
                 y = seq(1,50) )

plt = ggplot(dt, aes(x= y, y=x))+
        geom_point()


ramp <- colour_ramp(c("red", "green", "blue"))
show_col(ramp(seq(0, 1, length = 12)))
