library(tidyverse)
library(chillR)
library(kableExtra)
library(lubridate)
library(reshape2)

zuelpich_ex = read.table("weather_data/Weather_Zuelpich_2019_hourly.csv", header = TRUE, sep = ",")


zuel = zuelpich_ex %>% filter("2019-12-01 00:00:00"<date)
zuel$day = as.numeric(substr(zuel[, 3], 9, 10))

#zuel = zuel[,-c(4)]
nrow(zuel)


zuel[,2] = round(zuel[,2], digits = 2)


sum_ = 0
for (i in seq(1, nrow(zuel))) {
  if (zuel[, 4][i] == zuel[, 4][i + 1])
  {
    current = zuel[, 2][i]
    
    sum_ = sum_ + current
    
    zuel$cumsum[i] = sum_
  }
  
  else{
    
    break
    
    #zuel$cumsum[i] = zuel[, 2][i+1]
  }
  
  }

zuel
which()


test_data = data.frame(x= c(1,2,3,4,5),
                       y= c(9,8,7,6,7))


for (i in seq(1, nrow(zuelpich_ex))){
  
  print(zuelpich_ex[,2][i])
}




for(i in test_data) {
  print(test_data$x)
}

for (i in seq(1, nrow(test_data))) {
  print(test_data[, 2][i])
}

for (i in test_data$x)
{
  print(i)
}

for (i in test_data$y)
{
  print(i)
}


for (i in  test_data[, 2]) {
  print(i)
}


for (i in seq(1, length(test_data[, 1]))) {
  test_data$sum[i] = test_data[, 1][i] + test_data[, 2][i]
  print(test_data[, 3][i])
  
}









