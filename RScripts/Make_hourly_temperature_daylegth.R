require(chillR)
require(tidyverse)
require(kableExtra)
require(reshape2)
require(ggdark)



Glogau      <- daylength(latitude = 51.40, JDay = 1:365)
Teneriffa   <- daylength(latitude = 28.19, JDay = 1:365)
Zuelpich    <- daylength(latitude = 50.42, JDay = 1:365)
Moskau      <- daylength(latitude = 55.45, JDay = 1:365)
Karkaralinsk<- daylength(latitude = 49.24, JDay = 1:365)


df <- data.frame(base         = seq(length(Glogau$Sunrise)),
                 Glogau       = Glogau[[3]],
                 Teneriffa    = Teneriffa[[3]],
                 Zülpich      = Zuelpich[[3]],
                 Moskau       = Moskau[[3]],
                 Karkaralinsk = Karkaralinsk[[3]])
df
df <- pivot_longer(df, -"base", names_to = "Location", values_to = "daylength")

ggplot(df, aes(x=base, y=daylength, groupe =Location ) )+
    geom_line(aes(color = Location), lwd =1.0)+
    labs(x= "Days", y= "Daylength [h]")+ theme_grey(base_size = 20)
kable_styling
