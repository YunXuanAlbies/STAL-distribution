df <- read.csv("C:/Users/Yun/Documents/github/STAL-distribution/ebird_albatross.csv")
ST <- df[df$common_name == "Short-tailed Albatross",]
str(ST)

library(ggplot2)
library(lattice)
library(sf)

# Date
ST$Date_time <- as.POSIXct(ST$observation_date, format="%Y/%m/%d %H:%M:%S") #don't have to set tz
range(ST$Date_time)
ST <- ST[ST$Date_time >= "2020-01-01",] #2020-2024

xyplot(latitude ~ longitude, data = ST)
w2hr <- map_data('world')
w2hr_sub <- w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada",
                                  "North Korea","South Korea","Taiwan","Mongolia"),]
wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}
ggplot()+
  geom_polygon(data=w2hr_sub,
               aes(wrap360(long),lat,group=group),fill="gray25",color="grey60",linewidth=0.1)+
  geom_point(data=ST, 
             aes(x=wrap360(longitude),y=latitude), color="red",size=0.7)+
  coord_fixed(ratio=1,xlim = c(121,125),ylim=c(24,28))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

# month
# Oct-Dec, Jan, Feb-May
ST$month <- as.numeric(strftime(as.POSIXlt(ST$Date_time),format="%m"))
ST$month <- factor(ST$month, levels=1:12, labels=month.abb)
month_count <- aggregate(observation_count ~ month, data = ST, FUN = sum, drop = F)
month_count$observation_count <- replace(month_count$observation_count, is.na(month_count$observation_count), 0)
ggplot(month_count, aes(x=month, y=observation_count)) +
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Observation Count") +
  ggtitle("2020 - 2024 STAL observations on eBird in Taiwan") +
  scale_x_discrete(labels=month.abb) +
  theme_classic()

# kde

