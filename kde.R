df <- read.csv("C:/Users/Yun/Documents/github/STAL-distribution/ebird_albatross.csv")
ST <- df[df$common_name == "Short-tailed Albatross",]
str(ST)

library(ggplot2)
library(ggpattern)
library(lattice)
library(sf)
library(dplyr)

# Date
ST$Date_time <- as.POSIXct(ST$observation_date, format="%Y/%m/%d %H:%M") #don't have to set tz
range(ST$Date_time)
ST2024 <- ST[ST$Date_time >= "2020-01-01",] #2020-2024

# 2023-03-11 chaos Hanpo
# 2022-03-25 chaos Powei
ST2024 <- ST2024 %>% 
  filter(!(Date_time == "2023-03-11" & !grepl("obsr557276", observer_id))) %>%
  filter(!(Date_time == "2022-03-25" & !grepl("obsr659889", observer_id)))

# month
ST2024$month <- as.numeric(strftime(as.POSIXlt(ST2024$Date_time),format="%m"))
ST2024$month <- factor(ST2024$month, levels=1:12, labels=month.abb)
month_count <- aggregate(observation_count ~ month, data = ST2024, FUN = sum, drop = F)
month_count$observation_count <- replace(month_count$observation_count, is.na(month_count$observation_count), 0)
ggplot(month_count, aes(x=month, y=observation_count)) +
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Observation Count") +
  ggtitle("2020 - 2024 STAL observations on eBird in Taiwan") +
  theme_classic()

# plumage
month_count$adult <- c(0, 10, 29, 7, 0, 0, 0, 0, 0, 0, 0, 0)
month_count$subadult <- c(0, 2, 3, 2, 2, 0, 0, 0, 0, 0, 0, 0)
month_count$immature <- c(0, 4, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0)
month_count <- month_count %>%
  mutate(
    percentage_adult = (adult / (adult+subadult+immature)) * 100,
    percentage_subadult = (subadult / (adult+subadult+immature)) * 100,
    percentage_immature = (immature / (adult+subadult+immature)) * 100
  ) %>%
  replace(is.na(.), 0)
plumage <- data.frame(month = rep(month_count[c(2:5),1], 3), 
                      perc = c(unlist(month_count[c(2:5),c(6:8)])), 
                      class = rep(c("adult", "subadult", "immature"), each = 4))
plumage$class <- factor(plumage$class, levels = c("adult", "subadult", "immature"))
ggplot(plumage, aes(month, perc, fill = class)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("adult" = "#EED787", "subadult" = "#F89B9B", "immature" = "#443C5D"), name = "Class") + 
  theme_minimal() +
  ylab("percentage") + 
  ggtitle("Feb-May plumage")

# distribution
xyplot(latitude ~ longitude, data = ST2024)
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
  geom_point(data=eb_ST, 
             aes(x=wrap360(longitude),y=latitude), color="red",size=0.7)+
  geom_point(data=ob_ST, 
             aes(x=wrap360(LONGITUDE),y=LATITUDE), color="blue",size=0.7)+
  coord_fixed(ratio=1,xlim = c(121,124),ylim=c(24,27))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

# doughnut plot
# Oct-Dec, Jan, Feb-May

# kde

# occupancy
# select good quality lists
eb_ST <- ST2024[ST2024$protocol_type == c("Incidental", "eBird Pelagic Protocol", "Stationary"),]
eb_ST1 <- ST2024 %>% filter(protocol_type == "Traveling", duration_minutes <= 30)
eb_ST <- rbind(eb_ST, eb_ST1)
rm(eb_ST1)

ob_ST <- readxl::read_xlsx("C:/Users/Yun/Downloads/新增資料夾/at sea.xlsx")
ob_ST$WHEN <- as.POSIXct(ob$WHEN, format="%Y-%m-%d %H:%M:%S")
ob_ST$DATE <- strftime(as.POSIXct(ob_ST$WHEN), format="%Y-%m-%d")
eb_ST <- eb_ST %>% filter(!Date_time %in% unique(ob_ST$DATE)) %>% filter(locality_id != "L3290712") #port
