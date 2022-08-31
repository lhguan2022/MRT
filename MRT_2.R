#---------------------------------------
# Description
#---------------------------------------

# first look at the MRT data
# basic time series plot
# use parameters to load the data

#---------------------------------------
# 0. packages
#---------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(lubridate)

#---------------------------------------
# 1. the csv's
#---------------------------------------

Websites <- read_csv("D:/A. lhguan/MRT data/websites.csv")
names(Websites) <- c("YM", "url")
ym = "202201"
theURL = Websites$url[Websites$YM == ym]
df <- read_csv(theURL)

#---------------------------------------
# 2. EDA
#---------------------------------------

names(df) <- c("Date", "Hour", "In", "Out", "Value")

unique(df$Date)
unique(df$Hour)
unique(df$In)
unique(df$Out)

# holiday <- c("2021-05-01", "2021-05-02",
#              "2021-05-08", "2021-05-09",
#              "2021-05-15", "2021-05-16",
#              "2021-05-22", "2021-05-23",
#              "2021-05-29", "2021-05-30")
# holiday <- as.Date(holiday)
# Workday <- df %>% filter(!Date %in% holiday)

df$Weekday <- wday(df$Date, label = T)
Workday <- df %>% filter(!Weekday %in% c("週六", "週日"))

Workday_i <- aggregate(Value ~ Date + Hour + In,
                       FUN = sum, data = Workday)
Workday_o <- aggregate(Value ~ Date + Hour + Out,
                       FUN = sum, data = Workday)

timePlot <- function(loc){
    temp_i <- Workday_i %>% filter(In == loc)
    temp_o <- Workday_o %>% filter(Out == loc)
    ggplot() +
        # in
        geom_point(data = temp_i, aes(x = Hour, y = Value, group = Date), color = "blue") +
        geom_line(data = temp_i, aes(x = Hour, y = Value, group = Date), color = "blue") +
        # out
        geom_point(data = temp_o, aes(x = Hour, y = Value, group = Date), color = "red") +
        geom_line(data = temp_o, aes(x = Hour, y = Value, group = Date), color = "red") +
        # title
        ggtitle(loc)
}
stations <- unique(df$In)

for(i in 1:length(stations)){
    g <- timePlot(stations[i])
    ggsave(filename = paste(i, ".png"), plot = g, path = "2021_05/")
}
