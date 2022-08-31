#---------------------------------------
# Description
#---------------------------------------

# first look at the MRT data
# basic time series plot

#---------------------------------------
# 0. packages
#---------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)

#---------------------------------------
# 1. the data
#---------------------------------------

July_22 <- read_csv("D:/A. lhguan/MRT data/201701.csv")

#---------------------------------------
# 2. EDA
#---------------------------------------

names(July_22) <- c("Date", "Hour", "In", "Out", "Value")

unique(July_22$Date)
unique(July_22$Hour)
unique(July_22$In)
unique(July_22$Out)

holiday <- c("2022-07-02", "2022-07-03",
             "2022-07-09", "2022-07-10",
             "2022-07-16", "2022-07-17",
             "2022-07-23", "2022-07-24",
             "2022-07-30", "2022-07-31")
holiday <- as.Date(holiday)

Workday <- July_22 %>%
    filter(!Date %in% holiday)

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
stations <- unique(July_22$In)

for(i in 1:length(stations)){
    g <- timePlot(stations[i])
    ggsave(filename = paste(i, ".png"), plot = g)
}

Workday_i[which.max(Workday_i$Value),]
Workday_o[which.max(Workday_o$Value),]




