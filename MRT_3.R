#---------------------------------------
# Description
#---------------------------------------

# first look at the MRT data
# basic time series plot
# use parameters to load the data
# compute the three indexes

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

# 1.1 load the data from the given url

Websites <- read_csv("D:/A. lhguan/MRT data/websites.csv")
names(Websites) <- c("YM", "url")
ym = "201701"
theURL = Websites$url[Websites$YM == ym]
df <- read_csv(theURL)

#---------------------------------------
# 2. preprocess
#---------------------------------------

# 2.1 rename the columns

names(df) <- c("Date", "Hour", "In", "Out", "Value")

# 2.2 remove the holidays

df$Weekday <- wday(df$Date, label = T)
Workday <- df %>% filter(!Weekday %in% c("週六", "週日"))

# 2.3 aggregate the data by in-stations and out-stations

Workday_i <- aggregate(Value ~ Date + Hour + In,
                       FUN = sum, data = Workday)
Workday_o <- aggregate(Value ~ Date + Hour + Out,
                       FUN = sum, data = Workday)
# 2.4 aggregate again

Workday_i_m <- aggregate(Value ~ Hour + In,
                         FUN = mean, data = Workday_i)
Workday_o_m <- aggregate(Value ~ Hour + Out,
                       FUN = mean, data = Workday_o)

# 2.5 plot the result

timePlot <- function(loc){
    temp_i <- Workday_i_m %>% filter(In == loc)
    temp_o <- Workday_o_m %>% filter(Out == loc)
    ggplot() +
        # in
        geom_point(data = temp_i, aes(x = Hour, y = Value), color = "blue") +
        geom_line(data = temp_i, aes(x = Hour, y = Value, group = In), color = "blue") +
        # out
        geom_point(data = temp_o, aes(x = Hour, y = Value), color = "red") +
        geom_line(data = temp_o, aes(x = Hour, y = Value, group = Out), color = "red") +
        # title
        ggtitle(loc)
}

stations <- unique(df$In)
stickers <- stations
stickers[stations == "台北101/世貿"] <- "台北101"

for(i in 1:length(stations)){
    g <- timePlot(stations[i])
    if(!file.exists(ym)) dir.create(ym)
    ggsave(filename = paste(stickers[i], "_", ym, ".png", sep = ""), 
           plot = g, 
           path = paste(ym, "/", sep = ""))
}


#---------------------------------------
# 3. data transformation
#---------------------------------------

# 3.0 data frame for the result

result <- data.frame(station = stations)

# 3.1 sum of "In" between 5:00 ~ 12:00

f1 <- function(loc){
    temp_i <- Workday_i_m %>% filter(In == loc)
    sum_i <- sum(temp_i$Value[3:10])
    return(sum_i)
}

result$index1 <- rep(0, nrow(result))

for(i in 1:nrow(result)){
    result$index1[i] <- f1(result$station[i])
}

# 3.2 ratio of max of "In" and "Out" between 5:00 ~ 12:00

f2 <- function(loc){
    temp_i <- Workday_i_m %>% filter(In == loc)
    temp_o <- Workday_o_m %>% filter(Out == loc)
    max_i <- max(temp_i$Value[3:10])
    max_o <- max(temp_o$Value[3:10])
    return(max_i/max_o)
}

result$index2 <- rep(0, nrow(result))

for(i in 1:nrow(result)){
    result$index2[i] <- f2(result$station[i])
}

# 3.3 ratio of sum of "In" and "Out" between 5:00 ~ 12:00

f3 <- function(loc){
    temp_i <- Workday_i_m %>% filter(In == loc)
    temp_o <- Workday_o_m %>% filter(Out == loc)
    sum_i <- sum(temp_i$Value[3:10])
    sum_o <- sum(temp_o$Value[3:10])
    return(sum_i/sum_o)
}

result$index3 <- rep(0, nrow(result))

for(i in 1:nrow(result)){
    result$index3[i] <- f3(result$station[i])
}

ggplot(mapping = aes(x = index2, y = index3), data = result) +
    geom_point()



