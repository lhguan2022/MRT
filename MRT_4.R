#---------------------------------------
# 0. packages
#---------------------------------------

library(readr)
library(reshape2)
library(lubridate)
library(tidyverse)

#---------------------------------------
# 1. the csv's
#---------------------------------------

# 1.1 load the data

yy <- "2022"
# mm <- c("01", "02", "03", "04", "05", "06",
#         "07", "08", "09", "10", "11", "12")
mm <- c("01", "02", "03", "04", "05", "06",
        "07")

df <- data.frame()

for(i in 1:length(mm)){
    ym = paste(yy, mm[i], sep = "")
    dir = paste("D:/A. lhguan/MRT data/", ym, ".csv", sep = "")
    df_temp <- read_csv(dir)
    df <- rbind(df, df_temp)
}
remove(df_temp)

#---------------------------------------
# 2. preprocess
#---------------------------------------

# 2.1 rename the columns

names(df) <- c("Date", "Hour", "In", "Out", "Value")

# 2.2 remove the off hours and holidays

df$Weekday <- wday(df$Date, label = T)
Workday <- df %>% 
    filter(!Weekday %in% c("週六", "週日")) %>%
    filter(!Hour %in% c("02", "03", "04"))
remove(df)

# 2.3 aggregate the data by in-stations and out-stations

Workday_i <- Workday %>% 
    group_by(Date, Hour, In) %>% 
    summarise_at("Value", list(Value =~ sum(.)))

Workday_o <- Workday %>% 
    group_by(Date, Hour, Out) %>% 
    summarise_at("Value", list(Value =~ sum(.)))

remove(Workday)

Workday_io <- tibble(Date = Workday_i$Date, 
                     Hour = Workday_i$Hour,
                     Stations = Workday_i$In,
                     Value_i = Workday_i$Value,
                     Value_o = Workday_o$Value)

remove(list = c("Workday_i", "Workday_o"))

# 2.4 aggregate again

Workday_io_m <- Workday_io %>%
    group_by(Hour, Stations) %>%
    summarise_at(c("Value_i", "Value_o"), list(mean =~ mean(.)))

remove(Workday_io)

# 2.5 plot the result

timePlot <- function(loc){
    temp_io <- Workday_io_m %>% filter(Stations == loc)
    ggplot() +
        # in
        geom_point(data = temp_io, aes(x = Hour, y = Value_i_mean), color = "blue") +
        geom_line(data = temp_io, aes(x = Hour, y = Value_i_mean, group = Stations), color = "blue") +
        # out
        geom_point(data = temp_io, aes(x = Hour, y = Value_o_mean), color = "red") +
        geom_line(data = temp_io, aes(x = Hour, y = Value_o_mean, group = Stations), color = "red") +
        # axes and title
        ggtitle(loc) + xlab("Hour") + ylab("Value")
}

stations <- unique(Workday_io_m$Stations)
stickers <- stations
stickers[stations == "台北101/世貿"] <- "台北101_世貿"

for(i in 1:length(stations)){
    g <- timePlot(stations[i])
    if(!file.exists(yy)) dir.create(yy)
    ggsave(filename = paste(stickers[i], "_", yy, ".png", sep = ""), 
           plot = g, width = 20, height = 10, units = "cm",
           path = paste(yy, "/", sep = ""))
}


#---------------------------------------
# 3. data transformation
#---------------------------------------

Workday_io_512 <- Workday_io_m %>%
    filter(Hour %in% c("05", "06", "07", "08", "09", "10", "11", "12"))

remove(Workday_io_m)

result <- Workday_io_512 %>%
    group_by(Stations) %>%
    summarise_at(c("Value_i_mean", "Value_o_mean"),
                 list(index1 =~ sum(Value_i_mean),
                      index2 =~ max(Value_i_mean)/max(Value_o_mean),
                      index3 =~ sum(Value_i_mean)/sum(Value_o_mean))) %>%
    select(Stations, 
           index1 = Value_i_mean_index1,
           index2 = Value_i_mean_index2,
           index3 = Value_i_mean_index3)

remove(Workday_io_512)

write_delim(x = result, delim = " ",
            file = paste("result_", yy, ".txt", sep = ""))

