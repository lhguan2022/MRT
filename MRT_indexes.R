#---------------------------------------
# 0. packages
#---------------------------------------

# 0.1 packages

library(readr)
library(tidyverse)

# 0.2 working directory

setwd("~/R/MRT")

#---------------------------------------
# 1. load the results
#---------------------------------------

result_2017 <- read_csv("result_2017.csv")
result_2018 <- read_csv("result_2018.csv")
result_2019 <- read_csv("result_2019.csv")
result_2020 <- read_csv("result_2020.csv")
result_2021 <- read_csv("result_2021.csv")
result_2022 <- read_csv("result_2022.csv")

#---------------------------------------
# 2. compute the three indexes
#---------------------------------------

index_2017 <- result_2017 %>%
    mutate(Stations,
           index1 = Value_i_mean_sum,
           index2 = Value_i_mean_max / Value_o_mean_max,
           index3 = Value_i_mean_sum / Value_o_mean_sum,
           index4 = Value_i_mean_sum / (Value_i_mean_sum + Value_o_mean_sum)) %>%
    select(Stations, index1, index2, index3, index4)

index_2018 <- result_2018 %>%
    mutate(Stations,
           index1 = Value_i_mean_sum,
           index2 = Value_i_mean_max / Value_o_mean_max,
           index3 = Value_i_mean_sum / Value_o_mean_sum,
           index4 = Value_i_mean_sum / (Value_i_mean_sum + Value_o_mean_sum)) %>%
    select(Stations, index1, index2, index3, index4)

index_2019 <- result_2019 %>%
    mutate(Stations,
           index1 = Value_i_mean_sum,
           index2 = Value_i_mean_max / Value_o_mean_max,
           index3 = Value_i_mean_sum / Value_o_mean_sum,
           index4 = Value_i_mean_sum / (Value_i_mean_sum + Value_o_mean_sum)) %>%
    select(Stations, index1, index2, index3, index4)

index_2020 <- result_2020 %>%
    mutate(Stations,
           index1 = Value_i_mean_sum,
           index2 = Value_i_mean_max / Value_o_mean_max,
           index3 = Value_i_mean_sum / Value_o_mean_sum,
           index4 = Value_i_mean_sum / (Value_i_mean_sum + Value_o_mean_sum)) %>%
    select(Stations, index1, index2, index3, index4)

index_2021 <- result_2021 %>%
    mutate(Stations,
           index1 = Value_i_mean_sum,
           index2 = Value_i_mean_max / Value_o_mean_max,
           index3 = Value_i_mean_sum / Value_o_mean_sum,
           index4 = Value_i_mean_sum / (Value_i_mean_sum + Value_o_mean_sum)) %>%
    select(Stations, index1, index2, index3, index4)

index_2022 <- result_2022 %>%
    mutate(Stations,
           index1 = Value_i_mean_sum,
           index2 = Value_i_mean_max / Value_o_mean_max,
           index3 = Value_i_mean_sum / Value_o_mean_sum,
           index4 = Value_i_mean_sum / (Value_i_mean_sum + Value_o_mean_sum)) %>%
    select(Stations, index1, index2, index3, index4)

write_csv(x = index_2017, file = "index_2017.csv")
write_csv(x = index_2018, file = "index_2018.csv")
write_csv(x = index_2019, file = "index_2019.csv")
write_csv(x = index_2020, file = "index_2020.csv")
write_csv(x = index_2021, file = "index_2021.csv")
write_csv(x = index_2022, file = "index_2022.csv")



