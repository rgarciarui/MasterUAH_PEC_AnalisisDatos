
library(tidyverse)
bikes <- read_csv("bikes2016.csv")
str(bikes)

bikes <- bikes[, -2]

map(bikes, ~mean(is.na(.)))
bikes %>% map(~ mean(is.na(.)))

bikes %>% 
  summarise_all(funs(100*mean(is.na(.))))

bikes <- dplyr::select(bikes, -Timestamp)

bikes <- bikes %>%
  gather(key = District, value = N, -Date)
