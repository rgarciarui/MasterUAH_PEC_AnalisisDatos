
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

b %>% 
  group_by(District) %>%
  summarise_all(funs(count = count(!is.na(.))))



filtro1 <- b1 %>%
            group_by(District) %>%
            summarise(ciclistas = sum(N)) %>%
            arrange(desc(ciclistas)) %>%
            slice (1:5) %>%
            select(-ciclistas)

dat2 <- b1 %>%
  semi_join(filtro1, by = c("District")) %>%
  arrange(District)

b %>%
  na.omit() %>%
  group_by(District) %>%
  summarise(personas = mean(N)) %>%
  replace_na() %>%
  head()

b %>% 
  group_by(District) %>% 
  mutate(N = ifelse(is.na(N), mean(N, na.rm = TRUE), N))

b1 %>%
  separate(Date, c("Day", "Month", "Year"), sep = "/") -> b1


