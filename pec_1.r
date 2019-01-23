
library(tidyverse)
library(knitr)
bikes <- read_csv("bikes2016.csv")
str(bikes)

bikes <- dplyr::select(bikes, -Timestamp)

str(bikes)

bikes %>% 
  summarise_all(funs(100*mean(is.na(.))))

bikes <- bikes %>%
  gather(key = District, value = N, -Date) 

bikes %>%
  na.omit() %>%
  group_by(District) %>%
  summarise(personas = sum(N))

bikes %>%
  group_by(District) %>%
  mutate(N = ifelse(is.na(N), mean(N, na.rm = TRUE), N)) -> bikes

bikes %>%
  separate(Date, c("Day", "Month", "Year"), sep = "/") -> bikes

bikes %>%
  group_by(District) %>%
  summarise(ciclistas = sum(N)) %>%
  arrange(desc(ciclistas)) %>%
  slice(1:5)

filtro1 <- bikes %>%
  group_by(District) %>%
  summarise(ciclistas = sum(N)) %>%
  arrange(desc(ciclistas)) %>%
  slice(1:5) %>%
  select(-ciclistas)

bikes %>%
  semi_join(filtro1, by = c("District")) %>%
  arrange(District) %>%
  ggplot(aes(x = Day, y = N)) +
  geom_point() + geom_smooth() + coord_flip() +
  facet_wrap(District~.)

bikes %>%
  semi_join(filtro1, by = c("District")) %>%
  arrange(District) %>%
  ggplot(aes(x = Day, y = N)) +
  geom_point() + geom_smooth() + 
  facet_grid(District~.)


bikes %>%
  filter(District %in% c('Berri1', 'University', 'Boyer', 'Parc')) %>%
  filter(str_detect(Month, '01')) %>% kable

bikes %>%
  filter(District %in% c("Berri1", "University", "Boyer", "Parc")) %>%
  filter(str_detect(Month, "01")) %>%
  ggplot(aes(x = Day, y = N, group = District)) +
  geom_line(aes(color = District)) +
  geom_point(aes(color = District)) +
  labs(
    title = " Evolucion diaria del numero de ciclistas",
    x = "Dias (enero)",
    y = "Numero ciclistas"
  )



