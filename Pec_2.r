
library(tidyverse)
library(knitr)
library("kableExtra")

titanic <- read_csv("titanic.csv")

str(titanic)

kable(cbind(titanic, titanic), caption = "Datos titanic.csv") %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")


titanic %>% 
  select(Survived) %>% 
  summarise(Sobrevivientes = 100*(sum(Survived)/n())) 

titanic %>% 
  summarise_all(funs(100*mean(is.na(.)))) %>% kable() %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")

titanic <- dplyr::select(titanic, -Cabin)

str(titanic)

titanic %>%
  separate(Name, c("apellido", "Nombre"), sep = ",") %>%
  separate(Nombre, c("none", "Title"), sep = " ") %>% 
  select(-none, -apellido) %>%
  mutate(Title = ifelse(Title == 'Mlle.', 'Miss.', Title)) %>%
  mutate(Title = ifelse(Title == 'Ms.|Mme.', 'Mrs.', Title)) %>%
  mutate(Title = ifelse(!Title %in% c('Mr.', 'Mrs.', 'Master.', 'Miss.'), 'Otro', Title)) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")


