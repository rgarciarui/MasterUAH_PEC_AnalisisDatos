
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
  mutate(Title = ifelse(!Title %in% c('Mr.', 'Mrs.', 'Master.', 'Miss.'), 'Otro', Title)) -> titanic
  
kable(titanic) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")

titanic %>%
  select(Title, Age) %>%
  drop_na(Age) %>%
  ggplot(aes(x=Title, y=Age, fill=Title)) +
  geom_boxplot()


titanic %>%
  select(Age, Pclass, Survived) %>%
  drop_na(Age) %>%
  ggplot(aes(x = Pclass, y = Age, colour = factor(Survived))) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x") +
  facet_grid(Pclass ~ .)

titanic %>%
  select(Age, Title, Survived) %>%
  drop_na(Age) %>%
  ggplot(aes(x = Title, y = Age, colour = factor(Survived))) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x") +
  facet_grid(Title ~ .)

titanic %>%
  select(Pclass, Title, Survived) %>%
  ggplot(aes(x = Title, y = Pclass, colour = factor(Survived))) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x") +
  facet_grid(Title ~ .)


titanic %>%
  group_by(Pclass, Title) %>%
  mutate(
    Age = ifelse(
      is.na(Age),
      mean(Age, na.rm = TRUE),
      Age
    )
  ) -> titanic


kable(titanic) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "300px")


titanic %>%
  filter_all(any_vars(is.na(.))) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")


titanic %>%
  drop_na() -> titanic

titanic %>%
  group_by(Survived, Sex) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "50%", height = "225px")












