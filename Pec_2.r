
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
  mutate(freq = 100 * (n / sum(n))) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "50%", height = "225px")



titanic %>%
  group_by(Survived, Age) %>%
  filter(Survived == 1) %>%
  summarise (n = n()) %>%
  mutate(freq = 100 * (n / sum(n))) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "50%", height = "350px")


titanic %>%
  mutate(Decade = cut(
    Age,
    breaks = seq(0, 90, by = 10),
    right = TRUE
  )) -> titanic


titanic %>%
  mutate(Survived = cut(
    Survived,
    breaks = c(0, 1),
    labels = c("No", "Yes"),
    right = TRUE
  )) -> titanic

titanic %>%
  group_by(Survived, Decade) %>%
  filter(Survived == 1) %>%
  summarise (n = n()) %>%
  mutate(freq = 100 * (n / sum(n))) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "50%", height = "350px")

library(ggpubr)
theme_set(theme_pubr())
titanic %>%
  group_by(Survived, Decade) %>%
  filter(Survived == 1) %>%
  summarise(n = n()) %>%
  mutate(freq = 100 * (n / sum(n))) %>%
  ggplot(
    aes(x = Decade, y = n, fill=factor(Decade))
  ) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) + 
  geom_text(aes(label = n), vjust = -0.3) + 
  theme_pubclean() 
  


titanic %>%
  mutate(
    Survived =
      forcats::as_factor(
        as.character(Survived)
      )
  ) -> t

cols <- c('Survived')

library(purrrlyr)
t[,cols] <- 
  titanic %>% 
  select(one_of(cols)) %>% 
  dmap(as.factor)

