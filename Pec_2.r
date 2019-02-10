
library(tidyverse)
library(knitr)
library("kableExtra")
library(forcats)

library(purrr)
library(dplyr)
library(magrittr)
library(ggpubr)

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
  mutate(Name2 = Name) %>% 
  separate(Name2, c("apellido", "Nombre"), sep = ",") %>%
  separate(Nombre, c("none", "Title"), sep = " ") %>% 
  select(-none, -apellido) %>%
  mutate(Title = ifelse(Title == 'Mlle.', 'Miss.', Title)) %>%
  mutate(Title = ifelse(Title == 'Ms.|Mme.', 'Mrs.', Title)) %>%
  mutate(Title = ifelse(!Title %in% c('Mr.', 'Mrs.', 'Master.', 'Miss.'), 'Otro', Title)) -> titanic

titanic %>%
  select(PassengerId, Survived, Pclass, Title, everything()) -> titanic

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
  mutate(Survived = recode(Survived, 
                    "0" = "No", 
                    "1" = "Yes")) -> titanic
titanic$Survived %<>% factor

titanic %>%
  ggplot(aes(x = Survived, fill = Survived)) +
  geom_bar() +
  guides(fill = FALSE) +
  theme_pubclean()


titanic %>%
  group_by(Survived, Title) %>%
  filter(Survived == "Yes") %>%
  summarise (n = n()) %>%
  mutate(freq = 100 * (n / sum(n))) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "50%", height = "350px")

titanic %>%
  group_by(Survived, Title) %>%
  filter(Survived == "Yes") %>%
  summarise(n = n()) %>%
  mutate(freq = 100 * (n / sum(n))) %>%
  ggplot(
    aes(x = Title, y = n, fill=factor(Title))
  ) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) + 
  geom_text(aes(label = n), vjust = -0.3) + 
  theme_pubclean()


titanic %>%
  group_by(Title, Survived) %>%
  #filter(Survived == "Yes") %>%
  summarise (n = n()) %>%
  mutate(freq = 100 * (n / sum(n))) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "50%", height = "350px")

titanic %>%
  group_by(Survived, Title) %>%
  #filter(Survived == "Yes") %>%
  summarise (n = n()) %>%
  mutate(freq = 100 * (n / sum(n))) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "50%", height = "350px")

titanic %>%
  select(Survived, Title) %>%
  ggplot(aes(x = Title, y = Survived, colour = factor(Survived))) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x") +
  facet_grid(Title ~ .)


titanic %>%
  group_by(as.numeric(as.character(PassengerId))) %>%
  gather('Key', 'value', starts_with('x')) %>%
  summarise( Key.Sum = sum(value) ) %>%
  left_join( titanic, . )



titanic %>%
  mutate(
    Familysize = (
      as.numeric(as.character(SibSp)) +
        as.numeric(as.character(Parch)) + 1
    )
  ) -> titanic

titanic %>%
  mutate(
    Sigleton = (
      ifelse(Familysize > 1, FALSE, TRUE)
    )
  ) -> titanic


titanic %>%
  ggplot() +
  geom_point(aes(x = Age, y = Fare, color = Survived)) +
  ggtitle("Gráfica: Diagrama de Dispersión Age / Fare") +
  theme_pubclean()

titanic %>%
  ggplot(aes(Age)) +
  geom_histogram(
    breaks = seq(20, 50, by = 2),
    col = "red",
    alpha = .5,
    aes(fill = ..count..)
  ) +
  labs(
    title = "Histograma de la variable Age",
    x = "Age",
    y = "Count"
  ) +
  scale_fill_gradient(
    "Count",
    low = "green",
    high = "red"
  ) +
  theme_pubclean() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

titanic %>%
  select(Survived, Title) %>%
  ggplot(aes(x = Title, y = Survived, colour = factor(Survived))) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x") +
  facet_grid(Title ~ .)



titanic %>%
  group_by(Survived, Pclass, Sex) %>%
  filter(Survived == "Yes") %>%
  summarise(n = n()) %>%
  ggplot(
    aes(x = Sex, y = n, fill = factor(Pclass))
  ) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red3", "lightskyblue3", "lightgreen")) +
  geom_text(aes(label = n),
    position = position_stack(), vjust = 1.1
  ) + # Para los valores en la columna
  labs(title = "Pasajeros sobrevivientes, segun Sexo y Clase", y = NULL) + # Etiquetas del gráfico
  theme_minimal() +
  theme(legend.position = "top")
  


titanic %>%
  select(Pclass, Familysize) %>%
  group_by(Pclass, Familysize) %>%
  summarize() -> t

t %>%
  ggplot(
    aes(x = Pclass, y = Familysize, fill = Familysize)
  ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Familysize),
    position = position_stack(),
    vjust = 1.1,
    color = "white"
  ) +
  labs(
    title = "Pasajeros por tamaño de familia y clase",
    y = NULL
  ) +
  theme(legend.position = "top")


titanic %>%
  group_by(Pclass, Title, Sex) %>%
  ungroup() %>%
  select(Sex) %>%
  ggplot(aes(x = Sex, y = ..count.., fill = Sex)) + 
  geom_bar(stat = "count")
  geom_histogram() 
  

titanic %>%
  group_by(Pclass, Title, Sex, Age) %>%
  ungroup() %>%
  select(Sex, Age) %>%
  ggplot(., aes(Age, fill = Sex)) + 
  geom_histogram(alpha = 0.5, 
                 aes(y = ..count..), 
                 position = 'identity',
                 binwidth = 5
                 ) +
  labs(
    title = "Histograma de valores de Edad por sexo",
    y = NULL
  ) +
  theme(legend.position = "top")
    
titanic %>%
  group_by(Pclass, Title, Sex, Age) %>%
  ungroup() %>%
  select(Sex, Age) %>%
  ggplot(., aes(Age, fill = Sex)) + 
  geom_histogram(alpha = 0.5, 
                 aes(y = ..density..), 
                 position = 'identity',
                 binwidth = 5
  ) +
  labs(
    title = "Histograma de densidad de Edad por sexo",
    y = NULL
  ) +
  theme(legend.position = "top")
  
library(DT)  
titanic2 <- read_delim("titanic2.csv", ";", 
                       escape_double = FALSE, locale = locale(), 
                       trim_ws = TRUE)

datatable(
  summary(titanic2),
  filter = "top",
  options = list(
    pageLength = 5,
    autoWidth = TRUE
  ),
  colnames = c('Variable', 'Type')
)
  

titanic %>% 
  group_by(Name) %>% 
  filter(n()>1)

titanic2 %>% 
  group_by(name) %>% 
  filter(n()>1)


titanic <- left_join(
  x = titanic,
  y = titanic2,
  by = c("Name" = "name", "Ticket" = "ticket")
)

titanic %>%
  group_by(Pclass, Title, Survived, boat) %>%
  ungroup() %>%
  select(Survived, boat) %>%
  filter(Survived == "Yes") %>%
  mutate(Boat = ifelse(is.na(boat), 0, 1)) %>%
  select(-boat) %>%
  summarise(Freq = 100 * (sum(Boat) / n())) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "25%", height = "100px")




titanic %>%
  ungroup() %>%
  sample_n(
    round(nrow(.) * 0.3),
    replace = FALSE
  ) -> titanic30

titanic70 <- anti_join(
  titanic,
  titanic30,
  by = "PassengerId"
)



datatable(
  titanic70,
  filter = "top",
  options = list(
    pageLength = 5,
    autoWidth = TRUE
  )
)