library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(magrittr)

getwd()
setwd("C:/Users/DELL/OneDrive/Desktop/R Programming Files/practice_r")
list.files()

titanic_dts <- read_delim("titanicdataset.csv", delim = ",")

titanic <- titanic_dts %>%
  select(Survived, Pclass, Sex, Age, Fare, Name) %>% 
  rename(survived = Survived,
         pclass = Pclass,
         sex = Sex,
         age = Age,
         fare = Fare,
         name = Name)

summary(titanic)
sum(is.na(titanic$age))
titanic <- titanic %>%
  filter(!is.na(age))

titanic <- titanic %>% 
  mutate(age_group = cut(age,
                         breaks = c(0, 20, 40, 60, Inf),
                         labels = c("Child", "Teen", "Adult", "Senior"),
                         include.lowest = TRUE))
  
titanic %>% 
  summarise(mean_age = mean(age),
            max_fare = max(fare),
            min_fare = min(fare))

# Survival percentage by sex
titanic %>% 
  group_by(sex) %>% 
  summarise(survived_rate = mean(survived))

# Survival percentage by pclass
titanic %>% 
  group_by(pclass) %>% 
  summarise(survived_rate = mean(survived))


  
# Bar chart: Survival rate based on sex
titanic %>% 
  group_by(sex) %>% 
  summarise(survived_rate = mean(survived)) %>% 
  ggplot(mapping = aes(x = sex, y = survived_rate, fill = sex)) +
  geom_col() +
  labs(x = "Sex", y = "Survival Rate", fill = "Sex") +
  theme_minimal()

# Boxplot: Fare based on pclass
titanic %>% 
  ggplot(mapping = aes(x = fare, y = factor(pclass))) +
  geom_boxplot() +
  labs(x = "Fare", y = "Passenger Class") +
  ggtitle("Fare Distribution by Class",
          subtitle = "Source for Data: GitHub") +
  theme_minimal()
  
# Histogram: Age Distribution
ggplot(titanic, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "red") +
  labs(x = "Age", y = "Count", title = "Age Distribution of Titanic Passengers") +
  theme_minimal()

# Point Chart: Survival Rate based on Age
ggplot(titanic, aes(x = factor(survived), y = age)) +
  geom_point(method = "glm", color = "darkred", alpha = 0.4) + 
  geom_smooth(level = 0.95)






  