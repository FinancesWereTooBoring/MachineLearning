library(tidymodels)
library(tidyverse)
library(ggplot2)
library(dplyr)

load("data/offers_censored.RData")

apps_year <- offers %>%
  group_by(AppYear) %>%
  summarise(num_enrolled = sum(Status == "Enrolled"),
            num_notenroll = sum(Status == "Not enrolled")) %>%
  filter(AppYear != 2023)

# Number of enrollment statuses in each year
ggplot(apps_year, aes(x = as.factor(AppYear))) +
  geom_bar(aes(y = num_enrolled, fill = "Enrolled"), stat = "identity") +
  geom_bar(aes(y = num_notenroll, fill = "Not Enrolled"), stat = "identity") +
  labs(title = "Enrollment Status by Year",
       x = "Year",
       y = "Number of Offers",
       fill = "Enrollment Status") +
  scale_fill_manual(values = c("Enrolled" = "cornflowerblue", "Not Enrolled" = "pink3")) +
  theme_minimal() # proportion would be nice here

# Distribution of enrollment statuses
ggplot(offers, aes(x = Status)) +
  geom_bar() +
  labs(title = "Distribution of Enrollment Status",
       x = "Status",
       y = "Count")# maybe a per year view would be clearer

# Number of applications per year
ggplot(offers, aes(x = as.factor(AppYear))) +
  geom_bar() +
  labs(title = "Distribution of Applications by Year",
       x = "Application Year",
       y = "Count")

app4_3 <- offers %>%
  filter(App4 == 3)


ggplot(data = offers)+
  geom_bar(aes(x = App4, fill = Status), position = "fill")
  

str(offers)
