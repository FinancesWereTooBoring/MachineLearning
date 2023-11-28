library(tidymodels)
library(tidyverse)
load("data/offers_censored.RData")

head(offers)

offers$Program %>% table()
