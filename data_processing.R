library(tidymodels)
library(tidyverse)

load("data/offers_censored.RData")
source("./helpful_functions.R")

final_training_prediction_split <-
  offers |>
  make_appyear_split(test_year = 2023)

final_training <- training(final_training_prediction_split)

analysis_assessment_split <-
  offers |>
  drop_post_prediction_offers()|>
  filter(AppYear <= 2022) |>
  make_appyear_split(test_year = 2022)

analysis_train <- training(analysis_assessment_split)
assessment_test <- testing(analysis_assessment_split)
